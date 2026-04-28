get_endpoint_fn <- function(pr, path, verb) {
    eps <- pr$endpoints[["__no-preempt__"]]
    for (ep in eps) {
        if (identical(ep$path, path) && verb %in% ep$verbs) {
            return(ep$getFunc())
        }
    }
    stop("Endpoint not found: ", verb, " ", path, call. = FALSE)
}

with_temp_home <- function(code) {
    old_home <- Sys.getenv("HOME")
    home_dir <- tempfile("openeocraft-home-")
    dir.create(home_dir, recursive = TRUE)
    Sys.setenv(HOME = home_dir)
    on.exit({
        Sys.setenv(HOME = old_home)
        unlink(home_dir, recursive = TRUE, force = TRUE)
    }, add = TRUE)
    force(code)
}

create_files_context <- function() {
    testthat::skip_if_not_installed("openstac")
    candidates <- c(
        "docker/plumber.R",
        "../docker/plumber.R",
        "../../docker/plumber.R"
    )
    plumber_file <- candidates[file.exists(candidates)][1]
    if (is.na(plumber_file)) {
        testthat::skip("docker/plumber.R not found from current working directory")
    }
    pr <- plumber::plumb(plumber_file)
    basic <- paste(
        "Basic",
        base64enc::base64encode(charToRaw("brian:123456"))
    )
    auth_req <- mock_req(
        "/credentials/basic",
        method = "GET",
        HTTP_AUTHORIZATION = basic
    )
    auth_req$args <- list()
    auth_res <- mock_res()
    auth_fn <- get_endpoint_fn(pr, "/credentials/basic", "GET")
    token_doc <- auth_fn(auth_req, auth_res)
    token <- token_doc$access_token
    stopifnot(is.character(token), nzchar(token))

    list(
        api = get("api", envir = environment(auth_fn)),
        list_files = get_endpoint_fn(pr, "/files", "GET"),
        get_file = get_endpoint_fn(pr, "/files/<path:path>", "GET"),
        put_file = get_endpoint_fn(pr, "/files/<path:path>", "PUT"),
        delete_file = get_endpoint_fn(pr, "/files/<path:path>", "DELETE"),
        token = token
    )
}

make_bearer_req <- function(path, method, token, args = list(), post_body = NULL) {
    req <- mock_req(
        path,
        method = method,
        HTTP_AUTHORIZATION = paste("Bearer", token)
    )
    req$args <- args
    if (!is.null(post_body)) {
        req$postBody <- post_body
    }
    req
}

test_that("workspace file routes support upload and download", {
    with_temp_home({
        ctx <- create_files_context()

        put_req <- make_bearer_req(
            "/files/folder/hello.txt",
            method = "PUT",
            token = ctx$token,
            post_body = charToRaw("hello workspace")
        )
        put_res <- mock_res()
        put_doc <- ctx$put_file(put_req, put_res, path = "folder/hello.txt")
        expect_equal(put_doc$path, "folder/hello.txt")
        expect_true(put_doc$size > 0)
        expect_true(is.character(put_doc$modified))

        get_req <- make_bearer_req(
            "/files/folder/hello.txt",
            method = "GET",
            token = ctx$token
        )
        get_res <- mock_res()
        ctx$get_file(get_req, get_res, path = "folder/hello.txt")
        expect_equal(rawToChar(get_res$body), "hello workspace")
    })
})

test_that("GET /files supports optional pagination links", {
    with_temp_home({
        ctx <- create_files_context()

        req1 <- make_bearer_req(
            "/files/a.txt",
            method = "PUT",
            token = ctx$token,
            post_body = charToRaw("a")
        )
        ctx$put_file(req1, mock_res(), path = "a.txt")

        req2 <- make_bearer_req(
            "/files/b.txt",
            method = "PUT",
            token = ctx$token,
            post_body = charToRaw("b")
        )
        ctx$put_file(req2, mock_res(), path = "b.txt")

        list_req_1 <- make_bearer_req(
            "/files",
            method = "GET",
            token = ctx$token,
            args = list(limit = "1", page = "1")
        )
        list_res_1 <- mock_res()
        page1 <- ctx$list_files(list_req_1, list_res_1)

        expect_equal(length(page1$files), 1)
        rels1 <- vapply(page1$links, `[[`, "", "rel")
        expect_true("next" %in% rels1)

        list_req_2 <- make_bearer_req(
            "/files",
            method = "GET",
            token = ctx$token,
            args = list(limit = "1", page = "2")
        )
        list_res_2 <- mock_res()
        page2 <- ctx$list_files(list_req_2, list_res_2)

        expect_equal(length(page2$files), 1)
        rels2 <- vapply(page2$links, `[[`, "", "rel")
        expect_true("prev" %in% rels2)
    })
})

test_that("DELETE /files removes files and prunes empty parent folders", {
    with_temp_home({
        ctx <- create_files_context()

        put_req <- make_bearer_req(
            "/files/models/rf/model.rds",
            method = "PUT",
            token = ctx$token,
            post_body = charToRaw("model-bytes")
        )
        ctx$put_file(put_req, mock_res(), path = "models/rf/model.rds")

        delete_req <- make_bearer_req(
            "/files/models/rf/model.rds",
            method = "DELETE",
            token = ctx$token
        )
        delete_res <- mock_res()
        ctx$delete_file(delete_req, delete_res, path = "models/rf/model.rds")
        expect_equal(delete_res$status, 204L)

        root <- file.path(api_user_workspace(ctx$api, "brian"), "root")
        expect_false(file.exists(file.path(root, "models/rf/model.rds")))
        expect_false(dir.exists(file.path(root, "models/rf")))
    })
})

test_that("workspace file routes reject invalid auth and invalid paths", {
    with_temp_home({
        ctx <- create_files_context()

        no_auth_req <- mock_req("/files", method = "GET")
        no_auth_req$args <- list()
        no_auth_res <- mock_res()
        expect_error(
            ctx$list_files(no_auth_req, no_auth_res),
            "Token is missing"
        )

        invalid_path_req <- make_bearer_req(
            "/files/%2E%2E/secrets.txt",
            method = "PUT",
            token = ctx$token,
            post_body = charToRaw("x")
        )
        invalid_path_res <- mock_res()
        expect_error(
            ctx$put_file(invalid_path_req, invalid_path_res, path = "../secrets.txt"),
            "Invalid path"
        )

        put_req <- make_bearer_req(
            "/files/folder/item.txt",
            method = "PUT",
            token = ctx$token,
            post_body = charToRaw("content")
        )
        ctx$put_file(put_req, mock_res(), path = "folder/item.txt")

        folder_get_req <- make_bearer_req(
            "/files/folder",
            method = "GET",
            token = ctx$token
        )
        folder_get_res <- mock_res()
        expect_error(
            ctx$get_file(folder_get_req, folder_get_res, path = "folder"),
            "FileOperationUnsupported"
        )
    })
})
