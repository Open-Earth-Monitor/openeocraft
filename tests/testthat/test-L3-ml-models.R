# Functionality Description (proposed L3-ML API profile)
# GET /ml_models:
# - ML10: Lists the models stored on the back-end with their STAC MLM
#   metadata
# GET /ml_models/{id}:
# - ML11: Returns the full STAC MLM Item of one model; the identifier can be
#   passed to load_ml_model

mock_token <- "Bearer basic//5aad11e1d49b880a4468e1b252944e22"
mock_user <- "mock_user"
mlm_extension <- "https://stac-extensions.github.io/mlm/v1.5.0/schema.json"

new_models_api <- function(env = parent.frame()) {
    api <- mock_create_openeo_v1()
    api$work_dir <- withr::local_tempdir(.local_envir = env)
    api_attr(api, "api_base_url") <- "https://openeo.example"
    api
}

mlm_item <- function(id, href, framework = "R CARET",
                     extensions = list(mlm_extension)) {
    list(
        type = "Feature",
        stac_version = "1.1.0",
        stac_extensions = extensions,
        id = id,
        bbox = list(-63, -10, -62, -9),
        geometry = NULL,
        properties = list(
            datetime = "2024-01-01T00:00:00Z",
            `mlm:name` = id,
            `mlm:framework` = framework,
            `mlm:architecture` = "Random Forest",
            `mlm:tasks` = list("classification")
        ),
        assets = stats::setNames(list(list(
            href = href,
            type = "application/octet-stream",
            roles = list("mlm:model"),
            `mlm:artifact_type` = "saveRDS"
        )), paste0(id, ".rds")),
        links = list(
            list(href = "collection.json", rel = "collection"),
            list(href = paste0(id, ".json"), rel = "self"),
            list(href = "https://example.org/paper", rel = "cite-as")
        )
    )
}

# Same layout as save_ml_model: job-local model + Item, and a public copy.
write_saved_model <- function(api, user, job_id, id, public = TRUE,
                              item = mlm_item(id, paste0(
                                  "https://openeo.example/files/jobs/",
                                  job_id, "/models/", id, ".rds?token=x"
                              ))) {
    job_dir <- file.path(api$work_dir, "workspace", user, "jobs", job_id)
    dir.create(file.path(job_dir, "models"), recursive = TRUE,
               showWarnings = FALSE)
    saveRDS(list(id = id, job = job_id), file.path(job_dir, "models",
                                                    paste0(id, ".rds")))
    jsonlite::write_json(item, file.path(job_dir, paste0(id, ".json")),
                         auto_unbox = TRUE)
    if (public) {
        write_public_model(api, id)
    }
    invisible(job_dir)
}

write_public_model <- function(api, id, item = mlm_item(id, paste0(id, ".rds"))) {
    dir <- file.path(api$work_dir, "workspace", "public", "models")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(id = id, job = "public"), file.path(dir, paste0(id, ".rds")))
    jsonlite::write_json(item, file.path(dir, paste0(id, ".json")),
                         auto_unbox = TRUE)
    invisible(dir)
}

list_models <- function(api, ..., args = list()) {
    req <- mock_req("/ml_models", method = "GET", args = args, ...)
    api_ml_models(api, req, mock_res())
}

get_model <- function(api, model_id, ...) {
    req <- mock_req("/ml_models", model_id, method = "GET", ...)
    api_ml_model(api, req, mock_res(), model_id)
}

model_ids <- function(doc) {
    vapply(doc$models, `[[`, character(1), "id")
}

link_href <- function(doc, rel) {
    for (link in doc$links) {
        if (identical(link$rel, rel)) return(link$href)
    }
    NULL
}

test_that("GET /ml_models: Empty list when no model is stored", {
    api <- new_models_api()
    doc <- list_models(api)
    expect_identical(doc$models, list())
    expect_identical(link_href(doc, "self"), "https://openeo.example/ml_models")
    expect_identical(
        as.character(jsonlite::toJSON(doc$models, auto_unbox = TRUE)),
        "[]"
    )
})

test_that("GET /ml_models: Lists public models without authentication", {
    api <- new_models_api()
    write_saved_model(api, mock_user, "job-a", "rf_public")
    write_saved_model(api, mock_user, "job-b", "rf_private", public = FALSE)

    doc <- list_models(api)

    expect_identical(model_ids(doc), "rf_public")
    item <- doc$models[[1]]
    expect_identical(item$type, "Feature")
    expect_true(mlm_extension %in% unlist(item$stac_extensions))
    expect_identical(item$properties$`mlm:framework`, "R CARET")
    # Relative public href becomes an absolute, token-free URL.
    expect_identical(
        item$assets$rf_public.rds$href,
        "https://openeo.example/files/public/models/rf_public.rds"
    )
})

test_that("GET /ml_models: Adds the user's own models when authenticated", {
    api <- new_models_api()
    write_saved_model(api, mock_user, "job-a", "rf_public")
    write_saved_model(api, mock_user, "job-b", "rf_private", public = FALSE)
    write_saved_model(api, "someone_else", "job-c", "theirs", public = FALSE)

    doc <- list_models(api, HTTP_AUTHORIZATION = mock_token)

    expect_setequal(model_ids(doc), c("rf_public", "rf_private"))
    # The owner sees the job-local copy, which load_ml_model resolves first.
    own <- doc$models[[which(model_ids(doc) == "rf_public")]]
    expect_match(own$assets$rf_public.rds$href, "/files/jobs/job-a/models/")
})

test_that("GET /ml_models: Invalid token is rejected", {
    api <- new_models_api()
    expect_error(
        list_models(api, HTTP_AUTHORIZATION = "Bearer bogus"),
        "Invalid token"
    )
})

test_that("GET /ml_models: Same id resolves like load_ml_model", {
    api <- new_models_api()
    # Alphabetical job order: job-a wins over job-b, user wins over public.
    write_saved_model(api, mock_user, "job-b", "dup", public = FALSE,
                      item = mlm_item("dup", "b.rds", framework = "B"))
    write_saved_model(api, mock_user, "job-a", "dup", public = FALSE,
                      item = mlm_item("dup", "a.rds", framework = "A"))
    write_public_model(api, "dup",
                       item = mlm_item("dup", "dup.rds", framework = "P"))

    doc <- list_models(api, HTTP_AUTHORIZATION = mock_token)
    expect_identical(model_ids(doc), "dup")
    expect_identical(doc$models[[1]]$properties$`mlm:framework`, "A")

    anonymous <- list_models(api)
    expect_identical(anonymous$models[[1]]$properties$`mlm:framework`, "P")
})

test_that("GET /ml_models: Skips models without a valid STAC MLM Item", {
    api <- new_models_api()
    write_saved_model(api, mock_user, "job-a", "good")
    write_public_model(api, "no_mlm",
                       item = mlm_item("no_mlm", "no_mlm.rds", extensions = list()))
    dir <- file.path(api$work_dir, "workspace", "public", "models")
    saveRDS(list(), file.path(dir, "no_item.rds"))
    saveRDS(list(), file.path(dir, "broken.rds"))
    writeLines("{ not json", file.path(dir, "broken.json"))
    # The parent collection written by save_ml_model is not a model.
    jsonlite::write_json(list(type = "Collection", id = "ml-models"),
                         file.path(dir, "collection.json"), auto_unbox = TRUE)

    expect_identical(model_ids(list_models(api)), "good")
})

test_that("GET /ml_models: Supports limit/page pagination", {
    api <- new_models_api()
    for (id in c("m1", "m2", "m3")) write_public_model(api, id)

    page1 <- list_models(api, args = list(limit = "2"))
    expect_identical(model_ids(page1), c("m1", "m2"))
    expect_match(link_href(page1, "next"), "/ml_models\\?limit=2&page=2$")

    page2 <- list_models(api, args = list(limit = "2", page = "2"))
    expect_identical(model_ids(page2), "m3")
    expect_null(link_href(page2, "next"))

    expect_error(list_models(api, args = list(limit = "0")), "limit")
})

test_that("GET /ml_models/{id}: Returns the full STAC MLM Item", {
    api <- new_models_api()
    write_saved_model(api, mock_user, "job-a", "rf_public")

    item <- get_model(api, "rf_public")

    expect_identical(item$id, "rf_public")
    expect_identical(item$type, "Feature")
    expect_identical(item$properties$`mlm:architecture`, "Random Forest")
    expect_identical(item$bbox, list(-63L, -10L, -62L, -9L))
    expect_true("rf_public.rds" %in% names(item$assets))
    expect_identical(
        link_href(item, "self"),
        "https://openeo.example/ml_models/rf_public"
    )
    expect_identical(link_href(item, "parent"), "https://openeo.example/ml_models")
    expect_identical(link_href(item, "root"), "https://openeo.example/")
    # Relative storage links are dropped; absolute links are kept.
    expect_null(link_href(item, "collection"))
    expect_identical(link_href(item, "cite-as"), "https://example.org/paper")
    # Missing geometry is serialized as JSON null, as STAC requires.
    json <- jsonlite::toJSON(item, auto_unbox = TRUE)
    expect_match(as.character(json), '"geometry":null', fixed = TRUE)
})

test_that("GET /ml_models/{id}: Private models need the owner's token", {
    api <- new_models_api()
    write_saved_model(api, mock_user, "job-a", "rf_private", public = FALSE)

    err <- tryCatch(get_model(api, "rf_private"), error = identity)
    expect_equal(err$status, 404L)
    expect_identical(err$id, "ModelNotFound")

    item <- get_model(api, "rf_private", HTTP_AUTHORIZATION = mock_token)
    expect_identical(item$id, "rf_private")
})

test_that("GET /ml_models/{id}: Invalid ids are rejected", {
    api <- new_models_api()
    for (bad in c("../secret", "a b", "")) {
        err <- tryCatch(get_model(api, bad), error = identity)
        expect_equal(err$status, 400L)
        expect_identical(err$id, "ModelIdInvalid")
    }
})

test_that("GET /ml_models/{id}: Error handler formats openEO errors", {
    api <- new_models_api()
    err <- tryCatch(get_model(api, "missing"), error = identity)
    res <- mock_res()
    out <- api_error_handler(mock_req("/ml_models/missing"), res, err)
    expect_identical(out$id, "ModelNotFound")
    expect_equal(out$code, 404L)
    expect_match(out$message, "Model 'missing' not found")
})

test_that("GET /ml_models: Listed ids load with load_ml_model (end-to-end)", {
    skip_on_cran()
    skip_if_not_installed("sits")
    f <- system.file("ml/processes.R", package = "openeocraft")
    skip_if(f == "", "inst/ml/processes.R not found")

    api <- new_models_api()
    setup_namespace(api)
    ns <- get_namespace(api)
    eval(parse(f, encoding = "UTF-8"), envir = ns)
    run_as <- function(user, job_id, expr) {
        env <- create_env(api, user, list(id = job_id, status = "running"),
                          mock_req("/", method = "GET"))
        suppressMessages(eval(expr, envir = env, enclos = ns))
    }

    model <- structure(list(weights = 1:3), mlm_task = "classification",
                       mlm_architecture = "Random Forest",
                       mlm_framework = "R CARET",
                       mlm_bands = c("NDVI", "EVI"),
                       mlm_labels = c("forest", "water"))
    saved <- run_as(mock_user, "job-train", bquote(
        save_ml_model(data = .(model), name = "rf_e2e")
    ))
    expect_true(saved)

    owner <- list_models(api, HTTP_AUTHORIZATION = mock_token)
    expect_identical(model_ids(owner), "rf_e2e")
    item <- get_model(api, "rf_e2e", HTTP_AUTHORIZATION = mock_token)
    expect_identical(item$properties$`mlm:framework`, "R CARET")
    expect_identical(item$properties$`mlm:architecture`, "Random Forest")

    # Public listing: id is loadable by another user via the public fallback.
    expect_identical(model_ids(list_models(api)), "rf_e2e")
    loaded <- run_as("another_user", "job-predict", quote(
        load_ml_model(id = "rf_e2e")
    ))
    expect_identical(loaded$weights, 1:3)

    # Owner still gets the job-local copy first (unchanged behaviour).
    own <- run_as(mock_user, "job-predict", quote(load_ml_model(id = "rf_e2e")))
    expect_identical(own$weights, 1:3)

    # Unknown ids still fail with 404.
    err <- tryCatch(
        run_as("another_user", "job-x", quote(load_ml_model(id = "nope"))),
        error = identity
    )
    expect_equal(err$status, 404)
})
