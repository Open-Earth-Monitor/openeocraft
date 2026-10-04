#' @export
api_credential.openeo_v1 <- function(api, req, res) {
    auth <- gsub("Basic ", "", req$HTTP_AUTHORIZATION)
    auth <- rawToChar(base64enc::base64decode(auth))
    auth <- strsplit(auth, ":")[[1]]
    user <- auth[[1]]
    password <- auth[[2]]
    file <- api_attr(api, "credentials")
    credentials <- readRDS(file)
    if (!user %in% names(credentials$users) ||
        credentials$users[[user]]$password != password) {
        api_stop(403L, "User or password does not match")
    }
    # user is logged
    if (!"token" %in% names(credentials$users[[user]])) {
        credentials <- new_token(credentials, user, 30)
        saveRDS(credentials, file)
    } else {
        token <- credentials$users[[user]]$token
        # Renew access token when the stored expiry has passed
        if (credentials$tokens[[token]]$expiry < Sys.time()) {
            old_token <- credentials$users[[user]]$token
            credentials$tokens[[old_token]] <- NULL
            credentials <- new_token(credentials, user, 30)
            saveRDS(credentials, file)
        }
    }
    list(access_token = credentials$users[[user]]$token)
}
#' @export
api_wellknown.openeo_v1 <- function(api, req, res) {
    host <- get_host(api, req)
    doc <- update_wellknown_version(
        doc = list(),
        api_version = api$api_version,
        url = get_link(host, "/"),
        production = api$production
    )
    for (x in get_wellknown_versions(api)) {
        doc <- update_wellknown_version(
            doc = doc,
            api_version = x$api_version,
            url = x$url,
            production = x$production
        )
    }
    doc
}
#' @export
api_landing_page.openeo_v1 <- function(api, req, res) {
    doc <- list(
        type = "Catalog",
        id = api$id,
        title = api$title,
        description = api$description,
        backend_version = api$backend_version,
        stac_version = if (is.null(api$stac_api)) {
            "1.0.0"
        } else if (is.character(api$stac_api)) {
            api$stac_api[[1]]
        } else {
            api$stac_api$get("stac_version")
        },
        api_version = api$api_version,
        production = api$production,
        endpoints = get_endpoints(api),
        conformsTo = api$conforms_to,
        billing = list(
            currency = "EUR",
            default_plan = "free",
            plans = list(
                list(
                    name = "free",
                    description = "Free plan with no monetary charges"
                )
            )
        )
    )
    # Optional rels (terms-of-service, privacy-policy, create-form,
    # recovery-form) need product content; tracked in DEVELOPMENT.md.

    doc <- link_root(doc, api, req)
    doc <- link_self(doc, api, req, "application/json")
    doc <- link_spec(doc, api, req)
    doc <- link_docs(doc, api, req)
    host <- get_host(api, req)
    doc <- update_link(
        doc = doc,
        rel = "conformance",
        href = make_url(host, "/conformance"),
        type = "application/json"
    )
    doc <- update_link(
        doc = doc,
        rel = "data",
        href = make_url(host, "/collections"),
        type = "application/json"
    )
    doc <- update_link(
        doc = doc,
        rel = "version-history",
        href = make_url(host, "/.well-known/openeo"),
        type = "application/json"
    )
    doc
}
#' @export
api_conformance.openeo_v1 <- function(api, req, res) {
    doc <- list(conformsTo = api$conforms_to)
    doc
}
#' @export
api_processes.openeo_v1 <- function(api, req, res, check_auth = FALSE) {
    if (check_auth) {
        token <- req$header$token
        get_token_user(api, token)
    }
    procs <- unname(api_attr(api, "processes"))
    host <- get_host(api, req)
    doc <- list(
        processes = procs,
        links = list()
    )
    doc <- update_link(
        doc,
        rel = "self",
        href = make_url(host, "/processes"),
        type = "application/json"
    )
    page <- paginate_resource_list(
        items = procs,
        doc = doc,
        api = api,
        req = req,
        endpoint = "/processes",
        limit = parse_pagination_limit(req),
        page = parse_pagination_page(req)
    )
    page$doc$processes <- page$items
    page$doc
}
#' @rdname api_handling
#' @export
api_result.openeo_v1 <- function(api, req, res) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    pg <- req$body
    if (is.null(pg)) {
        api_stop(400L, "Missing process graph", id = "ProcessGraphMissing")
    }
    # Accept either a process graph wrapper or a job-like body with `process`
    process <- pg
    if (is.list(pg) && "process" %in% names(pg) && !is_pgraph(pg)) {
        process <- pg$process
    }
    if (!is_pgraph(process)) {
        api_stop(400L, "Invalid process graph", id = "ProcessGraphInvalid")
    }
    assert_payment_allowed(pg$plan %||% "Free")

    job_id <- job_sync_id()
    job <- list(
        id = job_id,
        title = "syncronous job",
        description = "syncronous job",
        process = process,
        status = "created",
        created = Sys.time(),
        plan = pg$plan %||% "Free",
        budget = pg$budget %||% 0.0,
        log_level = pg$log_level %||% "Info",
        links = list()
    )
    job_new_dir(api, user, job)
    job_crt_rds(api, user, job)
    job_sync(api, req, user, job_id)

    finished <- job_get(api, user, job_id)
    if (identical(finished$status, .job_status_error)) {
        logs <- logs_read_rds(api, user, job_id)
        msg <- if (length(logs)) {
            logs[[length(logs)]]$message
        } else {
            "Synchronous job failed"
        }
        api_stop(400L, msg, id = "JobError")
    }

    job_dir <- job_get_dir(api, user, job_id)
    result_files <- list.files(job_dir, pattern = "^[^_]", full.names = TRUE)
    res$status <- 200L
    if (length(result_files) == 1) {
        result <- structure(
            list(data = result_files),
            class = paste0("openeo_", ext_format(result_files))
        )
        return(data_serializer(result, res))
    }

    tar_file <- file.path(job_dir, "_files.tar")
    result_basenames <- basename(result_files)
    old_wd <- getwd()
    setwd(job_dir)
    tryCatch(
        utils::tar("_files.tar", files = result_basenames),
        finally = setwd(old_wd)
    )
    result <- structure(list(data = tar_file), class = "openeo_tar")
    data_serializer(result, res)
}
#' @export
api_jobs_list.openeo_v1 <- function(api, req, res) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    jobs <- job_read_rds(api, user)
    host <- get_host(api, req)
    job_items <- unname(lapply(jobs, \(job) {
        job[c("id", "title", "status", "created")]
    }))
    doc <- list(
        jobs = job_items,
        links = list()
    )
    doc <- update_link(
        doc,
        rel = "self",
        href = make_url(host, "/jobs"),
        type = "application/json"
    )
    page <- paginate_resource_list(
        items = job_items,
        doc = doc,
        api = api,
        req = req,
        endpoint = "/jobs",
        limit = parse_pagination_limit(req),
        page = parse_pagination_page(req)
    )
    page$doc$jobs <- page$items
    page$doc
}
#' @export
api_job_info.openeo_v1 <- function(api, req, res, job_id) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    jobs <- job_read_rds(api, user)
    # Check if the job_id exists in the jobs_list
    if (!(job_id %in% names(jobs))) {
        api_stop(404L, "Job not found")
    }
    # Retrieve the job from the jobs_list
    job <- jobs[[job_id]]
    reconciled <- job_reconcile_bg_process(api, user, job_id)
    if (!is.null(reconciled)) {
        job <- reconciled
    }
    res$status <- 200L
    job_populate_links(job, api, req)
}
#' @export
api_job_delete.openeo_v1 <- function(api, req, res, job_id) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    job_delete(api, user, job_id)
    res$status <- 204L
    list()
}
#' @export
api_job_create.openeo_v1 <- function(api, req, res) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    if (is.null(req$body)) {
        api_stop(400L, "Missing job information")
    }
    job_info <- job_check(req$body, partial = FALSE)
    job_id <- random_id(16L)
    job <- list(
        id = job_id,
        title = job_info$title,
        description = job_info$description,
        process = job_info$process,
        status = "created",
        created = Sys.time(),
        plan = job_info$plan,
        budget = job_info$budget,
        log_level = job_info$log_level,
        links = list()
    )
    # Directory first, then atomic jobs.rds index write (see atomic_save_rds).
    # Cross-process locking is deferred; see DEVELOPMENT.md.
    job_new_dir(api, user, job)
    jobs <- job_read_rds(api, user)
    job_save_rds(api, user, job, jobs)
    # Set HTTP headers
    host <- get_host(api, req)
    res$setHeader("Location", make_url(host, "/jobs/", job_id))
    res$setHeader("OpenEO-Identifier", job_id)
    res$status <- 201L
    list()
}
#' @export
api_job_start.openeo_v1 <- function(api, req, res, job_id) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    jobs <- job_read_rds(api, user)
    if (!(job_id %in% names(jobs))) {
        api_stop(404L, "Job not found")
    }
    assert_payment_allowed(jobs[[job_id]]$plan %||% "Free")
    if (identical(jobs[[job_id]]$status, .job_status_finished)) {
        res$status <- 202L
        return(list(
            id = job_id,
            message = "Job already finished",
            code = 200L
        ))
    }
    procs <- procs_read_rds(api)
    if (!is.null(procs[[job_id]])) {
        alive <- tryCatch(
            procs[[job_id]]$is_alive(),
            error = function(e) FALSE
        )
        if (alive) {
            res$status <- 202L
            return(list(
                id = job_id,
                message = "Job already started",
                code = 200L
            ))
        }
    }

    proc <- job_async(api, req, user, job_id)

    procs[[job_id]] <- proc
    procs_save_rds(api, procs)
    res$status <- 202L
    list()
}
#' @export
api_job_cancel_results.openeo_v1 <- function(api, req, res, job_id) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    job_cancel_results(api, user, job_id)
    res$status <- 202L
    list()
}
#' @export
api_me.openeo_v1 <- function(api, req, res) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    list(
        user_id = user,
        name = user,
        default_plan = "free"
    )
}
#' @export
api_credentials_oidc.openeo_v1 <- function(api, req, res) {
    # Discovery stub: no identity providers configured yet.
    # Full OIDC login remains deferred (see DEVELOPMENT.md).
    list(providers = list())
}
#' @export
api_file_formats.openeo_v1 <- function(api, req, res) {
    doc <- file_formats()
    token <- get_token(req)
    if (length(token)) {
        doc <- file_formats_auth(doc, api, token)
    }
    doc
}
#' @export
api_process_graphs_list.openeo_v1 <- function(api, req, res) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    graphs <- process_graphs_read(api, user)
    processes <- unname(lapply(graphs, process_graph_metadata))
    doc <- list(processes = processes, links = list())
    host <- get_host(api, req)
    doc <- update_link(
        doc,
        rel = "self",
        href = make_url(host, "/process_graphs"),
        type = "application/json"
    )
    doc
}
#' @export
api_process_graph_get.openeo_v1 <- function(api, req, res, process_graph_id) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    process_graph_get(api, user, process_graph_id)
}
#' @export
api_process_graph_put.openeo_v1 <- function(api, req, res, process_graph_id) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    result <- process_graph_put(api, user, process_graph_id, req$body)
    host <- get_host(api, req)
    res$setHeader(
        "Location",
        make_url(host, "/process_graphs/", process_graph_id)
    )
    res$setHeader("OpenEO-Identifier", process_graph_id)
    res$status <- if (result$created) 201L else 200L
    list()
}
#' @export
api_process_graph_delete.openeo_v1 <- function(api, req, res, process_graph_id) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    process_graph_delete(api, user, process_graph_id)
    res$status <- 204L
    list()
}
#' @export
api_ml_runtimes.openeo_v1 <- function(api, req, res) {
    # Runtimes are the same for every user, so the endpoint never looks at
    # the Authorization header (L3-ML requirement ML2).
    get_ml_runtimes(api)
}
#' @export
api_ml_models.openeo_v1 <- function(api, req, res) {
    user <- get_optional_token_user(api, req)
    models <- unname(lapply(
        ml_models_list(api, user),
        ml_model_doc,
        api = api,
        req = req
    ))
    host <- get_host(api, req)
    doc <- list(models = models, links = list())
    doc <- update_link(
        doc,
        rel = "self",
        href = make_url(host, "/ml_models"),
        type = "application/json"
    )
    page <- paginate_resource_list(
        items = models,
        doc = doc,
        api = api,
        req = req,
        endpoint = "/ml_models",
        limit = parse_pagination_limit(req),
        page = parse_pagination_page(req)
    )
    page$doc$models <- page$items
    page$doc
}
#' @export
api_ml_model.openeo_v1 <- function(api, req, res, model_id) {
    if (!is_string(model_id) ||
        !grepl(.ml_model_id_pattern, model_id, perl = TRUE)) {
        api_stop(
            400L,
            "Invalid model id '", model_id, "'. ",
            "It must match the pattern ", .ml_model_id_pattern,
            id = "ModelIdInvalid"
        )
    }
    user <- get_optional_token_user(api, req)
    record <- ml_model_get(api, user, model_id)
    if (is.null(record)) {
        api_stop(
            404L,
            "Model '", model_id, "' not found",
            id = "ModelNotFound"
        )
    }
    ml_model_doc(record, api, req)
}
