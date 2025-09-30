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
        # TODO: check token renewal
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
    # TODO: support to billing key
    doc <- list(
        type = "Catalog",
        id = api$id,
        title = api$title,
        description = api$description,
        backend_version = api$backend_version,
        stac_version = api$stac_api$get("stac_version"),
        api_version = api$api_version,
        production = api$production,
        endpoints = get_endpoints(api),
        conformsTo = api$conforms_to
    )
    # TODO:
    # It is highly RECOMMENDED to provide links with the following
    # rel (relation) types:
    #   - version-history: A link back to the Well-Known URL
    #     (including /.well-known/openeo, see the corresponding endpoint
    #     for details) to allow clients to work on the most recent version.
    #   - terms-of-service: A link to the terms of service. If a back-end
    #     provides a link to the terms of service, the clients MUST
    #     provide a way to read the terms of service and only connect to
    #     the back-end after the user agreed to them. The user interface
    #     MUST be designed in a way that the terms of service are not
    #     agreed to by default, i.e. the user MUST explicitly agree to them.
    #   - privacy-policy: A link to the privacy policy (GDPR). If a
    #     back-end provides a link to a privacy policy, the clients MUST
    #     provide a way to read the privacy policy and only connect to
    #     the back-end after the user agreed to them. The user
    #     interface MUST be designed in a way that the privacy policy
    #     is not agreed to by default, i.e. the user MUST explicitly
    #     agree to them.
    #   - service-desc or service-doc: A link to the API definition.
    #     Use service-desc for machine-readable API definition and
    #     service-doc for human-readable API definition. Required if
    #     full OGC API compatibility is desired.
    #   - conformance: A link to the Conformance declaration
    #     (see /conformance). Required if full OGC API compatibility
    #     is desired.
    #   - data: A link to the collections (see /collections). Required
    #     if full OGC API compatibility is desired.
    #   - create-form: A link to a user registration page.
    #   - recovery-form: A link to a page where a user can recover a
    #     user account (e.g. to reset the password or send a reminder
    #     about the username to the user's email account).

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
    procs <- api_attr(api, "processes")
    procs <- list(
        processes = unname(procs)
    )
    procs
}
#' @rdname api_handling
#' @export
api_result.openeo_v1 <- function(api, req, res) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    pg <- req$body

    # TODO: create job_check
    # job_prepare(api, user, job)
    # - fill defaults
    # - check consistency of the provided fields
    # - also check plan
    job_id <- job_sync_id()
    job <- list(
        id = job_id,
        title = "syncronous job",
        description = "syncronous job",
        process = pg,
        status = "created",
        created = Sys.time(),
        plan = "Free",
        budget = 0.0,
        log_level = "Info",
        links = list()
    )
    # TODO: create directory and job RDS file as an atomic transaction
    # create job's directory
    job_new_dir(api, user, job)
    # TODO: how to avoid concurrency issues on reading/writing?
    # use some specific package? e.g. filelock, sqllite?, mongodb?
    job_crt_rds(api, user, job)
    job_sync(api, req, user, job_id)

    # TODO: Test to see if the results are being returned correctly
    job_dir <- job_get_dir(api, user, job_id)
    result_files <- list.files(job_dir, pattern = "^[^_]", full.names = TRUE)
    if (length(result_files) == 1) {
        result <- structure(
            list(data = result_files),
            class = paste0("openeo_", ext_format(result_files))
        )
        return(data_serializer(result, res))
    }

    tar_file <- file.path(job_dir, "_files.tar")

    # TODO: remove directory structure from the tar file
    utils::tar(tar_file, result_files)
    result <- structure(list(data = tar_file), class = "openeo_tar")
    data_serializer(result, res)
}
#' @export
api_jobs_list.openeo_v1 <- function(api, req, res) {
    token <- get_token(req)
    user <- get_token_user(api, token)
    jobs <- job_read_rds(api, user)
    jobs <- list(
        jobs = unname(lapply(jobs, \(job) {
            job[c("id", "title", "status", "created")]
        })),
        # TODO: populate this link with some function like we do
        #   in other endpoints
        links = list()
    )
    jobs
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
    res$status <- 200L
    # TODO: populate links?
    job
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
    # TODO: create job_check
    # job_prepare(api, user, job)
    # - fill defaults
    # - check consistency of the provided fields
    # - also check plan

    token <- get_token(req)
    user <- get_token_user(api, token)
    if (is.null(req$body)) {
        api_stop(400L, "Missing job information")
    }
    job_info <- req$body
    # TODO: create job_check
    if (!"process" %in% names(job_info)) {
        api_stop(400L, "Invalid job information")
    }
    # check job --> job_info_check(job_info)
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
    # TODO: create directory and job RDS file as an atomic transaction
    # create job's directory
    job_new_dir(api, user, job)
    # TODO: how to avoid concurrency issues on reading/writing?
    # use some specific package? e.g. filelock, sqllite?, mongodb?
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
    # Check if the job_id exists in the jobs_list
    if (!(job_id %in% names(jobs))) {
        api_stop(404L, "Job not found")
    }
    # TODO: get process from job_id
    procs <- procs_read_rds(api)
    if (!is.null(procs[[job_id]])) {
        # TODO: check if there is another message to finished state!
        if (procs[[job_id]]$is_alive() ||
            jobs[[job_id]]$status == "finished") {
            return(list(
                id = job_id,
                message = "Job already started",
                code = 200L
            ))
        }
    }

    # TODO: implement queue: check for maximum number of workers
    # procs_alive(procs) -> manage process not alive
    # define in the api how many workers to start?
    #  --> wait if length(procs) >= workers (per user?)
    proc <- job_async(api, req, user, job_id)

    procs[[job_id]] <- proc
    procs_save_rds(api, procs)
    res$status <- 202L
    list()
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
