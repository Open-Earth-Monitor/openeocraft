# api_user_workspace(api, user) -> work_dir / <user>
# work_dir / <user> / jobs.rds
# work_dir / <user> / <job_id> /
# work_dir / <user> / files /

.job_status_error <- "error"
.job_status_finished <- "finished"

job_sync_id <- function() {
    format(Sys.time(), "job-%Y%m%d")
}
#' Atomically write an RDS object (temp file + rename)
#'
#' @param object R object to serialize.
#' @param file Destination path.
#' @param fail_msg Message passed to [api_stop()] on failure.
#' @return `NULL`, invisibly.
#' @keywords internal
atomic_save_rds <- function(object, file, fail_msg = "Could not save RDS file") {
    dir <- dirname(file)
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
    }
    tmp <- tempfile(pattern = "atomic_", tmpdir = dir, fileext = ".rds")
    ok <- FALSE
    on.exit({
        if (!ok && file.exists(tmp)) {
            unlink(tmp)
        }
    }, add = TRUE)
    tryCatch(
        {
            saveRDS(object, tmp)
            if (!file.rename(tmp, file)) {
                if (!file.copy(tmp, file, overwrite = TRUE)) {
                    api_stop(500L, fail_msg)
                }
                unlink(tmp)
            }
            ok <- TRUE
        },
        error = function(e) {
            api_stop(500L, fail_msg)
        }
    )
    invisible(NULL)
}

# list of named lists, each containing job details
job_read_rds <- function(api, user) {
    file <- file.path(api_user_workspace(api, user), "jobs.rds")
    if (!file.exists(file)) {
        return(list())
    }
    readRDS(file)
}
job_save_rds <- function(api, user, job, jobs) {
    jobs[[job$id]] <- job
    file <- file.path(api_user_workspace(api, user), "jobs.rds")
    atomic_save_rds(jobs, file, "Could not save the jobs file")
    invisible(NULL)
}
job_crt_rds <- function(api, user, job) {
    jobs <- job_read_rds(api, user)
    job_save_rds(api, user, job, jobs)
}
job_upd_status <- function(api, user, job_id, status) {
    jobs <- job_read_rds(api, user)
    if (!job_id %in% names(jobs)) {
        api_stop(500, "Could not find sync job id")
    }
    job <- jobs[[job_id]]
    if (job$status != status) {
        job$status <- status
        job_save_rds(api, user, job, jobs)
    }
    job
}

job_get <- function(api, user, job_id) {
    jobs <- job_read_rds(api, user)
    if (!job_id %in% names(jobs)) {
        api_stop(500, "Could not find sync job id")
    }
    job <- jobs[[job_id]]
    job
}
job_delete_rds <- function(api, user, job, jobs) {
    if (!job$id %in% names(jobs)) {
        return(invisible(NULL))
    }
    jobs[[job$id]] <- NULL
    file <- file.path(api_user_workspace(api, user), "jobs.rds")
    atomic_save_rds(jobs, file, "Could not save the jobs index file")
}
#' Manage job artefacts and metadata
#'
#' Helpers to manage stored jobs, update their status, query logs and results,
#' and compute derived URLs and documents.
#'
#' @param api An openeocraft API object.
#'
#' @param user The user identifier associated with the job.
#'
#' @param job_id Identifier of the job to operate on.
#'
#' @param req A plumber request object representing the incoming call.
#'
#' @param job A named list describing a job payload.
#'
#' @param offset Zero-based offset when paginating job logs.
#'
#' @param level Minimum log level to include, one of `"error"`, `"warning"`,
#'   `"info"`, or `"debug"`.
#'
#' @param limit Maximum number of log records to return.
#'
#' @param partial If `TRUE`, return a placeholder collection for unfinished jobs.
#'
#' @return
#'   * `job_get_dir()` returns the path backing the job workspace.
#'   * `job_sync()` returns `NULL` invisibly after updating job status.
#'   * `job_info()` returns a list containing the stored job metadata.
#'   * `job_update()` returns a confirmation list with a status message.
#'   * `job_delete()` returns `NULL` invisibly after removing artefacts.
#'   * `job_estimate()` returns a placeholder list describing cost estimates.
#'   * `job_logs()` returns a list with the filtered log entries.
#'   * `job_get_results()` returns either a STAC collection or a placeholder
#'     document when results are pending.
#'   * `job_empty_collection()` returns a minimal STAC collection describing the
#'     job in its current state.
#'
#' @name job_helpers
#'
#' @examples
#' \donttest{
#' api <- create_openeo_v1(
#'     id = "demo", title = "Demo", description = "Demo",
#'     backend_version = "0.4.1", stac_api = NULL,
#'     work_dir = tempdir(), production = FALSE
#' )
#' job_empty_collection(api, "alice", list(id = "j1", status = "created"))
#' }
NULL
#' @rdname job_helpers
#' @export
job_get_dir <- function(api, user, job_id) {
    file.path(api_user_workspace(api, user), "jobs", job_id)
}
job_new_dir <- function(api, user, job) {
    job_dir <- job_get_dir(api, user, job$id)
    if (dir.exists(job_dir)) {
        unlink(job_dir, recursive = TRUE)
        if (dir.exists(job_dir)) {
            api_stop(500, "Could not delete the job ", job$id, "'s folder")
        }
    }
    dir.create(job_dir, recursive = TRUE)
    if (!dir.exists(job_dir)) {
        api_stop(500, "Could not create the job ", job$id, "'s folder")
    }
}
job_del_dir <- function(api, user, job_id) {
    job_dir <- job_get_dir(api, user, job_id)
    unlink(job_dir, recursive = TRUE)
    if (dir.exists(job_dir)) {
        api_stop(500, "Could not delete the job ", job_id, "'s folder")
    }
}

procs_read_rds <- function(api) {
    procs <- api_attr(api, "bg_procs")
    if (is.null(procs)) {
        return(list())
    }
    procs
}

procs_save_rds <- function(api, procs) {
    api_attr(api, "bg_procs") <- procs
    invisible(NULL)
}
logs_read_rds <- function(api, user, job_id) {
    file <- file.path(api_user_workspace(api, user), "jobs", job_id, "logs.rds")
    if (!file.exists(file)) {
        return(list())
    }
    logs <- readRDS(file)
    logs
}
logs_save_rds <- function(api, user, job_id, logs) {
    file <- file.path(api_user_workspace(api, user), "jobs", job_id, "logs.rds")
    tryCatch(
        atomic_save_rds(logs, file),
        error = function(e) NULL
    )
    invisible(NULL)
}

# Optional openEO log fields (`path`, `usage`) via `...` are deferred; see
# DEVELOPMENT.md roadmap. Required args stay before `...`.
log_append <- function(api, user, job_id, code, level, message, ...) {
    logs <- logs_read_rds(api, user, job_id)
    msg <- as.character(message)[[1]]
    msg_short <- strsplit(msg, "\n", fixed = TRUE)[[1]][[1]]
    if (nchar(msg_short) > 500L) {
        msg_short <- paste0(substr(msg_short, 1L, 497L), "...")
    }
    entry_id <- paste0(job_id, "-", length(logs) + 1L)
    logs[[length(logs) + 1]] <- list(
        id = entry_id,
        code = code,
        level = level,
        message = msg_short,
        time = Sys.time(), ...
    )
    logs_save_rds(api, user, job_id, logs)
}

#' @rdname job_helpers
#' @export
job_sync <- function(api, req, user, job_id) {
    # Ensure thread caps even when the package was loaded without Docker/plumber
    # calling configure_openeocraft_runtime() (`.onLoad` stays quiet for CRAN).
    configure_openeocraft_runtime()
    job <- job_upd_status(api, user, job_id, "running")
    tryCatch(
        {
            run_pgraph(api, req, user, job, job$process)
            job_upd_status(api, user, job_id, "finished")
        },
        error = function(e) {
            code <- 100
            if ("code" %in% names(e)) {
                code <- e$code
            }

            # Concise client log + optional call detail via `data`
            call_str <- tryCatch(
                paste(deparse(conditionCall(e)), collapse = " "),
                error = function(err) NULL
            )
            detail <- list()
            if (!is.null(call_str) && nzchar(call_str) && call_str != "NULL") {
                detail$call <- call_str
            }
            job_upd_status(api, user, job_id, .job_status_error)
            if (length(detail)) {
                log_append(
                    api, user, job_id, code, "error", e$message,
                    data = detail
                )
            } else {
                log_append(api, user, job_id, code, "error", e$message)
            }
            invisible(NULL)
        }
    )
}
job_async <- function(api, req, user, job_id) {
    job_dir <- job_get_dir(api, user, job_id)
    # Update job status
    job_upd_status(api, user, job_id, "running")
    cmdargs <- c("--slave", "--no-save", "--no-restore")
    proc <- suppressMessages(callr::r_bg(
        func = function(user, job_id) {
            api <- utils::getFromNamespace(
                "openeocraft_worker_api", "openeocraft"
            )()
            openeocraft::job_sync(
                api, req = list(), user = user, job_id = job_id
            )
        },
        args = list(user, job_id),
        cmdargs = cmdargs,
        stdout = file.path(job_dir, "_stdout.log"),
        stderr = file.path(job_dir, "_stderr.log"),
        poll_connection = FALSE,
        supervise = TRUE
    ))
    proc
}

#' Mark running jobs as error when the callr worker has died.
#'
#' @keywords internal
job_reconcile_bg_process <- function(api, user, job_id) {
    jobs <- job_read_rds(api, user)
    if (!(job_id %in% names(jobs))) {
        return(NULL)
    }
    job <- jobs[[job_id]]
    if (!(job$status %in% c("running", "created"))) {
        return(job)
    }
    procs <- procs_read_rds(api)
    proc <- procs[[job_id]]
    worker_dead <- is.null(proc)
    if (!worker_dead) {
        worker_dead <- !tryCatch(proc$is_alive(), error = function(e) FALSE)
    }
    if (!worker_dead) {
        return(job)
    }
    created <- tryCatch(
        as.POSIXct(job$created, tz = "UTC"),
        error = function(e) NA
    )
    if (!is.na(created)) {
        age_sec <- as.numeric(difftime(Sys.time(), created, units = "secs"))
        if (age_sec < 15) {
            return(job)
        }
    }
    exit_code <- tryCatch(proc$get_exit_code(), error = function(e) NA_integer_)
    log_path <- file.path(job_get_dir(api, user, job_id), "_stderr.log")
    tail_err <- character(0)
    if (file.exists(log_path)) {
        lines <- readLines(log_path, warn = FALSE)
        lines <- lines[nzchar(lines)]
        if (length(lines)) {
            tail_err <- tail(lines, 3L)
        }
    }
    msg <- paste(
        c(
            sprintf(
                "Background worker exited unexpectedly (exit code %s).",
                exit_code
            ),
            tail_err
        ),
        collapse = "\n"
    )
    log_append(api, user, job_id, 100L, "error", msg)
    job_upd_status(api, user, job_id, .job_status_error)
    procs[[job_id]] <- NULL
    procs_save_rds(api, procs)
    jobs <- job_read_rds(api, user)
    jobs[[job_id]]
}
#' @rdname job_helpers
#' @export
job_info <- function(api, user, job_id) {
    jobs <- job_read_rds(api, user)

    # Check if the job_id exists in the jobs_list
    if (!(job_id %in% names(jobs))) {
        api_stop(404L, "Job not found")
    }

    # Retrieve the job from the jobs_list
    job <- jobs[[job_id]]

    # Return all metadata for the job
    job
}

#' @rdname job_helpers
#' @export
job_update <- function(api, user, job_id, job) {
    job_check(job, partial = TRUE)

    jobs <- job_read_rds(api, user)

    # Check if the job_id exists in the jobs_list
    if (!(job_id %in% names(jobs))) {
        api_stop(404L, "Job not found")
    }

    # Retrieve the job from the jobs_list
    current_job <- jobs[[job_id]]

    # Update job details based on the provided body
    job <- utils::modifyList(current_job, job)

    job$status <- "updated"
    job$updated <- Sys.time()

    # Update the job in the jobs_list
    job_save_rds(api, user, job, jobs)

    list(id = job_id, message = "Job updated", code = 200L)
}


#' @rdname job_helpers
#' @export
job_delete <- function(api, user, job_id) {
    jobs <- job_read_rds(api, user)
    # Check if the job_id exists in the jobs_list
    if (!(job_id %in% names(jobs))) {
        api_stop(404L, "Job not found")
    }
    removed_job <- jobs[[job_id]]
    job_delete_rds(api, user, removed_job, jobs)
    # Delete the folder associated with the job_id
    job_del_dir(api, user, job_id)
}
#' @rdname job_helpers
#' @export
job_estimate <- function(api, user, job_id) {
    # This would likely call a function to calculate cost or
    # duration based on job details calculate_estimate(job_id)
    # not implemented as depends on the cloud provider the
    # software is running on
    list(
        message = paste0(
            "The cost estimates will depends on the cloud ",
            "provider the software is running on"
        )
    )
}

#' @rdname job_helpers
#' @export
job_logs <- function(api,
                     user,
                     job_id,
                     offset = NULL,
                     level = "info",
                     limit = 10) {
    level_list <- c("error", "warning", "info", "debug")
    if (!level %in% level_list) {
        api_stop(
            400L, "level must be one of ",
            paste0("'", level_list, "'", collapse = ", ")
        )
    }
    limit <- as.integer(limit)
    if (is.na(limit) || limit < 1) {
        api_stop(400L, "limit parameter must be >= 1")
    }
    logs <- logs_read_rds(api, user, job_id)
    # Normalize entries for openEO (unique id, short message, level, time)
    logs <- lapply(seq_along(logs), function(i) {
        log <- logs[[i]]
        msg <- as.character(log$message %||% "")
        # Keep client-facing message concise (no full stack / process dump)
        msg_short <- strsplit(msg, "\n", fixed = TRUE)[[1]][[1]]
        if (nchar(msg_short) > 500L) {
            msg_short <- paste0(substr(msg_short, 1L, 497L), "...")
        }
        entry <- list(
            id = log$id %||% paste0(job_id, "-", i),
            level = log$level %||% "info",
            message = msg_short,
            time = log$time %||% Sys.time(),
            code = log$code
        )
        # openEO log entries may carry a `data` field (e.g. error call detail)
        if (!is.null(log$data)) {
            entry$data <- log$data
        }
        entry
    })
    levels <- vapply(logs, \(log) log$level, character(1))
    selection <- match(levels, level_list) <= match(level, level_list)
    logs <- logs[selection]

    # offset: return entries after the given log id (openEO pagination)
    if (!is.null(offset) && !(is.character(offset) && !nzchar(offset))) {
        offset <- as.character(offset)
        ids <- vapply(logs, \(log) as.character(log$id), character(1))
        idx <- match(offset, ids)
        if (!is.na(idx)) {
            logs <- if (idx < length(logs)) logs[(idx + 1L):length(logs)] else list()
        } else {
            # Numeric offset: skip first N entries
            off_n <- suppressWarnings(as.integer(offset))
            if (!is.na(off_n) && off_n >= 0L) {
                if (off_n >= length(logs)) {
                    logs <- list()
                } else if (off_n > 0L) {
                    logs <- logs[(off_n + 1L):length(logs)]
                }
            }
        }
    }
    if (length(logs) > limit) {
        logs <- logs[seq_len(limit)]
    }
    list(level = level, logs = unname(logs), links = list())
}

#' @rdname job_helpers
#' @export
job_get_results <- function(api, user, job_id, partial = FALSE, req = NULL) {
    jobs <- job_read_rds(api, user)
    if (!(job_id %in% names(jobs))) {
        api_stop(404L, "Job not found")
    }
    job <- jobs[[job_id]]
    if (job$status == .job_status_error) {
        api_stop(424L, "Job returned an error", id = "JobError")
    }
    results_path <- job_get_dir(api, user, job_id)
    if (!dir.exists(results_path)) {
        api_stop(404L, "No results found")
    }
    partial <- isTRUE(partial) || identical(tolower(as.character(partial)), "true")
    if (job$status != .job_status_finished) {
        if (!partial) {
            api_stop(
                400L,
                "Job has not finished processing yet",
                id = "JobNotFinished"
            )
        }
        collection <- job_empty_collection(api, user, job)
    } else {
        collection_file <- file.path(results_path, "_collection.json")
        if (!file.exists(collection_file)) {
            api_stop(404L, "No results found")
        }
        collection <- jsonlite::read_json(collection_file)
    }
    if (is.list(collection$assets) && length(collection$assets)) {
        anames <- names(collection$assets)
        if (is.null(anames)) {
            anames <- as.character(seq_along(collection$assets))
        }
        for (i in seq_along(collection$assets)) {
            if (is.null(collection$assets[[i]]$title)) {
                collection$assets[[i]]$title <- anames[[i]]
            }
        }
    }
    expires_at <- Sys.time() + 3600
    collection$expires <- format(expires_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    if (is.null(collection$links) || !is.list(collection$links)) {
        collection$links <- list()
    }
    if (!is.null(req)) {
        host <- get_host(api, req)
        collection <- update_link(
            collection,
            rel = "canonical",
            href = make_job_files_url(
                host, user, job_id,
                file = "_collection.json"
            ),
            type = "application/json"
        )
        collection <- update_link(
            collection,
            rel = "self",
            href = make_url(host, "/jobs/", job_id, "/results"),
            type = "application/json"
        )
    }
    # Prefer a real extent inferred from the output rasters over a fabricated
    # world bbox. Only fills in when the collection lacks its own extent and
    # inference from files succeeds; otherwise left absent.
    if (job$status == .job_status_finished &&
        (is.null(collection$extent) || !length(collection$extent))) {
        inferred <- infer_results_extent(api, user, job_id)
        if (!is.null(inferred)) {
            collection$extent <- inferred
        }
    }
    if (is.null(collection$stac_version)) {
        collection$stac_version <- "1.0.0"
    }
    collection
}

#' Infer a STAC extent from a finished job's raster outputs
#'
#' Reads the on-disk raster results (`.tif` / `.nc`) and computes their combined
#' bounding box reprojected to EPSG:4326. Returns `NULL` when there are no
#' raster files or `terra` is unavailable, so callers avoid fabricating an
#' extent. The temporal interval is left open (`[null, null]`) since it cannot
#' be inferred from the raster footprint alone.
#'
#' @inheritParams job_helpers
#' @return A STAC `extent` list, or `NULL`.
#' @keywords internal
infer_results_extent <- function(api, user, job_id) {
    if (!requireNamespace("terra", quietly = TRUE)) {
        return(NULL)
    }
    job_dir <- job_get_dir(api, user, job_id)
    files <- list.files(
        job_dir,
        pattern = "\\.(tif|tiff|nc)$",
        ignore.case = TRUE,
        full.names = TRUE
    )
    files <- files[!startsWith(basename(files), "_")]
    if (!length(files)) {
        return(NULL)
    }
    bbox <- tryCatch(
        {
            acc <- NULL
            for (f in files) {
                r <- terra::rast(f)
                poly <- terra::as.polygons(terra::ext(r), crs = terra::crs(r))
                poly <- terra::project(poly, "EPSG:4326")
                ev <- as.vector(terra::ext(poly)) # xmin, xmax, ymin, ymax
                vals <- c(ev[[1]], ev[[3]], ev[[2]], ev[[4]])
                acc <- if (is.null(acc)) {
                    vals
                } else {
                    c(
                        min(acc[1], vals[1]), min(acc[2], vals[2]),
                        max(acc[3], vals[3]), max(acc[4], vals[4])
                    )
                }
            }
            acc
        },
        error = function(e) NULL
    )
    if (is.null(bbox) || any(!is.finite(bbox))) {
        return(NULL)
    }
    list(
        spatial = list(bbox = list(as.numeric(bbox))),
        temporal = list(interval = list(list(NULL, NULL)))
    )
}

#' Cancel a running job and optionally clear result artefacts
#'
#' @inheritParams job_helpers
#' @return `NULL`, invisibly.
#' @keywords internal
job_cancel_results <- function(api, user, job_id) {
    jobs <- job_read_rds(api, user)
    if (!(job_id %in% names(jobs))) {
        api_stop(404L, "Job not found")
    }
    procs <- procs_read_rds(api)
    proc <- procs[[job_id]]
    if (!is.null(proc)) {
        tryCatch(proc$kill(), error = function(e) NULL)
        procs[[job_id]] <- NULL
        procs_save_rds(api, procs)
    }
    job_dir <- job_get_dir(api, user, job_id)
    if (dir.exists(job_dir)) {
        files <- list.files(job_dir, full.names = TRUE, all.files = FALSE)
        keep <- basename(files) %in% c("logs.rds")
        unlink(files[!keep], recursive = TRUE)
    }
    job_upd_status(api, user, job_id, "canceled")
    invisible(NULL)
}
#' @rdname job_helpers
#' @export
job_empty_collection <- function(api, user, job) {
    collection <- list(
        `openeo:status` = job$status,
        type = "Collection",
        stac_version = "1.0.0",
        id = job$id,
        title = job$title,
        description = job$description,
        license = "various",
        extent = list(),
        links = list(),
        assets = list()
    )
    collection
}
job_info_check <- function(job_info) {
    if (!all(c("title", "description", "process") %in% names(job_info))) {
        api_stop(400L, "Invalid job data")
    }
}

#' Soft validation and defaults for job payloads
#'
#' Requires `process` for full creates (not for `partial` updates). Does **not**
#' require `title` / `description` so existing clients stay compatible. Fills
#' `plan` and `log_level` defaults when missing on full creates.
#'
#' @param job_info Named list from the request body (or merge candidate).
#' @param partial If `TRUE`, only validate fields that are present (PATCH-style).
#' @return A normalized named list (full create) or `job_info` (partial).
#' @keywords internal
job_check <- function(job_info, partial = FALSE) {
    if (is.null(job_info) || !is.list(job_info)) {
        api_stop(400L, "Missing job information")
    }
    if (partial) {
        if ("process" %in% names(job_info) && is.null(job_info$process)) {
            api_stop(400L, "Invalid job information: 'process' must not be null")
        }
        return(job_info)
    }
    if (!"process" %in% names(job_info) || is.null(job_info$process)) {
        api_stop(400L, "Invalid job information: 'process' is required")
    }
    list(
        title = job_info$title,
        description = job_info$description,
        process = job_info$process,
        plan = if (is.null(job_info$plan)) "Free" else job_info$plan,
        budget = if (is.null(job_info$budget)) 0.0 else job_info$budget,
        log_level = if (is.null(job_info$log_level)) {
            "Info"
        } else {
            job_info$log_level
        },
        links = if (is.null(job_info$links)) list() else job_info$links
    )
}

#' Attach openEO links to a job document
#'
#' @param job Job list (mutated links).
#' @param api API object.
#' @param req Plumber request.
#' @return `job` with `self` and optional `results` links.
#' @keywords internal
job_populate_links <- function(job, api, req) {
    if (is.null(job$links) || !is.list(job$links)) {
        job$links <- list()
    }
    host <- get_host(api, req)
    job <- update_link(
        job,
        rel = "self",
        href = make_url(host, "/jobs/", job$id),
        type = "application/json"
    )
    if (identical(job$status, .job_status_finished)) {
        job <- update_link(
            job,
            rel = "results",
            href = make_url(host, "/jobs/", job$id, "/results"),
            type = "application/json"
        )
    }
    job
}
