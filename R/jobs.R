#' @import jsonlite
#' @import callr
#' @importFrom ids random_id


# api_user_workspace(api, user) -> work_dir / <user>
# work_dir / <user> / jobs.rds
# work_dir / <user> / <job_id> /
# work_dir / <user> / files /

.job_status_error <- "error"
.job_status_finished <- "finished"

job_sync_id <- function() {
  format(Sys.time(), "job-%Y%m%d")
}
# list of named lists, each containing job details
job_read_rds <- function(api, user) {
  file <- file.path(api_user_workspace(api, user), "jobs.rds")
  if (!file.exists(file)) {
    return(list())
  }
  jobs <- readRDS(file)
  return(jobs)
}
job_save_rds <- function(api, user, job, jobs) {
  jobs[[job$id]] <- job
  file <- file.path(api_user_workspace(api, user), "jobs.rds")
  tryCatch(saveRDS(jobs, file), error = function(e) {
    api_stop(500, "Could not save the jobs file")
  })
  invisible(NULL)
}
job_crt_rds <- function(api, user, job) {
  jobs <- job_read_rds(api, user)
  job_save_rds(api, user, job, jobs)
}
job_upd_status <- function(api, user, job_id, status) {
  jobs <- job_read_rds(api, user)
  api_stopifnot(job_id %in% names(jobs), status = 500,
                "Could not find sync job id")
  job <- jobs[[job_id]]
  job$status <- status
  job_save_rds(api, user, job, jobs)
  job
}
job_delete_rds <- function(api, user, job, jobs) {
  if (!job$id %in% names(jobs))
    return(invisible(NULL))
  jobs[[job$id]] <- NULL
  file <- file.path(api_user_workspace(api, user), "jobs.rds")
  tryCatch(saveRDS(jobs, file), error = function(e) {
    api_stop(500, "Could not save the jobs index file")
  })
}

job_get_dir <- function(api, user, job_id) {
  file.path(api_user_workspace(api, user), "jobs", job_id)
}
job_new_dir <- function(api, user, job) {
  job_dir <- job_get_dir(api, user, job$id)
  if (dir.exists(job_dir)) {
    unlink(job_dir, recursive = TRUE)
    api_stopifnot(!dir.exists(job_dir), 500, "Could not delete the job ",
                  job$id, "'s folder")
  }
  dir.create(job_dir, recursive = TRUE)
  api_stopifnot(dir.exists(job_dir), 500, "Could not create the job ",
                job$id, "'s folder")
}
job_del_dir <- function(api, user, job_id) {
  job_dir <- job_get_dir(api, user, job_id)
  unlink(job_dir, recursive = TRUE)
  api_stopifnot(!dir.exists(job_dir), 500, "Could not delete the job ",
                job_id, "'s folder")
}

procs_read_rds <- function(api) {
  file <- file.path(api_workdir(api), "procs.rds")
  if (!file.exists(file)) {
    return(list())
  }
  procs <- readRDS(file)
  procs
}

procs_save_rds <- function(api, procs) {
  file <- file.path(api_workdir(api), "procs.rds")
  tryCatch(saveRDS(procs, file), error = function(e) {
    api_stop(500, "Could not save the procs file")
  })
  invisible(NULL)
}
logs_read_rds <- function(api, user, job_id) {
  file <- file.path(api_user_workspace(api, user), job_id, "logs.rds")
  if (!file.exists(file)) {
    return(list())
  }
  logs <- readRDS(file)
  logs
}
logs_save_rds <- function(api, user, job_id, logs) {
  file <- file.path(api_user_workspace(api, user), job_id, "logs.rds")
  tryCatch(saveRDS(logs, file), error = function(e) NULL)
  invisible(NULL)
}

# TODO: include all possible fields in here. Required parameters
#   must come before the `...` (ellipsis) parameter; optional parameters
#   comes after
log_append <- function(api, user, job_id, code, level, message, ...) {
  # TODO: log this
  # - solution to `path` field:
  # - introduce 'markers' into processes` function so that
  #   using the traceback mechanism we can distinguish
  #   functions that are processes from internal R functions.
  #   Then we can use this to create a openEO traceback to
  #   store in `path` field.
  # - solution to `usage` field:
  #   create `usage_*()` API to generate metrics to be included in
  #   `usage` field.
  logs <- logs_read_rds(api, user, job_id)
  logs[[length(logs) + 1]] <- list(
    id = job_id,
    code = code,
    level = level,
    message = message,
    time = Sys.time(), ...
  )
  logs_save_rds(api, user, job_id, logs)
}

#' Create job
#'
#' @param req the request body containing job details
#' @export
job_create <- function(api, req, res, user, job) {
  # TODO: create job_check
  #job_prepare(api, user, job)
  # - fill defaults
  # - check consistency of the provided fields
  # - also check plan
  job_id <- ids::random_id()
  job <- list(
    id = job_id,
    title = job$title,
    description = job$description,
    process = job$process,
    status = "created",
    created = Sys.time(),
    plan = job$plan,
    budget = job$budget,
    log_level = job$log_level,
    links = list()
  )
  # TODO: create directory and job RDS file as an atomic transaction
  # create job's directory
  job_new_dir(api, user, job)

  # TODO: how to avoid concurrency issues on reading/writing?
  # use some specific package? e.g. filelock, sqllite?, mongodb?
  jobs <- job_read_rds(api, user)
  job_save_rds(api, user, job, jobs)
  host <- get_host(api, req)
  res$setHeader("Location", make_url(host, "/jobs/", job_id))
  res$setHeader("OpenEO-Identifier", job_id)
  res$status <- 201
  list()
}
job_sync <- function(api, req, user, job_id) {
  job <- job_upd_status(api, user, job_id, "running")

  run_pgraph(api, req, user, job, job$process)
  job_upd_status(api, user, job_id, "finished")
  return(NULL)
  tryCatch({
    run_pgraph(api, req, user, job, job$process)
    job_upd_status(api, user, job_id, "finished")
  },
  # TODO: add more information of errors:
  # - traceback
  # - implement proc_stop() function that store more details like
  #   code?
  # - implement proc_warning() function to update logs for warning
  error = function(e) {
    code <- 100
    if ("code" %in% names(e)) {
      code <- e$code
    }
    job_upd_status(api, user, job_id, .job_status_error)
    log_append(api, user, job_id, code, "error", e$message)
    invisible(NULL)
  })
}
job_async <- function(api, req, user, job_id) {
  # proc <- callr::r_bg(
  #   func = job_sync,
  #   args = list(api, user, job_id)
  # )
  # proc
  job_sync(api, req, user, job_id)
  return(0)
}
#' Start a job asynchronously
#'
#' This function starts a job based on the job_id provided. It checks for job validity,
#' updates job statuses, and processes the job asynchronously.
#'
#' @param job_id The identifier for the job.
#' @return A list with a message and a status code.
#' @export
job_start <- function(api, req, res, user, job_id) {
  jobs <- job_read_rds(api, user)
  # Check if the job_id exists in the jobs_list
  if (!(job_id %in% names(jobs))) {
    api_stop(404, "Job not found")
  }
  procs <- procs_read_rds(api)
  if (!is.null(procs[[job_id]])) {
    return(list(id = job_id, message = "Job already started", code = 200))
  }
  # TODO: implement queue: check for maximum number of workers
  # procs_alive(procs) -> manage process not alive
  # define in the api how many workers to start?
  # length(procs)
  # length(procs) >= workers (per user?) --> wait
  proc <- job_async(api, req, user, job_id)
  procs[[job_id]] <- proc
  procs_save_rds(api, procs)
  res$status <- 202
  list()
}

#' Get job information/metadata
#'
#' @param job_id The identifier for the job
#' @export
job_info <- function(api, user, job_id) {
  job_id <- URLdecode(job_id)
  jobs <- job_read_rds(api, user)

  # Check if the job_id exists in the jobs_list
  if (!(job_id %in% names(jobs))) {
    api_stop(404, "Job not found")
  }

  # Retrieve the job from the jobs_list
  job <- jobs[[job_id]]

  # Return all metadata for the job
  job
}

#' Update job
#'
#' @param job_id The identifier for the job
#' @param body The request body containing updates
#' @export
job_update <- function(api, user, job_id, job) {
  # TODO: all checks should be done in api_*() functions level
  job_id <- URLdecode(job_id)
  # TODO: implement job_check partial parameter that does the check
  #   job fields independently.
  #job_check(job, partial = TRUE)

  jobs <- job_read_rds(api, user)

  # Check if the job_id exists in the jobs_list
  if (!(job_id %in% names(jobs))) {
    api_stop(404, "Job not found")
  }

  # Retrieve the job from the jobs_list
  current_job <- jobs[[job_id]]

  # Update job details based on the provided body
  job <- utils::modifyList(current_job, job)

  job$status <- "updated"
  job$updated <- Sys.time()

  # Update the job in the jobs_list
  job_save_rds(api, user, job, jobs)

  list(id = job_id, message = "Job updated", code = 200)
}


#' Delete job
#'
#' @param job_id The identifier for the job
#' @export
job_delete <- function(api, user, job_id) {
  job_id <- URLdecode(job_id)
  jobs <- job_read_rds(api, user)
  # Check if the job_id exists in the jobs_list
  if (!(job_id %in% names(jobs))) {
    api_stop(404, "Job not found")
  }
  removed_job <- jobs[[job_id]]
  job_delete_rds(api, user, removed_job, jobs)
  # Delete the folder associated with the job_id
  job_del_dir(api, user, job_id)
  list(message = "Job deleted", code = 200, deleted_job = removed_job$id)
}

#' @export
job_list_all <- function(api, user) {
  jobs <- job_read_rds(api, user)
  jobs <- list(
    jobs = unname(lapply(jobs, \(job) {
      job[c("id", "status", "created")]
    })),
    # TODO: populate this link with some function like we do in other endpoints
    links = list()
  )
  jobs
}


# Get an estimate for a job
#' @export
job_estimate <- function(api, user, job_id) {
  # This would likely call a function to calculate cost or duration based on job details
  # calculate_estimate(job_id) not implemented as depends on the cloud provider the software is running on
  return(list(message = "The cost estimates will depends on the cloud provider the software is running on"))
}

# Retrieve logs for a job
#' @export
job_logs <- function(api, user, job_id, offset = 0, level = "info", limit = 10) {
  level_list <- c("error", "warning", "info", "debug")
  job_id <- URLdecode(job_id)
  offset <- as.integer(offset)
  if (is.na(offset)) offset <- 0
  api_stopifnot(level %in% level_list, 400, "level must be one of ",
                paste0("'", level_list, "'", collapse = ", "))
  limit <- as.integer(limit)
  api_stopifnot(limit >= 1, 400, "limit parameter must be >= 1")
  logs <- logs_read_rds(api, user, job_id)
  levels <- vapply(logs, \(log) log$level, character(1))
  selection <- match(levels, level_list) <= match(level, level_list)
  list(level = level, logs = logs[selection], links = list())
}

# Retrieve results for job results
#' @export
job_get_results <- function(api, user, job_id) {
  jobs <- job_read_rds(api, user)
  # Check if the job_id exists in the jobs_list
  if (!(job_id %in% names(jobs))) {
    api_stop(404, "Job not found")
  }
  job <- jobs[[job_id]]
  if (job$status == .job_status_error) {
    api_stop(424, "Job returned an error")
  }
  results_path <- file.path(job_get_dir(api, user, job_id))
  if (!dir.exists(results_path)) {
    api_stop(404, "No results found")
  }
  if (job$status != "finished") {
    job_empty_collection(api, user, job)
  }
  jsonlite::read_json(file.path(results_path, "_collection.json"))
}
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
