#' @import jsonlite
#' @import callr
#'

workspace_path <- "./workspace/jobs"

#' Save job result
#'
#' @param job_id The identifier for the job
#' @export
job_save_result <- function(job_id) {
  # TODO: Implement function
}

#' Access job API
#'
#' @param job_id The identifier for the job
#' @export
job_api <- function(job_id) {
  # TODO: Implement function
}

#' Save job
#'
#' @param job_id The identifier for the job
#' @export
job_save <- function(job_id) {
  # TODO: Implement function
}

#' Read job
#'
#' @param job_id The identifier for the job
#' @export
job_read <- function(job_id) {
  # TODO: Implement function
}

#' Delete job
#'
#' @param job_id The identifier for the job
#' @export
job_delete <- function(job_id) {
  # TODO: Implement function
}

#' Create job
#'
#' @param job_id The identifier for the job
#' @export
job_create <- function(job_id) {
  # TODO: Implement function
}

#' Get job API parameters
#'
#' @param job_id The identifier for the job
#' @export
job_api_params <- function(job_id) {
  # TODO: Implement function
}

#' Start job
#'
#' @param job_id The identifier for the job
#' @export
job_start <- function(job_id) {
  # TODO: Implement function
}

#' Get job information
#'
#' @param job_id The identifier for the job
#' @export
job_info <- function(job_id) {
  # TODO: Implement function
}

#' Update job
#'
#' @param job_id The identifier for the job
#' @param body The request body containing updates
#' @export
job_update <- function(job_id, body) {
  # TODO: Implement function
}

#' List all jobs
#'
#' @export
jobs_list_all <- function() {
  # TODO: Implement function
}


# Get an estimate for a job
#' @param job_id The identifier for the job
#' @export
job_estimate <- function(job_id) {
  # TO DO: This would likely call a function to calculate cost or duration based on job details
  estimate <- NULL
  return(estimate)
}

# Retrieve logs for a job
#' @param job_id The identifier for the job
#' @export
job_logs <- function(job_id) {
  logs_path <- file.path(workspace_path, job_id, "logs.txt")
  if (file.exists(logs_path)) {
    logs <- readLines(logs_path)
    return(logs)
  } else {
    return(list(message = "No logs found"))
  }
}

# Retrieve results or URLs for job results
#' @param job_id The identifier for the job
#' @export
job_get_results <- function(job_id) {
  results_path <- file.path(workspace_path, job_id, "results")
  if (dir.exists(results_path)) {
    files <- list.files(results_path, full.names = TRUE)
    return(files)
  } else {
    return(list(message = "No results found"))
  }
}


