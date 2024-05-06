#' @import jsonlite
#' @import callr
#' @importFrom ids random_id
#'

# list of named lists, each containing job details
jobs_list <- list()

# workspace path for storing job data
workspace_path <- "../workspace/jobs/users"


#' Save job result
#'
#' @param job_id The identifier for the job
#' @export
job_save_result <- function(job_id, result) {
  results_path <- file.path(workspace_path, job_id, "results")

  # Create the results folder if it doesn't exist
  if (!dir.exists(results_path)) {
    dir.create(results_path, recursive = TRUE)
  }

  # Generate a unique file name for the result
  result_filename <- paste0("result_", Sys.time(), ".bin")
  result_file_path <- file.path(results_path, result_filename)

  # Save the result to the file
  writeBin(result, result_file_path)

  return(list(message = "Result saved", result_filename = result_filename))
}



#' Delete job
#'
#' @param job_id The identifier for the job
#' @export
job_delete <- function(job_id) {
  # Check if the job_id exists in the jobs_list
  if (!(job_id %in% names(jobs_list))) {
    return(list(message = "Job not found", code = 404))
  }

  # Remove the job from the jobs_list
  removed_job <- jobs_list[[job_id]]
  remove(jobs_list[[job_id]])

  # Delete the folder associated with the job_id
  job_folder_path <- file.path(workspace_path, job_id)
  if (file.exists(job_folder_path)) {
    unlink(job_folder_path, recursive = TRUE)
  }

  return(list(message = "Job deleted", code = 200, deleted_job = removed_job))
}

#' Create job
#'
#' @param req the request body containing job details
#' @export
job_create <- function(req) {
  if (is.na(req$id)) {
    req$id = ids::random_id(bytes = 6)
  }
  else {
    req$id = req$id
  }

  job <- list(
    id = req$id,
    status = "created",
    process = req$process,
    created = Sys.time(),
    title = req$title,
    description = req$description
  )
  jobs_list[[req$id]] <- job
  return(list(id = job$id, message = "Job created", code = 201))
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

#' Get job information/metadata
#'
#' @param job_id The identifier for the job
#' @export
job_info <- function(job_id) {
  # Check if the job_id exists in the jobs_list
  if (!(job_id %in% names(jobs_list))) {
    return(list(message = "Job not found", code = 404))
  }

  # Retrieve the job from the jobs_list
  job <- jobs_list[[job_id]]

  # Return all metadata for the job
  return(list(
    id = job$id,
    status = job$status,
    process = job$process,
    created = job$created,
    title = job$title,
    description = job$description
  ))
}

#' Update job
#'
#' @param job_id The identifier for the job
#' @param body The request body containing updates
#' @export
job_update <- function(job_id, body) {
    if (!(job_id %in% names(jobs_list))) {
      return(list(message = "Job not found", code = 404))
    }

    job <- jobs_list[[job_id]]

    # Update job details based on the provided body
    if (!is.null(body$process)) {
      job$process <- body$process
    }
    if (!is.null(body$title)) {
      job$title <- body$title
    }
    if (!is.null(body$description)) {
      job$description <- body$description
    }

    job$status <- "updated"
    job$updated <- Sys.time()

    # Update the job in the jobs_list
    jobs_list[[job_id]] <- job

    return(list(id = job$id, message = "Job updated", code = 200))
}

#' List all jobs
#'
#' @export
jobs_list_all <- function() {
  if (length(jobs_list) == 0) {
    return(list(message = "No jobs found", code = 404))
  } else {
    job_info <- lapply(jobs_list, function(job) {
      job <- c(job$id, job$status, job$process, job$created, job$title, job$description)
      names(job) <- c("id", "status", "process", "created", "title", "description")
      return(job)
    })
    return(job_info)
  }
}


# Get an estimate for a job
#' @param job_id The identifier for the job
#' @export
job_estimate <- function(job_id) {
  # This would likely call a function to calculate cost or duration based on job details
  # calculate_estimate(job_id) not implemented as depends on the cloud provider the software is running on
  return(list(message = "The cost estimates will depends on the cloud provider the software is running on"))
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

# Retrieve results for job results
#' @param job_id The identifier for the job
#' @export
job_get_results <- function(job_id) {
  results_path <- file.path(workspace_path, job_id, "results")
  if (dir.exists(results_path)) {
    files <- list.files(results_path, full.names = TRUE)
    results <- lapply(files, function(file) {
      # Read the binary data from the file
      result <- readBin(file, what = "raw", n = file.info(file)$size)
      return(result)
    })
    return(results)
  } else {
    return(list(message = "No results found"))
  }
}
