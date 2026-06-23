# Shared helpers for UC2 example scripts: poll logging and result download.

require_env <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    stop(sprintf("Missing required environment variable: %s", name), call. = FALSE)
  }
  value
}

normalize_host <- function(raw_host) {
  if (grepl("^https?://", raw_host)) {
    raw_host
  } else {
    paste0("http://", raw_host)
  }
}

connect_with_retry <- function(host, user, password, attempts = 12, sleep_seconds = 5) {
  last_error <- NULL
  for (i in seq_len(attempts)) {
    message(sprintf("Connection attempt %d/%d", i, attempts))
    trial <- try({
      con <- connect(host = host)
      login(con = con, user = user, password = password)
    }, silent = TRUE)
    if (!inherits(trial, "try-error")) {
      return(trial)
    }
    last_error <- trial
    if (i < attempts) {
      Sys.sleep(sleep_seconds)
    }
  }
  stop(
    sprintf(
      "Failed to connect/login to backend '%s' after %d attempts. Last error: %s",
      host, attempts, as.character(last_error)
    ),
    call. = FALSE
  )
}

job_status <- function(job, con) {
  info <- describe_job(job, con = con)
  status_raw <- info$status
  if (is.null(status_raw)) "" else tolower(as.character(status_raw))
}

init_poll_log <- function(output_dir) {
  poll_log <- Sys.getenv("JOB_POLL_LOG", unset = "")
  if (!nzchar(poll_log)) {
    poll_log <- file.path(output_dir, "job_poll.log")
  }
  dir.create(dirname(poll_log), recursive = TRUE, showWarnings = FALSE)
  assign(".poll_log_path", poll_log, envir = .GlobalEnv)
  invisible(poll_log)
}

append_poll_log <- function(msg) {
  poll_log <- get(".poll_log_path", envir = .GlobalEnv)
  line <- sprintf("%s %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg)
  cat(line, "\n", file = poll_log, append = TRUE)
  message(msg)
  flush.console()
}

wait_until_job_terminal <- function(job, con, poll_sec, max_sec) {
  start <- Sys.time()
  repeat {
    status <- job_status(job, con = con)
    elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    append_poll_log(sprintf("[%.0fs] Job %s status: %s", elapsed, job$id, status))

    if (status %in% c("finished", "completed", "done")) {
      return(invisible(TRUE))
    }
    if (status %in% c("error", "failed")) {
      stop(sprintf("Job failed with status: %s", status), call. = FALSE)
    }
    if (status %in% c("canceled", "cancelled")) {
      stop(sprintf("Job was canceled: %s", status), call. = FALSE)
    }
    if (elapsed > max_sec) {
      stop(
        sprintf(
          "Timed out after %.0fs waiting for job (max JOB_MAX_WAIT_SECONDS=%s)",
          elapsed,
          max_sec
        ),
        call. = FALSE
      )
    }
    Sys.sleep(poll_sec)
  }
}

resolve_job <- function(con, job_id = NULL) {
  if (is.null(job_id) || !nzchar(job_id)) {
    job_id <- Sys.getenv("OPENEO_JOB_ID", unset = "")
  }
  if (!nzchar(job_id)) {
    last_file <- Sys.getenv("JOB_ID_FILE", unset = "")
    if (!nzchar(last_file)) {
      out <- Sys.getenv("OUTPUT_DIR", unset = "/tmp/openeocraft-docker-results")
      last_file <- file.path(out, "last_job_id.txt")
    }
    if (file.exists(last_file)) {
      job_id <- trimws(readLines(last_file, n = 1L, warn = FALSE))
    }
  }
  if (!nzchar(job_id)) {
    stop("Set OPENEO_JOB_ID or provide last_job_id.txt from a prior submit.", call. = FALSE)
  }
  jobs <- list_jobs(con = con)
  job <- jobs[[job_id]]
  if (is.null(job)) {
    stop("Job not found: ", job_id, call. = FALSE)
  }
  job
}

download_job_results <- function(job, con, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(output_dir, job$id)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  api_ok <- tryCatch({
    download_results(job = job, folder = dest, con = con)
    TRUE
  }, error = function(e) {
    append_poll_log(sprintf("download_results failed: %s", conditionMessage(e)))
    FALSE
  })
  if (api_ok) {
    append_poll_log(sprintf("Downloaded via API to %s", dest))
    return(invisible(dest))
  }

  container <- Sys.getenv("OPENEO_DOCKER_CONTAINER", unset = "openeocraft")
  user <- Sys.getenv("OPENEO_USER", unset = "brian")
  job_dir <- sprintf("/var/openeo/workspace/%s/jobs/%s", user, job$id)
  cmd <- sprintf(
    "docker cp %s:%s/. %s/",
    shQuote(container),
    shQuote(job_dir),
    shQuote(dest)
  )
  append_poll_log(sprintf("Falling back to: %s", cmd))
  status <- system(cmd)
  if (status != 0L) {
    stop("Docker copy fallback failed with exit code ", status, call. = FALSE)
  }
  append_poll_log(sprintf("Downloaded via docker cp to %s", dest))
  invisible(dest)
}
