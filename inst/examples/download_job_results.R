#!/usr/bin/env Rscript
# Poll a submitted job (optional) and download results to OUTPUT_DIR.
#
# Usage:
#   export OPENEO_HOST=http://127.0.0.1:8000 OPENEO_USER=brian OPENEO_PASSWORD=...
#   export OPENEO_JOB_ID=<job_id>   # or rely on OUTPUT_DIR/last_job_id.txt
#   export OUTPUT_DIR=/tmp/openeocraft-docker-results
#   Rscript inst/examples/download_job_results.R
#
# Set WAIT_FOR_JOB=false to skip polling (job already finished).

library(openeo)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  script_dir <- dirname(sub("^--file=", "", file_arg[[1L]]))
  helpers <- file.path(script_dir, "uc2_job_client.R")
} else {
  helpers <- "inst/examples/uc2_job_client.R"
}
source(helpers, local = TRUE)

host <- normalize_host(require_env("OPENEO_HOST"))
user <- require_env("OPENEO_USER")
password <- require_env("OPENEO_PASSWORD")
output_dir <- Sys.getenv("OUTPUT_DIR", unset = "/tmp/openeocraft-docker-results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
init_poll_log(output_dir)

poll_seconds <- as.numeric(Sys.getenv("JOB_POLL_SECONDS", unset = "30"))
max_wait_seconds <- as.numeric(Sys.getenv("JOB_MAX_WAIT_SECONDS", unset = "172800"))
if (!is.finite(poll_seconds) || poll_seconds < 1) {
  poll_seconds <- 30
}
if (!is.finite(max_wait_seconds) || max_wait_seconds < poll_seconds) {
  max_wait_seconds <- 172800
}

wait_for_job <- tolower(Sys.getenv("WAIT_FOR_JOB", unset = "true"))
wait_for_job <- wait_for_job %in% c("1", "true", "yes", "y")

connection <- connect_with_retry(host = host, user = user, password = password)
job <- resolve_job(con = connection)

append_poll_log(sprintf("Resolved job %s (wait=%s)", job$id, wait_for_job))

if (wait_for_job) {
  wait_until_job_terminal(
    job = job,
    con = connection,
    poll_sec = poll_seconds,
    max_sec = max_wait_seconds
  )
}

dest <- download_job_results(job = job, con = connection, output_dir = output_dir)
append_poll_log("Done.")
message("Results in: ", dest)
