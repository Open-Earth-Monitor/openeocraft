# TO-DO: show R errors into terminal
# Determine plumber file path - works both locally and in Docker
docker_path <- "/opt/dockerfiles/docker/plumber.R"
if (file.exists(docker_path)) {
  # Running in Docker container
  plumber_file <- docker_path
} else {
  # Running locally - try multiple possible paths
  # When running: Rscript docker/server.R (from repo root)
  # When running: Rscript server.R (from docker/ directory)
  possible_paths <- c(
    "docker/plumber.R", # From repo root
    "plumber.R" # From docker/ directory
  )
  plumber_file <- NULL
  for (path in possible_paths) {
    if (file.exists(path)) {
      plumber_file <- path
      break
    }
  }
  if (is.null(plumber_file)) {
    stop("Could not find plumber.R file. Tried: ", paste(possible_paths, collapse = ", "))
  }
}

plumber::plumb(plumber_file) |>
  plumber::pr_set_debug(TRUE) |>
  plumber::pr_run(port = 8000, host = "0.0.0.0")
