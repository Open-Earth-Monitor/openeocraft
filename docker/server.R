# Suppress torch's interactive "Do you want to continue?" prompt before any
# package load triggers torch's .onLoad hook (via openeocraft -> sits -> torch).
# Must be set here, before library() calls, for it to take effect in time.
Sys.setenv(TORCH_INSTALL = "1")

# Limit BLAS/torch threads before any worker forks (sits_regularize uses multicore).
Sys.setenv(
  OMP_NUM_THREADS = Sys.getenv("OMP_NUM_THREADS", "1"),
  MKL_NUM_THREADS = Sys.getenv("MKL_NUM_THREADS", "1"),
  OPENBLAS_NUM_THREADS = Sys.getenv("OPENBLAS_NUM_THREADS", "1"),
  TORCH_NUM_THREADS = Sys.getenv("TORCH_NUM_THREADS", "1")
)

# Plumber parent only; callr job workers pick up OPENEOCRAFT_* via processes.R + .onLoad.
if (file.exists("/.dockerenv")) {
  options(
    openeocraft.resource_fraction = as.numeric(
      Sys.getenv("OPENEOCRAFT_RESOURCE_FRACTION", "0.5")
    ),
    openeocraft.multicores_max = as.integer(
      Sys.getenv("OPENEOCRAFT_MULTICORES_MAX", "4")
    ),
    openeocraft.memsize = as.integer(Sys.getenv("OPENEOCRAFT_MEMSIZE", "16")),
    openeocraft.memsize_auto = FALSE
  )
  message(
    "[startup] Docker resource limits: multicores_max=",
    getOption("openeocraft.multicores_max"),
    ", memsize=",
    getOption("openeocraft.memsize"),
    " GB"
  )
}

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
