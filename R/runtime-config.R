#' Configure thread limits and Docker resource caps for openeocraft workers.
#'
#' Called from server/worker entrypoints (not from `.onLoad`) so callr
#' background workers inherit the same limits as the Plumber parent.
#'
#' @return Invisibly NULL.
#' @keywords internal
configure_openeocraft_runtime <- function() {
    for (v in c(
        "OMP_NUM_THREADS",
        "MKL_NUM_THREADS",
        "OPENBLAS_NUM_THREADS",
        "TORCH_NUM_THREADS"
    )) {
        if (!nzchar(Sys.getenv(v, unset = ""))) {
            args <- stats::setNames(list("1"), v)
            do.call(Sys.setenv, args)
        }
    }

    if (!file.exists("/.dockerenv")) {
        return(invisible(NULL))
    }

    options(
        openeocraft.resource_fraction = as.numeric(
            Sys.getenv("OPENEOCRAFT_RESOURCE_FRACTION", "0.5")
        ),
        openeocraft.multicores_max = as.integer(
            Sys.getenv("OPENEOCRAFT_MULTICORES_MAX", "4")
        ),
        openeocraft.memsize = as.integer(
            Sys.getenv("OPENEOCRAFT_MEMSIZE", "16")
        ),
        openeocraft.memsize_auto = FALSE
    )
    message(
        "[runtime] Docker resource limits: multicores_max=",
        getOption("openeocraft.multicores_max"),
        ", memsize=",
        getOption("openeocraft.memsize"),
        " GB, resource_fraction=",
        getOption("openeocraft.resource_fraction")
    )
    invisible(NULL)
}

#' Build API + process namespace for callr job workers.
#'
#' The Plumber parent holds a live API with a populated process namespace;
#' serializing that object into callr workers is unreliable. Workers bootstrap
#' the same configuration from disk instead.
#'
#' STAC integration via the optional GitHub package `openstac` is configured in
#' Docker/`plumber` deployments, not in this CRAN package path (`stac_api = NULL`).
#'
#' @return openEO API object with processes loaded.
#' @keywords internal
openeocraft_worker_api <- function() {
    configure_openeocraft_runtime()
    work_dir <- if (file.exists("/.dockerenv")) {
        "/var/openeo"
    } else {
        path.expand("~/openeo-tests")
    }
    processes_file <- "/opt/dockerfiles/inst/ml/processes.R"
    if (!file.exists(processes_file)) {
        processes_file <- system.file("ml/processes.R", package = "openeocraft")
    }
    api <- create_openeo_v1(
        id = "openeocraft",
        title = "openEO compliant R backend",
        description = "openEOcraft worker",
        backend_version = "0.4.1",
        stac_api = NULL,
        work_dir = work_dir,
        production = FALSE
    )
    set_credentials(api, file = path.expand("~/openeo-credentials.rds"))
    base_url <- Sys.getenv("OPENEOCRAFT_API_BASE_URL", unset = "")
    if (!nzchar(base_url) && file.exists("/.dockerenv")) {
        base_url <- "http://127.0.0.1:8000"
    }
    if (nzchar(base_url)) {
        assign("api_base_url", base_url, envir = attr(api, "env"))
    }
    load_processes(api, processes_file)
    api
}

# Back-compat alias for older call sites.
.openeocraft_worker_api <- openeocraft_worker_api
