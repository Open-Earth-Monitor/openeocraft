#' Configure thread limits and Docker resource caps for openeocraft workers.
#'
#' Called from package load and at job start so callr background workers inherit
#' the same limits as the Plumber parent (server.R options do not propagate).
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
            Sys.setenv(v = "1")
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
#' @return openEO API object with processes loaded.
#' @keywords internal
.openeocraft_worker_api <- function() {
    work_dir <- if (file.exists("/.dockerenv")) {
        "/var/openeo"
    } else {
        "~/openeo-tests"
    }
    processes_file <- "/opt/dockerfiles/inst/ml/processes.R"
    if (!file.exists(processes_file)) {
        processes_file <- system.file("ml/processes.R", package = "openeocraft")
    }
    file <- system.file("ml/db.rds", package = "openeocraft")
    stac_api <- openstac::create_stac(
        id = "openlandmap",
        title = "OpenLandMap STAC API",
        description = "OpenLandMap STAC API"
    )
    stac_api <- openstac::set_db(stac_api, driver = "local", file = file)
    api <- create_openeo_v1(
        id = "openeocraft",
        title = "openEO compliant R backend",
        description = "openEOcraft worker",
        backend_version = "0.3.1",
        stac_api = stac_api,
        work_dir = work_dir,
        production = FALSE
    )
    set_credentials(api, file = "~/openeo-credentials.rds")
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
