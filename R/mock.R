# Define expected versions
expected_api_version <- "1.2.0"
expected_stac_version <- "1.0.0"

#' Mock request/response helpers
#'
#' Utilities that emulate plumber request and response objects for tests and
#' examples.
#'
#' @param ... Path segments or named headers used to construct the mock
#'   request.
#'
#' @param method HTTP method to emulate.
#'
#' @param api An openeocraft API object.
#'
#' @return `mock_req()` returns a list representing a plumber request,
#'   `mock_res()` returns an object with minimal `setHeader()` and
#'   `getHeader()` methods, and `mock_well_known_response()` returns a
#'   list that mimics the `.well-known/openeo` endpoint payload.
#'
#' @name mock_helpers
#'
#' @examples
#' req <- mock_req("/jobs", method = "GET")
#' res <- mock_res()
#' res$setHeader("Content-Type", "application/json")
#' res$getHeader("Content-Type")
NULL

#' @rdname mock_helpers
#' @export
mock_req <- function(..., method = "GET") {
    dots <- list(...)
    paths <- unlist(dots[names(dots) == ""])
    vars <- dots[names(dots) != ""]

    headers <- vars[grepl("^HTTP_", names(vars))]
    args <- vars$args
    if (is.null(args)) {
        args <- list()
    }

    req <- c(
        list(
            REQUEST_METHOD = method,
            HTTP_ACCESS_CONTROL_REQUEST_HEADERS = c(
                "content-type", "authorization", "accept"
            ),
            rook.url_scheme = "https",
            HTTP_HOST = "localhost",
            SERVER_NAME = "localhost",
            SERVER_PORT = NULL,
            PATH_INFO = paste0(paths, collapse = "/"),
            args = args
        ),
        headers
    )

    req
}

#' @rdname mock_helpers
#' @export
mock_res <- function() {
    headers <- new.env()
    list2env(
        list(
            setHeader = function(key, value) {
                headers[[key]] <<- value
            },
            getHeader = function(key) {
                headers[[key]]
            }
        )
    )
}

mock_create_openeo_v1 <- function() {
    api <- create_openeo_v1(
        id = "openeocraft",
        title = "openEO compliant R backend",
        description = paste0(
            "OpenEOcraft offers a robust R framework ",
            "designed for the development and deployment ",
            "of openEO API applications."
        ),
        backend_version = "0.4.1",
        stac_api = NULL,
        work_dir = tempdir(),
        conforms_to = NULL,
        production = FALSE
    )

    set_credentials(
        api,
        file = system.file(
            "mock/mock-credentials.rds",
            package = "openeocraft"
        )
    )

    processes_file <- system.file(
        "mock/mock-processes.R",
        package = "openeocraft"
    )
    load_processes(api, processes_file)

    api
}

mock_api_setup_plumber <- function(api, ..., api_base_url = NULL,
                                   wellknown_versions = list()) {
    stopifnot(is_absolute_url(api_base_url))
    api_attr(api, "api_base_url") <- api_base_url
    set_wellknown_versions(api, wellknown_versions)

    api_attr(api, "endpoints") <- list(
        list(path = "/collections", methods = c("GET")),
        list(path = "/processes", methods = c("GET")),
        list(path = "/jobs", methods = c("GET", "POST", "DELETE"))
    )
    api
}

mock_landing_page <- function(api) {
    req <- mock_req("/", method = "GET")
    res <- mock_res()
    api_landing_page(api, req, res)
}

mock_conformance <- function(api) {
    req <- mock_req("/conformance", method = "GET")
    res <- mock_res()
    api_conformance(api, req, res)
}

mock_result <- function(api) {
    req <- mock_req("/result", method = "POST")
    res <- mock_res()
    api_result(api, req, res)
}

#' @rdname mock_helpers
#' @export
mock_well_known_response <- function(api) {
    list(
        url = "http://0.0.0.0:8000/",
        api_version = expected_api_version,
        production = FALSE
    )
}
