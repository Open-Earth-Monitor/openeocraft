# Define expected versions
expected_api_version <- "1.2.0"
expected_stac_version <- "1.0.0"

ensure_openstac <- function() {
  if (!requireNamespace("openstac", quietly = TRUE)) {
    stop("Package 'openstac' is required for this operation.", call. = FALSE)
  }
}

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
#'   `getHeader()` methods, and `mock_well_known_response()` returns a list that
#'   mimics the `.well-known/openeo` endpoint payload.
#'
#' @name mock_helpers
NULL

#' @rdname mock_helpers
#' @export
mock_req <- function(..., method = "GET") {
  dots <- list(...)
  paths <- unlist(dots[names(dots) == ""])
  vars <- dots[names(dots) != ""]

  # Extract headers
  headers <- vars[grepl("^HTTP_", names(vars))]

  req <- c(list(
    REQUEST_METHOD = method,
    HTTP_ACCESS_CONTROL_REQUEST_HEADERS = c(
      "content-type", "authorization", "accept"
    ),
    rook.url_scheme = "https",  # Enforcing HTTPS
    HTTP_HOST = "localhost",
    SERVER_NAME = "localhost",
    SERVER_PORT = NULL,
    PATH_INFO = paste0(paths, collapse = "/")
  ),
  # Add headers separately
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
  # Create openEO API object
  api <- create_openeo_v1(
    id = "openeocraft",
    title = "openEO compliant R backend",
    description = "OpenEOcraft offers a robust R framework designed for the development and deployment of openEO API applications.",
    backend_version = "0.2.0",
    stac_api = NULL,
    work_dir = tempdir(),
    conforms_to = NULL,
    production = FALSE
  )

  # Mock get mock users and  the mock token
  set_credentials(api, file = system.file("mock/mock-credentials.rds", package = "openeocraft"))

  # Mock processes
  processes_file <- system.file("mock/mock-processes.R", package = "openeocraft")
  load_processes(api, processes_file)

  api
}

mock_api_setup_plumber <- function(api, ..., api_base_url = NULL, wellknown_versions = list()) {
  stopifnot(is_absolute_url(api_base_url))
  api_attr(api, "api_base_url") <- api_base_url
  set_wellknown_versions(api, wellknown_versions)

  # Add required endpoints
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

mock_collections <- function(api) {
  ensure_openstac()
  req <- mock_req("/collections", method = "GET")
  res <- mock_res()
  openstac::api_collections(api, req, res)
}

mock_collection <- function(api, collection_id) {
  ensure_openstac()
  req <- mock_req("/collections", collection_id, method = "GET")
  res <- mock_res()
  openstac::api_collection(api, req, res, collection_id)
}

mock_items <- function(api, collection_id, limit = 10, bbox, datetime, page = 1) {
  ensure_openstac()
  req <- mock_req("/collections", collection_id, "items", method = "GET")
  res <- mock_res()
  openstac::api_items(api, req, res, collection_id, limit, bbox, datetime, page)
}

mock_item <- function(api, collection_id, item_id) {
  ensure_openstac()
  req <- mock_req("/collections", collection_id, "items", item_id, method = "GET")
  res <- mock_res()
  openstac::api_item(api, req, res, collection_id, item_id)
}

mock_search <- function(api, limit = 10, bbox = "", datetime, intersects = "", ids, collections, page = 1) {
  ensure_openstac()
  req <- mock_req("/search", method = "GET")
  res <- mock_res()
  openstac::api_search(api, req, res, limit, bbox, datetime, intersects, ids, collections, page)
}

#' @rdname mock_helpers
#' @export
mock_well_known_response <- function(api) {
  req <- mock_req("/.well-known/openeo", method = "GET")
  res <- mock_res()
  list(
    url = "http://0.0.0.0:8000/",
    api_version = expected_api_version,
    production = FALSE
  )
}
