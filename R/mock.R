# Define expected versions
expected_api_version <- "1.2.0"
expected_stac_version <- "1.0.0"

#' @export
mock_req <- function(..., method = "GET") {
  dots <- list(...)
  paths <- unlist(dots[names(dots) == ""])
  vars <- dots[names(dots) != ""]
  req <- list(
    REQUEST_METHOD = method,
    HTTP_ACCESS_CONTROL_REQUEST_HEADERS = c(
      "content-type", "authorization", "accept"
    ),
    rook.url_scheme = "mock",
    HTTP_HOST = "localhost",
    SERVER_NAME = "localhost",
    SERVER_PORT = NULL,
    PATH_INFO = paste0(paths, collapse = "/")
  )
  req <- c(req, vars)
  req
}

#' @export
mock_res <- function() {
  list(
    setHeader = function(key, value) {},
    status = 200
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
    work_dir = getwd(),
    conforms_to = NULL,
    production = FALSE
  )
  api
}

mock_api_setup_plumber <- function(api, ...,
                                   api_base_url = NULL,
                                   wellknown_versions = list()) {
  stopifnot(is_absolute_url(api_base_url))
  api_attr(api, "api_base_url") <- api_base_url
  set_wellknown_versions(api, wellknown_versions)
  # Add required endpoints
  api_attr(api, "endpoints") <- list(
    list(
      path = "/collections",
      methods = c("GET")
    ),
    list(
      path = "/processes",
      methods = c("GET")
    ),
    list(
      path = "/jobs",
      methods = c("GET", "POST", "DELETE")
    )
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
  req <- mock_req("/collections", method = "GET")
  res <- mock_res()
  api_collections(api, req, res)
}
mock_collection <- function(api, collection_id) {
  req <- mock_req("/collections", collection_id, method = "GET")
  res <- mock_res()
  api_collection(api, req, res, collection_id)
}
mock_items <- function(api,
                       collection_id,
                       limit = 10,
                       bbox,
                       datetime,
                       page = 1) {
  req <- mock_req("/collections", collection_id, "items", method = "GET")
  res <- mock_res()
  api_items(
    api = api,
    req = req,
    res = res,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page
  )
}
mock_item <- function(api, collection_id, item_id) {
  req <- mock_req(
    "/collections",
    collection_id,
    "items",
    item_id,
    method = "GET"
  )
  res <- mock_res()
  api_item(api, req, res, collection_id, item_id)
}
mock_search <- function(api,
                        limit = 10,
                        bbox = "",
                        datetime,
                        intersects = "",
                        ids,
                        collections,
                        page = 1) {
  req <- mock_req("/search", method = "GET")
  res <- mock_res()
  api_search(
    api = api,
    req = req,
    res = res,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    intersects = intersects,
    ids = ids,
    collections = collections,
    page = page
  )
}

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
