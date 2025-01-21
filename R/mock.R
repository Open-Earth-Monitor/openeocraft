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
mock_api_file_formats <- function(api, req, res) {
  list(
    input = list(
      GeoTIFF = list(
        description = "Geotiff is one of the most widely supported formats. This backend allows reading from Geotiff to create raster data cubes.",
        gis_data_types = list("raster"),
        parameters = list(),
        title = "GeoTiff"
      ),
      GeoJSON = list(
        description = "GeoJSON allows sending vector data as part of your JSON request. GeoJSON is always in EPSG:4326.",
        gis_data_types = list("vector"),
        parameters = list(),
        title = "GeoJSON"
      ),
      JSON = list(
        description = "JSON is a generic data serialization format. Being generic, it allows to represent various data types (raster, vector, table, etc.).",
        gis_data_types = list("raster", "vector"),
        parameters = list(),
        title = "JavaScript Object Notation (JSON)"
      )
    ),
    output = list(
      GeoTIFF = list(
        description = "Cloud Optimized Geotiff is one of the most widely supported formats and thus a popular choice for further dissemination. This implementation stores all bands in one file, and creates one file per timestamp in your datacube.",
        gis_data_types = list("raster"),
        parameters = list(
          ZLEVEL = list(
            default = 6,
            description = "Specifies the compression level used for DEFLATE compression.",
            type = "integer"
          ),
          colormap = list(
            default = NULL,
            description = "Allows specifying a colormap for single-band GeoTIFFs.",
            type = c("object", "null")
          )
        ),
        title = "GeoTiff"
      ),
      JSON = list(
        description = "JSON is a generic data serialization format. Being generic, it allows to represent various data types (raster, vector, table, etc.).",
        gis_data_types = list("raster", "vector"),
        parameters = list(),
        title = "JavaScript Object Notation (JSON)"
      ),
      netCDF = list(
        description = "netCDF files allow to accurately represent an openEO datacube and its metadata.",
        gis_data_types = list("raster"),
        parameters = list(
          filename_prefix = list(
            default = NULL,
            description = "Specifies the filename prefix when outputting multiple files.",
            type = "string"
          )
        ),
        title = "Network Common Data Form"
      )
    )
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
