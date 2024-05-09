#' Create response document
#'
#' These are functions responsible for creating the document of the
#' API endpoint response. It uses the `api` and the `req` objects
#' to make documents to be send as response to the user. It does
#' this process by dispatching to specific API implementations.
#'
#' \itemize{
#'
#' \item `doc_landing_page`: Creates document as a response to
#'   `/` endpoint.
#'
#' \item `doc_conformance`: Creates document as a response to
#'   `/conformance` endpoint.
#'
#' \item `doc_collections`: Creates document as a response to
#'   `/collections` endpoint.
#'
#' \item `doc_collection`: Creates document as a response to
#'   `/collection/{collection_id}` endpoint.
#'
#' \item `doc_items`: Creates document as a response to
#'   `/collection/{collection_id}/items` endpoint.
#'
#' \item `doc_item`: Creates document as a response to
#'   `/collection/{collection_id}/item/{item_id}` endpoint.
#'
#' \item `doc_search`: Creates document as a response to
#'   `/search` endpoint.
#'
#' }
#'
#' @param api An object representing the API.
#'
#' @param req The request object from the `plumber` package,
#'   containing information about the HTTP request made to the
#'   API endpoint.
#'
#' @param collection_id The identifier of the collection. This
#'   parameter specifies which collection the request is targeting.
#'
#' @param item_id The identifier of the item within the specified
#'   collection. This parameter specifies which item the request
#'   is targeting.
#'
#' @param limit The maximum number of items to return. If not
#'   specified, the default value is used.
#'
#' @param bbox The bounding box for spatial filtering, specified
#'   as a numeric vector of four coordinates
#'   (`long_min`, `lat_min`, `long_max`, `lat_max`). Use
#'   `deparse_array()` to convert it to comma-separated string.
#'
#' @param datetime The temporal filter for items. It must be
#'   specified as a
#'   `list(start = start_date, end = end_date, exact = exact_date)`
#'   object. Use `deparse_datetime()` function to convert this
#'   object to a STAC datetime string.
#'
#' @param intersects The spatial filter for items, specified as
#'   a GeoJSON geometry object representing the area of interest.
#'   Use `deparse_json()` function to convert the GeoJSON
#'   geometry list into a GeoJSON string.
#'
#' @param ids A list of item identifiers to filter the search
#'   results. Use `deparse_array()` to convert it to a comma-
#'   separated string.
#'
#' @param collections A list of collection identifiers to filter
#'   the search results. Use `deparse_array()` to convert it to a
#'   comma-separated string.
#'
#' @param page The page number of the results when paginating.
#'
#' @return For API creation functions, returns a api object.
#'   For API handling functions, returns the document to return
#'   as response.
#'
#' @seealso [deparse_array()], [deparse_datetime()], [deparse_json()]
#'
#' @references
#' For more information about the STAC specification,
#' see: \url{https://stacspec.org/}
#'
#' For more information about the OGC API specification,
#' see: \url{http://www.opengis.net/doc/IS/ogcapi-features-1/1.0}
#'
#' @name doc_handling
#'
NULL
#' @rdname doc_handling
#' @export
doc_wellknown <- function(api, req) {
  UseMethod("doc_wellknown", api)
}
#' @rdname doc_handling
#' @export
doc_landing_page <- function(api, req) {
  UseMethod("doc_landing_page", api)
}
#' @rdname doc_handling
#' @export
doc_conformance <- function(api, req) {
  UseMethod("doc_conformance", api)
}
#' @rdname doc_handling
#' @export
doc_processes <- function(api, req) {
  UseMethod("doc_processes", api)
}
#' @rdname doc_handling
#' @export
doc_collections <- function(api, req) {
  UseMethod("doc_collections", api)
}
#' @rdname doc_handling
#' @export
doc_collection <- function(api, req, collection_id) {
  UseMethod("doc_collection", api)
}
#' @rdname doc_handling
#' @export
doc_items <- function(api,
                      req,
                      collection_id,
                      limit,
                      bbox,
                      datetime,
                      page) {
  UseMethod("doc_items", api)
}
#' @rdname doc_handling
#' @export
doc_item <- function(api, req, collection_id, item_id) {
  UseMethod("doc_item", api)
}
#' @rdname doc_handling
#' @export
doc_search <- function(api,
                       req,
                       limit,
                       bbox,
                       datetime,
                       intersects,
                       ids,
                       collections,
                       page) {
  UseMethod("doc_search", api)
}
#' @keywords internal
links_landing_page <- function(doc, api, req) {
  UseMethod("links_landing_page", api)
}
#' @keywords internal
links_collection <- function(doc, api, req) {
  UseMethod("links_collection", api)
}
#' @keywords internal
links_collections <- function(doc, api, req) {
  UseMethod("links_collections", api)
}
#' @keywords internal
links_item <- function(doc, api, req) {
  UseMethod("links_item", api)
}
#' @keywords internal
links_items <- function(doc,
                        api,
                        req,
                        collection_id,
                        limit,
                        bbox,
                        datetime,
                        page) {
  UseMethod("links_items", api)
}
links_search <- function(doc,
                         api,
                         req,
                         limit,
                         bbox,
                         datetime,
                         intersects,
                         ids,
                         collections,
                         page) {
  UseMethod("links_search", api)
}
