#' Register collection metadata with an API instance
#'
#' This convenience helper stores pointers to collection configuration for a
#' backend. The function is currently a stub and will be extended once STAC
#' integration is implemented.
#'
#' @param api An openeocraft API object.
#'
#' @param collections Optional list of collection definitions.
#'
#' @param stac_api Optional STAC client object.
#'
#' @param catalog_file Optional path to a static STAC catalogue.
#'
#' @return The `api` object, invisibly.
#'
#' @export
#'
#' @examples
#' \donttest{
#' api <- create_openeo_v1(
#'     id = "demo", title = "Demo", description = "Demo",
#'     backend_version = "0.4.1", stac_api = NULL,
#'     work_dir = tempdir(), production = FALSE
#' )
#' load_collections(api, collections = list())
#' }
load_collections <- function(api, collections = NULL, stac_api = NULL,
                             catalog_file = NULL) {
    api_attr(api, "collections") <- list(
        collections = collections,
        stac_api = stac_api,
        catalog_file = catalog_file
    )
    invisible(api)
}
