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
load_collections <- function(api, collections = NULL, stac_api = NULL,
                             catalog_file = NULL) {
  api_attr(api, "collections") <- list(
    collections = collections,
    stac_api = stac_api,
    catalog_file = catalog_file
  )
  invisible(api)
}
