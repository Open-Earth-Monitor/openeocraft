#' @export
load_collections <- function(api, collections = NULL, stac_api = NULL,
                             catalog_file = NULL) {
  # TODO implement connection to STAC API and static STAC catalog
  # TODO check input object
  collections_id <- vapply(collections, `[[`, character(1), "id")
  names(collections) <- collections_id
  set_attr(api, "collections", collections)
}

get_collections <- function(api, req, res, collection_id = NULL) {
  host <- get_host(api, req)
  collections <- get_attr(api, "collections")
  collections <- lapply(collections, function(collection) {
    collection$links <- list(
      new_link(
        rel = "root",
        href = get_link(host, "/collections")
      ),
      new_link(
        rel = "self",
        href = get_link(host, "/collections", collection$id)
      ),
      new_link(
        rel = "item",
        href = get_link(host, "/collections", collection$id, "items")
      )
    )
    collection
  })
  if (is.null(collection_id)) {
    collection_list <- list(
      collections = unname(collections),
      links = list(
        new_link(
          rel = "self",
          href = get_link(host, "/collections")
        )
      )
    )
    return(collection_list)
  }
  stopifnot(collection_id %in% names(collections))
  collections[[collection_id]]
}
