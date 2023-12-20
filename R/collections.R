new_link <- function(rel, href, ...) {
  dots <- list(...)
  c(list(rel = rel, href = href), dots)
}

load_collections <- function(api, collections = NULL, stac_api = NULL,
                             catalog_file = NULL) {
  # TODO implement connection to STAC API and static STAC catalog
  # TODO check input object
  collections_id <- vapply(collections, `[[`, character(1), "id")
  names(collections) <- collections_id
  set_attr(api, "collections", collections)
}

get_collections <- function(api, collection_id = NULL) {
  collections <- get_attr(api, "collections")
  collections <- lapply(collections, function(collection) {
    collection$links <- list(
      new_link(
        rel = "root",
        href = get_endpoint(api, "/collections")
      ),
      new_link(
        rel = "self",
        href = get_endpoint(
          api,
          paste("/collections", collection$id, sep = "/")
        )
      ),
      new_link(
        rel = "item",
        href = get_endpoint(
          api,
          paste("/collections", collection$id, "items", sep = "/")
        )
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
          href = get_endpoint(api, "/collections")
        )
      )
    )
    return(collection_list)
  }
  stopifnot(collection_id %in% names(collections))
  collections[[collection_id]]
}
