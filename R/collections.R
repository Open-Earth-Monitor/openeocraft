new_link <- function(rel, href, ...) {
  dots <- list(...)
  c(list(rel = rel, href = href), dots)
}

load_collections <- function(api) {
  collections <- list()
  collections_id <- character()
  for (source in sits:::.sources()) {
    for (collection in sits:::.source_collections(source)) {
      collection_id <- paste(source, collection, sep = "/")
      platform <- sits:::.source_collection_satellite(source, collection)
      instrument <- sits:::.source_collection_sensor(source, collection)
      collections <- c(collections, list(
        list(
          stac_version = "1.0.0",
          stac_extensions = list(),
          id = collection_id,
          title = paste0(platform, " (", instrument, ")"),
          description = "(Include a description on sits collections metadata)",
          license = "proprietary",
          extent = list(
            spatial = list(bbox = list()),
            temporal = list(interval = list(list()))
          ),
          links = list(
            new_link(
              rel = "root",
              href = get_endpoint(api, "/collections")
            ),
            new_link(
              rel = "self",
              href = get_endpoint(
                api,
                paste("/collections", collection_id, sep = "/")
              )
            )
          ),
          `cube:dimensions` = list(
            x = list(
              type = "spatial",
              axis = "x",
              extent = list()
            ),
            y = list(
              type = "spatial",
              axis = "y",
              extent = list()
            ),
            t = list(
              type = "temporal",
              extent = list()
            ),
            bands = list(
              type = "bands",
              values = list()
            )
          ),
          summaries = list(
            constellation = list(paste(platform, instrument, sep = "/")),
            `eo:bands` = list()
          )
        )
      ))
      collections_id <- c(collections_id, collection_id)
    }
  }
  names(collections) <- collections_id
  set_attr(api, "collections", collections)
}

get_collections <- function(api, collection_id = NULL) {
  collections <- get_attr(api, "collections")
  if (is.null(collection_id))
    return(list(
      collections = unname(collections),
      links = list(
        new_link(
          rel = "self",
          href = get_endpoint(api, "/collections")
        )
      )
    ))
  stopifnot(collection_id %in% names(collections))
  collections[[collection_id]]
}
