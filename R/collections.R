new_link <- function(rel, href, ...) {
  dots <- list(...)
  c(list(rel = rel, href = href), dots)
}

load_collections <- function() {
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
              href = get_endpoint("/collections")
            ),
            new_link(
              rel = "self",
              href = get_endpoint(
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
  assign("collections", collections, envir = .openeo, inherits = FALSE)
}

get_collections <- function(api, collection_id = NULL) {
  collections <- get("collections", envir = get_env(api), inherits = FALSE)
  if (is.null(collection_id))
    return(list(
      collections = unname(collections),
      links = list(
        new_link(
          rel = "self",
          href = get_endpoint("/collections")
        )
      )
    ))
  stopifnot(collection_id %in% names(collections))
  collections[[collection_id]]
}
