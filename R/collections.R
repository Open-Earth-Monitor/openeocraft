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
            list(
              rel = "root",
              href = paste0(get_host(), ":", get_port(), "/collections")
            ),
            list(
              rel = "self",
              href = paste0(
                get_host(), ":", get_port(),
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

get_collections <- function(collection_id = NULL) {
  collections <- get("collections", envir = .openeo, inherits = FALSE)
  if (is.null(collection_id))
    return(list(
      collections = unname(collections),
      links = list(list(
        href = paste0(get_host(), ":", get_port(), "/collections"),
        rel = "self"
      ))
    ))
  stopifnot(collection_id %in% names(collections))
  collections[[collection_id]]
}
