collection <- function(collection_id) {
  collections <- get("collections", envir = .openeo, inherits = FALSE)
  stopifnot(collection_id %in% names(collections))
  collections[[collection_id]]
}

load_collections <- function() {
  collections <- list()
  collections_id <- character()
  for (source in sits:::.sources()) {
    for (collection in sits:::.source_collections(source)) {
      platform <- sits:::.source_collection_satellite(source, collection)
      instrument <- sits:::.source_collection_sensor(source, collection)
      collections <- c(collections, list(
        list(
          id = paste(source, collection, sep = "/"),
          title = paste0(platform, " (", instrument, ")"),
          constellation = paste(platform, instrument, sep = "/"),
          description = "(Include a description on sits collections metadata)"
        )
      ))
      collections_id <- c(collections_id, paste(source, collection, sep = "/"))
    }
  }
  names(collections) <- collections_id
  assign("collections", collections, envir = .openeo)
}
