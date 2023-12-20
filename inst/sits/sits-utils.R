# stac_endpoint <- "https://example.com/stac"
# load_collections(api, stac_api = stac_endpoint)
# catalog_file <- system.file("sits/catalog.json", package = "openeocraft")
# load_collections(api, stac_catalog = catalog_file)

#########################################

collections <- list()
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
        links = list(),
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
  }
}

jsonlite::write_json(
  collections, 
  path = "inst/sits/collections.json", 
  pretty = TRUE,
  auto_unbox = TRUE
)

