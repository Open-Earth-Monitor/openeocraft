.openeo <- new.env()

#* @apiTitle Plumber Example API

#* Information about the back-end
#* @get /
function() {
  my_endpoint <- "/"
  list(
    api_version = "1.2.0",
    backend_version = "0.1.0",
    stac_version = "1.0.0",
    type = "Catalog",
    id = "sits-openeo",
    title = "SITS implementation on openEO",
    description = "This is the implmentation of SITS package on openEO framework.",
    production = FALSE,
    endpoints = list_endpoints(),
    links = list(
      href = paste0(get_host(), ":", get_port(), my_endpoint),
      rel = "self"
    )
  )
}

list_collections <- function() {
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
  assign("collections", collections, .openeo)
}

#* Basic metadata for all datasets
#* @serializer unboxedJSON
#* @get /collections
function() {
  my_endpoint <- "/collections"
  if (!exists("collections", envir = .openeo, inherits = FALSE))
    list_collections()
  collections <- list(
    collections = unname(get("collections", envir = .openeo, inherits = FALSE)),
    links = list(list(
      href = paste0(get_host(), ":", get_port(), my_endpoint),
      rel = "self"
    ))
  )
  collections
}

#* Full metadata for a specific dataset
#* @param collection_id Collection identifier
#* @serializer unboxedJSON
#* @get /collections/<collection_id>
function(collection_id) {
  collection_id <- URLdecode(collection_id)
  my_endpoint <- paste("/collections", collection_id, sep = "/")
  if (!exists("collections", envir = .openeo, inherits = FALSE))
    list_collections()
  collections <- get("collections", envir = .openeo, inherits = FALSE)
  stopifnot(collection_id %in% names(collections))
  collection <- collections[[collection_id]]
  list(
    stac_version = "1.0.0",
    stac_extensions = list(),
    id = collection$id,
    title = collection$title,
    description = collection$description,
    license = "proprietary",
    extent = list(
      spatial = list(bbox = unname(collection$bbox)),
      temporal = list(interval = list(unname(collection$interval)))
    ),
    links = list(
      list(
        rel = "root",
        href = paste0(get_host(), ":", get_port(), "/collections")
      ),
      list(
        rel = "self",
        href = paste0(get_host(), ":", get_port(), my_endpoint)
      )
    ),
    `cube:dimensions` = list(
      x = list(
        type = "spatial",
        axis = "x",
        extent = list(collection$bbox$xmin, collection$bbox$xmax)
      ),
      y = list(
        type = "spatial",
        axis = "y",
        extent = list(collection$bbox$ymin, collection$bbox$ymax)
      ),
      t = list(
        type = "temporal",
        extent = list(collection$interval$start_date, collection$interval$end_date)
      ),
      bands = list(
        type = "bands",
        values = list(collection$bands)
      )
    ),
    summaries = list(
      constellation = list(collection$constelation),
      `eo:bands` = list()
    )
  )
}

#* HTTP Basic authentication
#* @get /credentials/basic
function() {
  token = "b34ba2bdf9ac9ee1"
  list(access_token = token)
}

#* Process and download data synchronously
#* @post /result
function(req, res) {
  p <- req$body$process
  expr <- pgraph_expr(p)
  eval(expr, envir = parent.env(environment()))
}
