#* @apiTitle openEO API
#* @apiVersion 1.2.0

api_version <- "1.2.0"


#* Information about the back-end
#* @serializer unboxedJSON
#* @get /.well-known/openeo
function() {
  # TODO: include this metadata into a config or init function
  list(
    versions = list(
      list(
        url = get_endpoint(api, "/"),
        production = FALSE,
        api_version = api_version
      )
    )
  )
}

#* Information about the back-end
#* @get /
function() {
  # TODO: include this metadata into a config or init function
  list(
    id = api$id,
    title = api$title,
    description = api$description,
    backend_version = api$backend_version,
    stac_version = "1.0.0",
    type = "Catalog",
    api_version = api_version,
    production = FALSE,
    endpoints = list_endpoints(api),
    links = list(
      # TODO: list other types of links
      new_link(
        href = get_endpoint(api, "/"),
        rel = "self"
      )
    )
  )
}

#* Basic metadata for all datasets
#* @serializer unboxedJSON
#* @get /collections
function() {
  # TODO: add format function to build up final object to deliver
  get_collections(api)
}

#* Full metadata for a specific dataset
#* @param collection_id Collection identifier
#* @serializer unboxedJSON
#* @get /collections/<collection_id>
function(collection_id) {
  collection_id <- URLdecode(collection_id)
  get_collections(api, collection_id)
}

#* HTTP Basic authentication
#* @get /credentials/basic
function() {
  # TODO: implement token generator based on authentication
  token = "b34ba2bdf9ac9ee1"
  list(access_token = token)
}

#* Process and download data synchronously
#* @post /result
function(req, res) {
  p <- req$body
  result <- run_pgraph(api, p)
  # TODO Have to create a result_class to test the result?
  if (!"format" %in% names(result)) {
    res$status <- 200
    res$body <- list()
    res$contentType <- "application/json"
  } else {
    # TODO serialize according to format of the save_result process graph
    result # list(data=..., format=...)
    switch(result$format,
           "GTiff" = {},
           "RDS" = {},
           stop("Format '", result$format, "' is not supported.")
    )
  }
}

#* Lists api processes
#* @serializer unboxedJSON
#* @get /processes
function() {
  get_processes(api)
}
