#* @apiTitle openEO API
#* @apiVersion 1.2.0

api_version <- "1.2.0"

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
#* @serializer unboxedJSON
#* @post /result
function(req, res) {
  p <- req$body
  run_pgraph(api, p)
}

#* Lists api processes
#* @get /processes
function() {
  list_processes(api)
}
