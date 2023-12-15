#* @apiTitle openEO API
#* @apiVersion 1.2.0

api_version = "1.2.0"

#* Information about the back-end
#* @get /
function() {
  # TODO: include this metadata into a config or init function
  list(
    api_version = api_version,
    backend_version = "0.1.0",
    stac_version = "1.0.0",
    type = "Catalog",
    id = "sits-openeo",
    title = "SITS implementation on openEO",
    description = "This is the implmentation of SITS package on openEO framework.",
    production = FALSE,
    endpoints = list_endpoints(),
    links = list(
      # TODO: list other types of links
      new_link(
        href = get_endpoint("/"),
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
  get_collections()
}

#* Full metadata for a specific dataset
#* @param collection_id Collection identifier
#* @serializer unboxedJSON
#* @get /collections/<collection_id>
function(collection_id) {
  collection_id <- URLdecode(collection_id)
  get_collections(collection_id)
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
  p <- req$body$process
  run_pgraph(p)
}
