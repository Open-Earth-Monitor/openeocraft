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
function(req, res) {
  auth <- gsub("Basic ", "",req$HTTP_AUTHORIZATION)
  auth <- rawToChar(base64decode(auth))
  # print(auth) # "rolf:123456"
  # TODO: implement token generator based on authentication
  token = "b34ba2bdf9ac9ee1"
  list(access_token = token)
}

#* Process and download data synchronously
#* @serializer serialize_result
#* @post /result
function(req, res) {
  p <- req$body
  if ("process" %in% names(p))
    p <- p$process
  run_pgraph(api, p)
}

#* Lists api processes
#* @serializer unboxedJSON
#* @get /processes
function() {
  get_processes(api)
}
