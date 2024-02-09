library(openeocraft)

# Create API object
api <- create_api(
  id = "openeo-sits",
  title = "openEO backend for sits",
  description = "This is an openEO compliant R backend for sits package.",
  backend_version = "0.2.0",
  api_version = "1.2.0"
)

# Load processes
processes_file <- system.file("sits/processes.R", package = "openeocraft")
load_processes(api, processes_file)

# Load collections
collections <- jsonlite::read_json(
  path = system.file("sits/collections.json", package = "openeocraft"),
  auto_unbox = TRUE
)
load_collections(api, collections = collections)

####################################


#* @apiTitle openEO API
#* @apiVersion 1.2.0

#* Information about the back-end
#* @serializer unboxedJSON
#* @get /.well-known/openeo
function(req, res) {
  api_wellknown(api, req, res)
}

#* Information about the back-end
#* @get /
function(req, res) {
  api_landing_page(api, req, res)
}

#* Basic metadata for all datasets
#* @serializer unboxedJSON
#* @get /collections
function(req, res) {
  api_collections(api, req, res)
}

#* Full metadata for a specific dataset
#* @param collection_id Collection identifier
#* @serializer unboxedJSON
#* @get /collections/<collection_id>
function(req, res, collection_id) {
  collection_id <- URLdecode(collection_id)
  api_collection(api, req, res, collection_id)
}

#* HTTP Basic authentication
#* @get /credentials/basic
function(req, res) {
  api_credential(api, req, res)
}

#* Lists api processes
#* @serializer unboxedJSON
#* @get /processes
function(req, res) {
  api_processes(api, req, res)
}

#* Process and download data synchronously
#* @serializer serialize_result
#* @post /result
function(req, res) {
  api_result(api, req, res)
}

#* Register the endpoints in the api
#* @plumber
function(pr) {
  api_reg_endpoints(api, pr)
}
