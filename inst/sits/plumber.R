#* @apiTitle openEO API
#* @apiDescription
#*   Spatio-Temporal Asset Catalog for global layers provided and maintained
#*   by Open Earth Monitor Cyber-infrastructure (OEMC) consortium.
#* @apiVersion 1.2.0
#* @apiBasePath /

# Load libraries
library(openeocraft)
library(plumber)
#library(promises)
#library(future)

# Set number of processes to serve the API
#future::plan(future::multisession(workers = 2))

# Create an STAC server API object
stac_api <- openstac::create_stac(
  id = "openlandmap",
  title = "OpenLandMap STAC API",
  description = paste(
    "Spatio-Temporal Asset Catalog for global layers provided ",
    "by OpenLandMap and maintaned by OpenGeoHub Foundation"
  ),
  conforms_to = NULL
)

# Set API database
file <- system.file("sits/sits.rds", package = "openeocraft")
stac_api <- openstac::set_db(stac_api, driver = "local", file = file)

# Create openEO API object
api <- create_openeo_v1(
  id = "openeo-sits",
  title = "openEO backend for sits",
  description = "This is an openEO compliant R backend for sits package.",
  backend_version = "0.2.0",
  stac_api = stac_api,
  work_dir = "~/openeo-tests",
  conforms_to = NULL,
  production = FALSE
)

set_credentials(api, file = "~/openeo-credentials.rds")
new_credential(api, user = "rolf", password = "123456")
new_credential(api, user = "brian", password = "123456")

# Load processes
processes_file <- system.file("sits/processes.R", package = "openeocraft")
load_processes(api, processes_file)

#* Enable Cross-origin Resource Sharing
#* @filter cors
function(req, res) {
  api_cors_handler(req, res, origin = "*", methods = "*")
}

#* HTTP Basic authentication
#* @get /credentials/basic
function(req, res) {
  api_credential(api, req, res)
}

#* Lists api processes
#* @serializer unboxedJSON
#* @get /conformance
function(req, res) {
  print("conformance")
  api_conformance(api, req)
}

#* Basic metadata for all datasets
#* @serializer unboxedJSON
#* @get /collections
function(req, res) {
  print("collections")
  openstac::api_collections(api$stac_api, req)
}

#* Full metadata for a specific dataset
#* @param collection_id Collection identifier
#* @serializer unboxedJSON
#* @get /collections/<collection_id>
function(req, res, collection_id) {
  print("collections/<col_id>")
  doc <- openstac::api_collection(api$stac_api, collection_id, req)
  delete_link(doc, rel = "item")
}

#* Lists api processes
#* @serializer unboxedJSON
#* @get /processes
function(req, res) {
  print("processes")
  api_processes(api, req, check_auth = FALSE)
}

#* Process and download data synchronously
#* @post /result
function(req, res) {
  print("result")
  api_result(api, req, res)
}

#* List all batch jobs
#* @serializer unboxedJSON
#* @get /jobs
function(req, res) {
  print("GET /jobs")
  api_jobs_list(api, req)
}

#* Get batch job metadata
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @get /jobs/<job_id:str>
function(req, res, job_id) {
  print("GET /jobs/<jobid>")
  token <- gsub("^.*//", "", req$HTTP_AUTHORIZATION)
  user <- token_user(api, token)
  job_info(api, user, job_id)
}

#* Create a new batch job
#* @serializer unboxedJSON
#* @post /jobs
function(req, res) {
  print("POST /jobs")
  token <- gsub("^.*//", "", req$HTTP_AUTHORIZATION)
  user <- token_user(api, token)
  job <- req$body
  job_create(api, req, res, user, job)
}

#* Delete a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @delete /jobs/<job_id:str>
function(req, res, job_id) {
  print("DELETE /jobs/<jobid>")
  token <- gsub("^.*//", "", req$HTTP_AUTHORIZATION)
  user <- token_user(api, token)
  job_delete(api, user, job_id)
}

#* Update a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @patch /jobs/<job_id:str>
function(req, res, job_id) {
  print("PATCH /jobs/<jobid>")
  token <- gsub("^.*//", "", req$HTTP_AUTHORIZATION)
  user <- token_user(api, token)
  job <- req$body
  job_update(api, user, job_id, job)
}

#* Start a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @post /jobs/<job_id:str>/results
function(req, res, job_id) {
  print("POST /jobs/<jobid>/results")
  token <- gsub("^.*//", "", req$HTTP_AUTHORIZATION)
  user <- token_user(api, token)
  job_start(api, req, res, user, job_id)
}

#* Start a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @get /jobs/<job_id:str>/results
function(req, res, job_id) {
  print("GET /jobs/<jobid>/results")
  token <- gsub("^.*//", "", req$HTTP_AUTHORIZATION)
  user <- token_user(api, token)
  job_get_results(api, user, job_id)
}

#* Get an estimate for a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @get /jobs/<job_id>/estimate
function(req, res, job_id) {
  print("GET /jobs/<jobid>/estimate")
  token <- gsub("^.*//", "", req$HTTP_AUTHORIZATION)
  user <- token_user(api, token)
  job_id <- URLdecode(job_id)
  job_estimate(api, user, job_id)
}

#* Logs for a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @get /jobs/<job_id>/logs
function(req, res, job_id, offset, level, limit) {
  print("GET /jobs/<jobid>/logs")
  token <- gsub("^.*//", "", req$HTTP_AUTHORIZATION)
  user <- token_user(api, token)
  job_logs(api, user, job_id, offset, level, limit)
}

#* List supported file formats
#* @serializer unboxedJSON
#* @get /file_formats
function(req, res) {
  print("file_formats")
  file_formats()
}

# NOTE:
#  this must be placed after endpoints to be shown in
#  the land page, so that endpoints can be mapped properly
#  endpoints registered after this setup will not be listed.

#* Setup plumber router
#* @plumber
function(pr) {
  api_setup_plumber(
    api = api,
    pr = pr,
    handle_errors = TRUE,
    spec_endpoint = "/api",
    docs_endpoint = "/docs",
    wellknown_versions = list()
  )
}

#* Information about the back-end
#* @serializer unboxedJSON
#* @get /
function(req, res) {
  api_landing_page(api, req)
}

#* Information about the back-end
#* @serializer unboxedJSON
#* @get /.well-known/openeo
function(req, res) {
  api_wellknown(api, req)
}

#* Workspace files handling
#* @get /files/jobs/<job_id>/<asset>
function(req, res, job_id, asset) {
  file <- gsub("^([^?]+)?", "\\1", asset)
  if (!"token" %in% names(req$args)) {
    api_stop(401, "URL token parameter is missing")
  }
  token <- req$args$token
  user <- rawToChar(base64enc::base64decode(token))
  path <- file.path(api_user_workspace(api, user), "jobs", job_id, file)
  api_stopifnot(file.exists(path), status = 404,
                "File not found")
  res$setHeader("Content-Type", ext_content_type(path))
  res$body <- readBin(path, what = "raw", n = file.info(path)$size)
}
