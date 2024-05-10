#* @apiTitle openEO API
#* @apiDescription
#*   Spatio-Temporal Asset Catalog for global layers provided and maintained
#*   by Open Earth Monitor Cyber-infrastructure (OEMC) consortium.
#* @apiVersion 1.2.0
#* @apiBasePath /

# Load libraries
library(openeocraft)
library(plumber)
library(promises)
library(future)

# Set number of processes to serve the API
future::plan(future::multisession(workers = 2))

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
# stac_api <- openstac::set_db(stac_api, driver = "local", file = "openlandmap.rds")

# Create openEO API object
api <- create_openeo_v1(
  id = "openeo-sits",
  title = "openEO backend for sits",
  description = "This is an openEO compliant R backend for sits package.",
  backend_version = "0.2.0",
  stac_api = stac_api,
  conforms_to = NULL,
  production = FALSE
)

set_credentials(api, file = "~/openeo-credentials.rds")
new_credential(api, user = "rolf", password = "123456")
new_credential(api, user = "brian", password = "123456")

# Load processes
processes_file <- system.file("sits/processes.R", package = "openeocraft")
load_processes(api, processes_file)

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

#* Enable Cross-origin Resource Sharing
#* @filter cors
function(req, res) {
  api_cors_handler(req, res, origin = "*", methods = "*")
}

#* Information about the back-end
#* @serializer unboxedJSON
#* @get /.well-known/openeo
function(req, res) {
  doc_wellknown(api, req)
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
  doc_conformance(api, req)
}

#* Information about the back-end
#* @get /
function(req, res) {
  doc_landing_page(api, req)
}

#* Basic metadata for all datasets
#* @serializer unboxedJSON
#* @get /collections
function(req, res) {
  openstac::api_collections(api$stac_api, req, res)
}

#* Full metadata for a specific dataset
#* @param collection_id Collection identifier
#* @serializer unboxedJSON
#* @get /collections/<collection_id>
function(req, res, collection_id) {
  collection_id <- URLdecode(collection_id)
  openstac::api_collection(api$stac_api, req, res, collection_id)
}

#* Lists api processes
#* @serializer unboxedJSON
#* @get /processes
function(req, res) {
  doc_processes(api, req)
}

#* Process and download data synchronously
#* @post /result
function(req, res) {
  api_result(api, req, res)
}

#* List all batch jobs
#* @get /jobs
function(req, res) {
  api_jobs(api, req, res)
}

#* Create a new batch job
#* @post /jobs
function(req, res) {
  api_jobs(api, req, res)
}

#* Modify a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @patch /jobs/<job_id>
function(req, res, job_id) {
  job_id <- URLdecode(job_id)
  api_jobs(api, req, res, job_id)
}

#* Full metadata for a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @get /jobs/<job_id>
function(req, res, job_id) {
  job_id <- URLdecode(job_id)
  api_jobs(api, req, res, job_id)
}

#* Delete a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @delete /jobs/<job_id>
function(req, res, job_id) {
  job_id <- URLdecode(job_id)
  api_jobs(api, req, res, job_id)
}

#* Get an estimate for a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @get /jobs/<job_id>/estimate
function(req, res, job_id) {
  job_id <- URLdecode(job_id)
  api_jobs(api, req, res, job_id, subroute = "estimate")
}

#* Logs for a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @get /jobs/<job_id>/logs
function(req, res, job_id) {
  job_id <- URLdecode(job_id)
  api_jobs(api, req, res, job_id, subroute = "logs")
}

#* List batch job results
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @get /jobs/<job_id>/results
function(req, res, job_id) {
  job_id <- URLdecode(job_id)
  api_jobs(api, req, res, job_id, subroute = "results")
}

#* Start processing a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @post /jobs/<job_id>/results
function(req, res, job_id) {
  job_id <- URLdecode(job_id)
  api_jobs(api, req, res, job_id, subroute = "results")
}

#* Cancel processing a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @delete /jobs/<job_id>/results
function(req, res, job_id) {
  job_id <- URLdecode(job_id)
  api_jobs(api, req, res, job_id, subroute = "results")
}
