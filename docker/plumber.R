#* @apiTitle openEO API
#* @apiDescription
#*   Spatio-Temporal Asset Catalog for global layers provided and maintained
#*   by Open Earth Monitor Cyber-infrastructure (OEMC) consortium.
#* @apiVersion 1.2.0
#* @apiBasePath /

# Load libraries
library(openeocraft)
library(plumber)

# Increase request body size limit (100MB) to handle large payloads
# such as serialized training datasets in process graphs
options(plumber.maxRequestSize = 1024 * 1024 * 100)

# Increase curl/httr timeouts for large data transfers
options(timeout = 600) # 10 minutes for base R connections
if (requireNamespace("httr", quietly = TRUE)) {
  httr::set_config(httr::config(connecttimeout = 60, timeout = 600))
}
# library(promises)
# library(coro)

# Set number of processes to serve the API
# future::plan("multisession")

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
file <- system.file("ml/db.rds", package = "openeocraft")
stac_api <- openstac::set_db(stac_api, driver = "local", file = file)

# Create openEO API object
api <- create_openeo_v1(
  id = "openeocraft",
  title = "openEO compliant R backend",
  description = "OpenEOcraft offers a robust R framework designed for the development and deployment of openEO API applications.",
  backend_version = "0.2.0",
  stac_api = stac_api,
  work_dir = "~/openeo-tests",
  conforms_to = NULL,
  production = FALSE
)

# set_credentials(api, file = "~/openeo-tests/openeo-credentials.rds")
set_credentials(api, file = "~/openeo-credentials.rds")
new_credential(api, user = "rolf", password = "123456")
new_credential(api, user = "brian", password = "123456")
new_credential(api, user = "user", password = "password")

# Load processes
# When running locally (Rscript docker/server.R), load from source for easier development
# When running in Docker, load from installed package

# Try multiple paths to find the source file
local_paths <- c(
  "inst/ml/processes.R", # From repo root
  "../inst/ml/processes.R" # From docker/ directory
)

processes_file <- NULL
for (path in local_paths) {
  if (file.exists(path)) {
    processes_file <- path
    cat("Loading processes from source:", normalizePath(path), "\n")
    break
  }
}

# Fallback to installed package
if (is.null(processes_file)) {
  processes_file <- system.file("ml/processes.R", package = "openeocraft")
  cat("Loading processes from package:", processes_file, "\n")
}

load_processes(api, processes_file)

#* Enable Cross-origin Resource Sharing
#* @filter cors
function(req, res) {
  cat("CORS filter:", req$REQUEST_METHOD, req$PATH_INFO, fill = TRUE)
  api_cors_handler(req, res, origin = "*", methods = "*")
}

#* HTTP Basic authentication
#* @serializer unboxedJSON
#* @get /credentials/basic
function(req, res) {
  print("GET /credentials/basic")
  api_credential(api, req, res)
}

#* Lists api processes
#* @serializer unboxedJSON
#* @get /conformance
function(req, res) {
  print("GET /conformance")
  api_conformance(api, req, res)
}

#* Basic metadata for all datasets
#* @serializer unboxedJSON
#* @get /collections
function(req, res) {
  print("GET /collections")
  openstac::api_collections(api$stac_api, req, res)
}

#* Full metadata for a specific dataset
#* @param collection_id Collection identifier
#* @serializer unboxedJSON
#* @get /collections/<collection_id>
function(req, res, collection_id) {
  print("GET /collections/<col_id>")
  doc <- openstac::api_collection(api$stac_api, req, res, collection_id)
  doc <- delete_link(doc, rel = "item")
  doc
}

#* Lists api processes
#* @serializer unboxedJSON
#* @get /processes
function(req, res) {
  print("GET /processes")
  doc <- api_processes(api, req, res, check_auth = FALSE)
  doc
}

#* Process and download data synchronously
#* @post /result
function(req, res) {
  print("POST /result")
  doc <- api_result(api, req, res)
  doc
}

#* List all batch jobs
#* @serializer unboxedJSON
#* @get /jobs
function(req, res) {
  print("GET /jobs")
  doc <- api_jobs_list(api, req, res)
  doc
}

#* Get batch job metadata
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @get /jobs/<job_id:str>
function(req, res, job_id) {
  print("GET /jobs/<jobid>")
  doc <- api_job_info(api, req, res, job_id)
  doc
}

#* Create a new batch job
#* @serializer unboxedJSON
#* @post /jobs
function(req, res) {
  print("POST /jobs")
  # print(req$body$process)
  api_job_create(api, req, res)
}

#* Delete a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @delete /jobs/<job_id:str>
function(req, res, job_id) {
  print("DELETE /jobs/<jobid>")
  token <- get_token(req)
  user <- get_token_user(api, token)
  job_delete(api, user, job_id)
}

#* Update a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @patch /jobs/<job_id:str>
function(req, res, job_id) {
  print("PATCH /jobs/<jobid>")
  token <- get_token(req)
  user <- get_token_user(api, token)
  job <- req$body
  job_update(api, user, job_id, job)
}

#* Start a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @post /jobs/<job_id:str>/results
function(req, res, job_id) {
  print("POST /jobs/<jobid>/results")
  api_job_start(api, req, res, job_id)
}

#* Lists batch job results
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @get /jobs/<job_id:str>/results
function(req, res, job_id) {
  print("GET /jobs/<jobid>/results")
  token <- get_token(req)
  user <- get_token_user(api, token)
  job_get_results(api, user, job_id)
}

#* Get an estimate for a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @get /jobs/<job_id>/estimate
function(req, res, job_id) {
  print("GET /jobs/<jobid>/estimate")
  token <- get_token(req)
  user <- get_token_user(api, token)
  job_estimate(api, user, job_id)
}

#* Logs for a batch job
#* @param job_id job identifier
#* @serializer unboxedJSON
#* @get /jobs/<job_id>/logs
function(req,
         res,
         job_id,
         offset = 0,
         level = "info",
         limit = 10) {
  print("GET /jobs/<jobid>/logs")
  token <- get_token(req)
  user <- get_token_user(api, token)
  job_logs(api, user, job_id, offset, level, limit)
}

#* List supported file formats
#* @serializer unboxedJSON
#* @get /file_formats
function(req, res) {
  print("GET /file_formats")
  doc <- api_file_formats(api, req, res)
  doc
}

# NOTE:
#  this must be placed after endpoints to be shown in
#  the land page, so that endpoints can be mapped properly
#  endpoints registered after this setup will not be listed.

#* Setup plumber router
#* @plumber
function(pr) {
  print("Setting up server...")
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
  print("GET /")
  api_landing_page(api, req, res)
}

#* Information about the back-end
#* @serializer unboxedJSON
#* @get /.well-known/openeo
function(req, res) {
  print("GET /.well-known/openeo")
  api_wellknown(api, req, res)
}

#* Workspace job files handling
#* @head /files/jobs/<job_id>/<asset>
#* @get /files/jobs/<job_id>/<asset>
function(req, res, job_id, asset) {
  print("GET /files/jobs/<jobid>/<asset>")
  file <- gsub("^([^?]+)?", "\\1", asset)
  if (!"token" %in% names(req$args)) {
    api_stop(401L, "URL token parameter is missing")
  }
  token <- req$args$token
  user <- rawToChar(base64enc::base64decode(token))
  path <- file.path(api_user_workspace(api, user), "jobs", job_id, file)
  if (!file.exists(path)) {
    api_stop(404L, "File not found")
  }
  res$setHeader("Content-Type", ext_content_type(path))
  if (get_method(req) == "HEAD") {
    res$status <- 200L
  } else {
    res$body <- readBin(path, what = "raw", n = file.info(path)$size)
  }
  res
}

#* Workspace root files handling
#* @head /files/root/<folder>/<asset>
#* @get /files/root/<folder>/<asset>
function(req, res, folder, asset) {
  print("GET /files/root/<folder>/<asset>")
  file <- gsub("^([^?]+)?", "\\1", asset)
  if (!"token" %in% names(req$args)) {
    api_stop(401L, "URL token parameter is missing")
  }
  token <- req$args$token
  user <- rawToChar(base64enc::base64decode(token))
  path <- file.path(api_user_workspace(api, user), "root", folder, file)
  if (!file.exists(path)) {
    api_stop(404L, "File not found")
  }
  res$setHeader("Content-Type", ext_content_type(path))
  if (get_method(req) == "HEAD") {
    res$status <- 200L
  } else {
    res$body <- readBin(path, what = "raw", n = file.info(path)$size)
  }
  res
}

#* Public workspace files handling (no token)
#*  e.g. http://127.0.0.1:8000/files/public/<job_id>/models/tempcnn_rondonia.rds
#* @head /files/public/<folder>/<asset>
#* @get /files/public/<folder>/<asset>
function(req, res, folder, asset) {
  print("GET /files/public/<folder>/<asset>")
  file <- gsub("^([^?]+)?", "\\1", asset)
  public_dir <- openeocraft::api_user_workspace(api, "public")
  path <- file.path(public_dir, folder, file)
  if (!file.exists(path)) {
    api_stop(404L, "File not found")
  }
  res$setHeader("Content-Type", ext_content_type(path))
  if (get_method(req) == "HEAD") {
    res$status <- 200L
  } else {
    res$body <- readBin(path, what = "raw", n = file.info(path)$size)
  }
  res
}
