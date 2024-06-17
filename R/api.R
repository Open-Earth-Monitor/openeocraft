#' Handle API requests
#'
#' These are functions responsible for handling requests of the
#' API endpoint. It interfaces HTTP requests from `plumber` and uses the
#' `api` and the `req` objects to prepare a response to the request by
#' dispatching to specific API implementations. HTTP input parameters are
#' parsed internally.
#'
#' \itemize{
#'
#' \item `create_api`: Creates an API object. It allows users setup
#'   custom API classes to create response documents.
#'
#' \item `create_openeo_v1`: Creates an API object for openEO v1.2 API Features.
#'
#' \item `setup_plumber`: Register the Plumber router in the API server.
#'   It also can enable the Plumber documentation and set the handler
#'   of errors in the API.
#'
#' \item `api_spec`: Generates the OpenAPI specification for the API server.
#'
#' }
#'
#' @param api_class A character string specifying the custom S3 class
#'   of the API. It allows advanced users setup new classes to handle
#'   response documents. Currently, `openeocraft` supports `oafeat` and
#'   `stac` S3 classes. To implement a new set of response document
#'   handlers, users must implement for their new class all generic
#'   functions declared in `R/doc.R`. For more details, see the
#'   `github` page of the project.
#'
#' @param id A character string specifying the id of the API.
#'
#' @param title A character string specifying the title of the API.
#'
#' @param description A character string describing the API.
#'
#' @param conforms_to A character vector specifying the conformance
#'   standards adhered to by the API. This parameter can be NULL or
#'   contain additional conformance standards to add to the defaults.
#'
#' @param pr The Plumber router object to be associated with the API server.
#'   For annotated API definition, users can capture the current Plumber
#'   object by annotating `@plumber` keyword in comment block. See
#'   references below for more details.
#'
#' @param spec_endpoint The endpoint where the API specification
#'   (OpenAPI) will be available. An `NULL` value disable this feature.
#'
#' @param docs_endpoint The endpoint where the API documentation
#'   (swagger) will be available. An `NULL` value disable this feature.
#'
#' @param handle_errors A logical value indicating whether to handle
#'   errors using the `openeocraft` default error handler. Default is `TRUE`.
#'
#' @param api An object representing the API. This object is typically
#'   created using either the `create_stac` or `create_ogcapi`
#'
#' @param req The request object from the `plumber` package, containing
#'   information about the HTTP request made to the API endpoint.
#'
#' @param res The response object from the `plumber` package, used to
#'   construct and send the HTTP response back to the client making
#'   the request.
#'
#' @param collection_id The identifier of the collection. This parameter
#'   specifies which collection the request is targeting.
#'
#' @param ... Additional arguments to be passed to the method-specific
#'   functions.
#'
#' @return For API creation functions, returns a api object. For API
#'   handling functions, returns the document to return as response.
#'
#' @references
#' For more information about the STAC specification,
#' see: \url{https://stacspec.org/}
#'
#' For more information about the OGC API specification,
#' see: \url{http://www.opengis.net/doc/IS/ogcapi-features-1/1.0}
#'
#' For more information about annotated Plumber API definition, see:
#' \url{https://www.rplumber.io/articles/annotations.html}
#'
#' @name api_handling
#'
NULL
#' @rdname api_handling
#' @export
create_api <- function(api_class,
                       id,
                       title,
                       description,
                       backend_version,
                       api_version,
                       stac_api,
                       work_dir,
                       conforms_to,
                       production, ...) {
  structure(
    list(
      id = id,
      title = title,
      description = description,
      backend_version = backend_version,
      api_version = api_version,
      stac_api = stac_api,
      work_dir = work_dir,
      conforms_to = unique(as.list(conforms_to)),
      production = production, ...
    ),
    class = api_class,
    env = new.env(hash = TRUE, parent = parent.frame())
  )
}

#' @rdname api_handling
#' @export
create_openeo_v1 <- function(id,
                             title,
                             description,
                             backend_version,
                             stac_api,
                             work_dir,
                             conforms_to = NULL,
                             production = FALSE, ...) {
  # A list of all conformance classes specified in a standard that the
  # server conforms to.
  openeo_v1_conforms_to <- list(
    "https://api.openeo.org/1.2.0",
    "https://api.stacspec.org/v1.0.0/collections"
  )
  create_api(
    api_class = "openeo_v1",
    id = id,
    title = title,
    description = description,
    backend_version = backend_version,
    api_version = "1.2.0",
    stac_api = stac_api,
    work_dir = work_dir,
    conforms_to = c(openeo_v1_conforms_to, as.list(conforms_to)),
    production = production, ...
  )
}

#' @rdname api_handling
#' @export
api_setup_plumber <- function(api,
                              pr, ...,
                              handle_errors = TRUE,
                              api_base_url = NULL,
                              spec_endpoint = "/api",
                              docs_endpoint = "/docs",
                              wellknown_versions = list()) {
  stopifnot(is_absolute_url(api_base_url))
  # TODO: replace all calls to `api_attr()` by proper functions
  api_attr(api, "plumber") <- pr
  api_attr(api, "api_base_url") <- api_base_url
  set_wellknown_versions(api, wellknown_versions)
  setup_endpoints(api, pr)
  # set error handling
  if (handle_errors)
    plumber::pr_set_error(pr, api_error_handler)
  # setup /api and /docs endpoints
  if (!is.null(spec_endpoint)) {
    setup_plumber_spec(api, pr, spec_endpoint)
    if (!is.null(docs_endpoint))
      setup_plumber_docs(api, pr, docs_endpoint, spec_endpoint)
  }
}

#' @rdname api_handling
#' @export
api_credential <- function(api, req, res) {
  auth <- gsub("Basic ", "",req$HTTP_AUTHORIZATION)
  auth <- rawToChar(base64enc::base64decode(auth))
  auth <- strsplit(auth, ":")[[1]]
  user <- auth[[1]]
  password <- auth[[2]]
  file <- api_attr(api, "credentials")
  credentials <- readRDS(file)
  print(user)
  print(password)
  print(credentials)
  if (!user %in% names(credentials$users) ||
      credentials$users[[user]]$password != password) {
    api_stop(403, "User or password does not match")
  }
  # user is logged
  if (!"token" %in% names(credentials$users[[user]])) {
    credentials <- new_token(credentials, user, 30)
    saveRDS(credentials, file)
  } else if (credentials$users[[user]]$expiry > Sys.time()) {
    old_token <- credentials$users[[user]]$token
    credentials$tokens[[old_token]] <- NULL
    credentials <- new_token(credentials, user, 30)
    saveRDS(credentials, file)
  }
  list(access_token = credentials$users[[user]]$token)
}

#' @rdname api_handling
#' @export
api_result <- function(api, req, res) {
  token <- gsub("^.*//", "", req$HTTP_AUTHORIZATION)
  user <- token_user(api, token)
  pg <- req$body
  result <- run_pgraph(api, user, pg)
  api_serializer(result, res)
}

api_serializer <- function(x, res) {
  UseMethod("api_serializer", x)
}
#' @export
api_serializer.openeo_json <- function(x, res) {
  res$setHeader("Content-Type", "application/json")
  res$body <- x$data
  res
}
#' @export
api_serializer.openeo_gtiff <- function(x, res) {
  res$setHeader("Content-Type", "image/tiff")
  res$body <- readBin(x$data, what = "raw", n = file.info(x$data)$size)
  res
}
#' @export
api_serializer.openeo_netcdf <- function(x, res) {
  res$setHeader("Content-Type", "application/octet-stream")
  res$body <- readBin(x$data, what = "raw", n = file.info(x$data)$size)
  res
}

#' @export
api_serializer.openeo_rds <- function(x, res) {
  res$setHeader("Content-Type", "application/rds")
  res$body <- readBin(x$data, what = "raw", n = file.info(x$data)$size)
  res
}
#' @export
api_wellknown <- function(api, req) {
  doc_wellknown(api, req)
}
#' @export
api_landing_page <- function(api, req) {
  doc_landing_page(api, req)
}
#' @export
api_conformance <- function(api, req) {
  doc_conformance(api, req)
}
#' @export
api_processes <- function(api, req, check_auth = FALSE) {
  if (check_auth) {
    token <- req$header$token
    token_user(api, token)
  }
  doc_processes(api, req)
}
#' @export
api_jobs_list <- function(api, req) {
  token <- req$header$token
  user <- token_user(api, token)
  job_list_all(api, user)
}
#' @export
api_user_workspace <- function() {
  env <- parent.frame(2)
  api_stopifnot(exists("api", env) && exists("user", env), status = 500,
                "api and user objects not defined in the evaluation scope")
  user_workspace(env$api, env$user)
}

#' Get Supported File Formats
#'
#' This function returns a list of supported input and output file formats
#' for GIS data. Each format includes details such as title, description,
#' GIS data types, and parameters.
#'
#' @return A list containing two elements:
#' \describe{
#'   \item{input}{A list of supported input formats.}
#'   \item{output}{A list of supported output formats.}
#' }
#' @export
file_formats <- function() {
  # Define the output formats
  outputFormats <- list(
    GTiff = list(
      title = "GeoTiff",
      description = "Export to GeoTiff.",
      gis_data_types = list("raster"),
      parameters = list(
        format = list(
          type = "string",
          description = "GeoTiff"
        )
      )
    ),
    NetCDF = list(
      title = "Network Common Data Form",
      description = "Export to NetCDF.",
      gis_data_types = list("raster"),
      parameters = list(
        format = list(
          type = "string",
          description = "NetCDF"
        )
      )
    ),
    RDS = list(
      title = "R Data Serialization",
      description = "Export to RDS.",
      gis_data_types = list("raster"),
      parameters = list(
        format = list(
          type = "string",
          description = "RDS"
        )
      )
    ),
    JSON = list(
      title = "JSON Data Serialization",
      description = "Export to JSON.",
      gis_data_types = list("raster"),
      parameters = list(
        format = list(
          type = "string",
          description = "JSON"
        )
      )
    )
  )

  # Define the input formats
  inputFormats <- list(
    GTiff = list(
      title = "GeoTiff",
      description = "Geotiff is one of the most widely supported formats. This backend allows reading from Geotiff to create raster data cubes.",
      gis_data_types = list("raster"),
      parameters = list(
        format = list(
          type = "string",
          description = "GeoTiff"
        )
      )
    )
  )

  # return the list of supported formats
  list(
    input = inputFormats,
    output = outputFormats
  )
}

#' @export
format_ext <- function(format) {
  switch(format,
    gtiff = ".tif",
    netcdf = ".nc",
    rds = ".rds",
    json = ".json"
  )
}

# TODO:
# - Supported UDF runtimes `GET /udf_runtime`
# - Supported secondary web service protocols `GET /service_types`
# - OpenID Connect authentication `GET /credentials/oidc`
# - HTTP Basic authentication `GET /credentials/basic`
# - Information about the authenticated user `GET /me`
# - (openstac) Metadata filters for a specific dataset `GET /collections/{collection_id}/queryables`
# - List all user-defined processes `GET /process_graphs`
# - Full metadata for a user-defined process `GET /process_graphs/{process_graph_id}`
# - Validate a user-defined process (graph) `POST /validation`
# - Store a user-defined process `PUT /process_graphs/{process_graph_id}`
# - Delete a user-defined process `DELETE /process_graphs/{process_graph_id}`
