#' API helper functions
#'
#' This set of functions provides HTTP CORS (Cross-Origin Resource Sharing)
#' support and error handling for the API.
#'
#' \itemize{
#'
#' \item `api_cors_handler`: HTTP CORS support. Typically called from a
#'   `plumber` filter to manage CORS.
#'
#' \item `api_error_handler`: Error handling function to be provided to
#'   `plumber` router to construct HTTP errors in a standardized way.
#'   Use `plumber::pr_set_error()` function to set the error handler
#'   in `plumber`.
#'
#' \item `api_stop`: Throws an error and set the HTTP status code and
#'   error message to be returned to the user by the `api_error_handler`
#'   function.
#'
#' \item `api_stopifnot`: Throws an error if the provided expression
#'   is evaluated as `FALSE`.
#'
#' \item `get_host`: Get the API host address from an `req` object.
#'
#' \item `get_path`: Get the path from an `req` object.
#'
#' \item `get_method`: Get the HTTP method from an `req` object.
#'
#' }
#'
#' @param req The request object from the `plumber` package, containing
#'   information about the HTTP request made to the API endpoint.
#'
#' @param res The response object from the `plumber` package, used to
#'   construct and send the HTTP response back to the client making
#'   the request.
#'
#' @param origin The value to set for the 'Access-Control-Allow-Origin'
#'   header in CORS requests. Defaults to '*'.
#'
#' @param methods The value to set for the 'Access-Control-Allow-Methods'
#'   header in CORS requests. Defaults to '*'.
#'
#' @param err The error object containing information about the
#'   encountered error. If the error is thrown by `api_stopifnot` or
#'   `api_stop` functions, this object has 'status' and 'message' fields
#'   that are used to produce the HTTP response error.
#'
#' @param status The HTTP status code to set for the response. This just
#'   works if the `api_error_handler` function is handling errors in
#'   `plumber`.
#'
#' @param expr The expression to evaluate. If the expression evaluates
#'   to FALSE, an error will be raised.
#'
#' @param ... Additional arguments to be passed to error handling functions.
#'
#' @seealso
#' [plumber::pr_set_error()]: Function to set error handler in `plumber`.
#'
#' @references
#' The code for `api_cors_handler` was based on a issue discussion post
#' on CORS support in plumber at
#' `https://github.com/rstudio/plumber/issues/66#issuecomment-418660334`
#'
#' @name api_helpers
NULL
#' @rdname api_helpers
#' @export
api_cors_handler <- function(req, res, origin = "*", methods = "*") {
  res$setHeader("Access-Control-Allow-Origin", origin)
  if (req$REQUEST_METHOD != "OPTIONS") {
    plumber::forward()
  } else {
    res$setHeader("Access-Control-Allow-Methods", methods)
    res$setHeader("Access-Control-Allow-Headers",
                  req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  }
}
#' @rdname api_helpers
#' @export
api_error_handler <- function(req, res, err) {
  if (is.null(err$status)) err$status <- 500
  if (is.null(err$message)) err$message <- "Internal server error"
  res$status <- err$status
  list(code = err$status, message = paste("Error:", err$message))
}
#' @rdname api_helpers
#' @export
api_stop <- function(status, ...) {
  stop(errorCondition(paste0(...), status = status))
}
#' @rdname api_helpers
#' @export
api_stopifnot <- function(expr, status, ...) {
  message <- paste0(...)
  if (!nzchar(message))
    message <- paste(deparse(substitute(expr)), "is not TRUE")
  if (!expr) api_stop(status, message)
}
#' @rdname api_helpers
#' @export
api_success <- function(status, ...) {
  list(code = status, message = paste0(...))
}
#' @rdname api_helpers
#' @export
get_host <- function(api, req) {
  host <- api_attr(api, "api_base_url")
  if (!is.null(host))
    return(host)
  if ("HTTP_HOST" %in% names(req))
    return(paste0(req$rook.url_scheme, "://", req$HTTP_HOST))
  host <- paste0(req$rook.url_scheme, "://", req$SERVER_NAME)
  if (!is.null(req$SERVER_PORT) && nzchar(req$SERVER_PORT) &&
      req$SERVER_PORT != "80")
    host <- paste0(host, ":", req$SERVER_PORT)
  host
}
#' @rdname api_helpers
#' @export
get_path <- function(req) {
  req$PATH_INFO
}
#' @rdname api_helpers
#' @export
get_method <- function(req) {
  req$REQUEST_METHOD
}
#' @keywords internal
api_env <- function(api) {
  attr(api, "env")
}
#' @keywords internal
api_attr <- function(api, name) {
  if (exists(name, envir = api_env(api), inherits = FALSE))
    get(name, envir = api_env(api), inherits = FALSE)
}
#' @keywords internal
`api_attr<-` <- function(api, name, value) {
  assign(name, value, envir = api_env(api), inherits = FALSE)
  api
}
get_endpoints <- function(api) {
  api_attr(api, "endpoints")
}
setup_endpoints <- function(api, pr) {
  # list all endpoints
  endpoints <- lapply(pr$endpoints$`__no-preempt__`, function(x) {
    list(path = format_endpoint(x$path), methods = list(x$verbs))
  })
  api_attr(api, "endpoints") <- endpoints
  api
}
get_wellknown_versions <- function(api) {
  api_attr(api, "wellknown_versions")
}
set_wellknown_versions <- function(api, wellknown_versions) {
  api_attr(api, "wellknown_versions") <- wellknown_versions
  api
}
#' @keywords internal
get_plumber <- function(api) {
  api_attr(api, "plumber")
}
#' @keywords internal
setup_plumber_spec <- function(api, pr, spec_endpoint) {
  spec_handler <- function(req, res, ...) {
    # TODO: add models
    utils::modifyList(
      list(servers = list(list(
        url = make_url(get_host(api, req))
      ))),
      pr$getApiSpec()
    )
  }
  api_attr(api, "spec_endpoint") <- spec_endpoint
  plumber::pr_set_docs(pr, FALSE)
  plumber::pr_get(
    pr = pr,
    path = spec_endpoint,
    handler = spec_handler,
    serializer = plumber::serializer_unboxed_json(),
    tag = "API"
  )
}
#' @keywords internal
setup_plumber_docs <- function(api, pr, docs_endpoint, spec_endpoint) {
  docs_handler <- function(req, res, ...) {
    swagger::swagger_spec(
      api_path = paste0(
        '"',
        make_url(get_host(api, req), spec_endpoint, ...),
        '"'
      )
    )
  }
  api_attr(api, "docs_endpoint") <- docs_endpoint
  plumber::pr_static(
    pr = pr,
    path = docs_endpoint,
    direc = swagger::swagger_path()
  )
  plumber::pr_get(
    pr = pr,
    path = paste0(docs_endpoint, "/index.html"),
    handler = docs_handler,
    serializer = plumber::serializer_html(),
    tag = "API"
  )
  plumber::pr_get(
    pr = pr,
    path = paste0(docs_endpoint, "/"),
    handler = docs_handler,
    serializer = plumber::serializer_html(),
    tag = "API"
  )
}

#' @export
new_credential <- function(api, user, password) {
  file <- api_attr(api, "credentials")
  stopifnot(!is.null(file) || file.exists(file))
  credentials <- readRDS(file)
  current_value <- list()
  if (user %in% names(credentials$users))
    current_value <- credentials$users[[user]]
  credentials$users[[user]] <- utils::modifyList(
    x = current_value,
    val = list(user = user, password = password)
  )
  saveRDS(credentials, file)
}

#' @export
set_credentials <- function(api, file) {
  if (!file.exists(file))
    saveRDS(list(users = list(), tokens = list()), file)
  api_attr(api, "credentials") <- file
  invisible(api)
}
new_token <- function(credentials, user, valid_days = 30) {
  token <- ids::random_id()
  expiry <- Sys.time() + valid_days * 60 * 60 * 24
  credentials$users[[user]]$token <- token
  credentials$tokens[[token]]$expiry <- expiry
  credentials$tokens[[token]]$user <- user
  credentials
}
#' @export
token_user <- function(api, token) {
  if (is.null(token)) {
    api_stop(401, "Token is missing")
  }
  file <- api_attr(api, "credentials")
  stopifnot(!is.null(file) || file.exists(file))
  credentials <- readRDS(file)
  if (!token %in% names(credentials$tokens)) {
    api_stop(401, "Invalid token")
  }
  if (Sys.time() > credentials$tokens[[token]]$expiry) {
    api_stop(401, "Token expired")
  }
  user <- credentials$tokens[[token]]$user
  user
}
api_workdir <- function(api) {
  api$work_dir
}
#' @export
api_user_workspace <- function(api, user) {
  if (!dir.exists(api_workdir(api))) {
    dir.create(api_workdir(api))
    dir.create(file.path(api_workdir(api), "workspace"))
  }
  workspace_dir <- file.path(api_workdir(api), "workspace", user)
  if (!dir.exists(workspace_dir)) {
    dir.create(workspace_dir)
  }
  workspace_dir
}
current_api <- function() {
  env <- parent.frame(3)
  api_stopifnot(exists("api", env), status = 500,
                "api object is not defined in the evaluation scope")
  env$api
}
current_user <- function() {
  env <- parent.frame(3)
  api_stopifnot(exists("user", env), status = 500,
                "user is not defined in the evaluation scope")
  env$user
}
current_job <- function() {
  env <- parent.frame(3)
  api_stopifnot(exists("job", env), status = 500,
                "job object is not defined in the evaluation scope")
  env$job
}
current_request <- function() {
  env <- parent.frame(3)
  api_stopifnot(exists("req", env), status = 500,
                "request object is not defined in the evaluation scope")
  env$req
}
#' @export
save_result <- function(data, format) {
  api <- current_api()
  user <- current_user()
  job <- current_job()
  req <- current_request()
  job_dir <- job_get_dir(api, user, job$id)
  host <- get_host(api, req)
  times <- stars::st_get_dimension_values(data, "t")
  bands <- "default"
  no_bands <- TRUE
  if ("bands" %in% names(stars::st_dimensions(data))) {
    bands <- stars::st_get_dimension_values(data, "bands")
    no_bands <- FALSE
  }
  data <- split(data, 3)
  results <- list()
  for (i in seq_along(times)) {
    assets_file <- lapply(seq_along(bands), \(j) {
      asset_file <- paste0(paste(bands[[j]], times[[i]], sep = "_"),
                           format_ext(format))
      if (no_bands) {
        stars::write_stars(dplyr::select(data[,,i], dplyr::all_of(j)), file.path(job_dir, asset_file))
      } else {
        stars::write_stars(dplyr::select(data[,,,i], dplyr::all_of(j)), file.path(job_dir, asset_file))
      }
      asset_file
    })
    # TODO: implement asset tokens
    assets <- lapply(assets_file, \(asset_file) {
      list(
        href = make_url(host, file.path("/files/jobs", job$id, asset_file),
                        token = base64enc::base64encode(charToRaw(user))),
        # TODO: implement format_content_type() function
        type = format_content_type(format),
        roles = list("data")
      )
    })
    names(assets) <- paste0(bands, "_", times[[i]])
    results <- c(results, assets)
    # bbox <- sf::st_bbox(data)
    # id <- digest::digest(list(bbox, times[[i]]))
    # item_file <- paste0("_", id, ".json")
    # item <- list(
    #   id = id,
    #   type = "Feature",
    #   geometry = NULL,
    #   properties = list(
    #     datetime = format(as.POSIXlt(times[[i]]), "%Y-%m-%dT%H:%M:%SZ")
    #   ),
    #   assets = assets,
    #   links = list(
    #     list(
    #       rel = "self",
    #       href = make_url(host, paste0("/files/jobs/", job$id, item_file)),
    #       type = "application/geo+json"
    #     )
    #   )
    # )
    # jsonlite::write_json(
    #   x = item,
    #   path = file.path(job_dir, item_file),
    #   auto_unbox = TRUE
    # )
    # list(
    #   href = item_file,
    #   rel = "item",
    #   type = "application/geo+json"
    # )
  }
  # TODO: where to out assets? links? assets?
  collection <- job_empty_collection(api, user, job)
  collection$assets <- results
  jsonlite::write_json(
    x = collection,
    path = file.path(job_dir, "_collection.json"),
    auto_unbox = TRUE
  )
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
#' @export
ext_format <- function(filename) {
  ext <- gsub("\\.([^.]+)$", "\\1", filename)
  switch(ext,
         tif = "gtiff",
         nc = "netcdf",
         rds = "rds",
         json = "json"
  )
}
#' @export
ext_content_type <- function(filename) {
  ext <- gsub("\\.([^.]+)$", "\\1", filename)
  switch(ext,
    tif = "image/tiff",
    nc = "application/octet-stream",
    rds = "application/rds",
    json = "application/json",
  )
}
#' @export
format_content_type <- function(format) {
  switch(format,
         gtiff = "image/tiff",
         netcdf = "application/octet-stream",
         rds = "application/rds",
         json = "application/json"
  )
}
