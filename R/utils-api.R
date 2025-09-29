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
#' @param api The openeocraft API object used to service the request.
#'
#' @param origin The value to set for the 'Access-Control-Allow-Origin'
#'   header in CORS requests. Defaults to '*'.
#'
#' @param methods The value to set for the 'Access-Control-Allow-Methods'
#'   header in CORS requests. Defaults to '*'.
#'
#' @param err The error object containing information about the
#'   encountered error. If the error is thrown by `api_stop`
#'   function, this object has 'status' and 'message' fields
#'   that are used to produce the HTTP response error.
#'
#' @param status The HTTP status code to set for the response. This just
#'   works if the `api_error_handler` function is handling errors in
#'   `plumber`.
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
        res$setHeader(
            "Access-Control-Allow-Headers",
            req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS
        )
        res$status <- 200L
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
api_success <- function(status, ...) {
    list(code = status, message = paste0(...))
}
#' @rdname api_helpers
#' @export
get_host <- function(api, req) {
    host <- api_attr(api, "api_base_url")
    if (!is.null(host)) {
        return(host)
    }
    if ("HTTP_HOST" %in% names(req)) {
        return(paste0(req$rook.url_scheme, "://", req$HTTP_HOST))
    }
    host <- paste0(req$rook.url_scheme, "://", req$SERVER_NAME)
    if (!is.null(req$SERVER_PORT) && nzchar(req$SERVER_PORT) &&
        req$SERVER_PORT != "80") {
        host <- paste0(host, ":", req$SERVER_PORT)
    }
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
    if (exists(name, envir = api_env(api), inherits = FALSE)) {
        get(name, envir = api_env(api), inherits = FALSE)
    }
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
#' Credential management helpers
#'
#' These helpers manage the credential store used by openeocraft during
#' authentication. They allow you to register credential files with an API
#' instance, add or update user/password pairs, and resolve bearer tokens
#' from incoming requests.
#'
#' @param api An openeocraft API object.
#'
#' @param file Path to an `.rds` file used to persist credential data.
#'
#' @param user Username to retrieve or update in the credential store.
#'
#' @param password Plain-text password to store for `user`.
#'
#' @param req A plumber request object that contains the
#'   `HTTP_AUTHORIZATION` header.
#'
#' @param token Access token extracted from an HTTP request.
#'
#' @return `set_credentials()` returns the `api` object invisibly.
#'   `get_token()` returns the access token extracted from the request.
#'   `get_token_user()` returns the user associated with a token.
#'
#' @name credential_helpers
NULL

#' @rdname credential_helpers
#' @export
new_credential <- function(api, user, password) {
    file <- api_attr(api, "credentials")
    stopifnot(!is.null(file) || file.exists(file))
    credentials <- readRDS(file)
    current_value <- list()
    if (user %in% names(credentials$users)) {
        current_value <- credentials$users[[user]]
    }
    credentials$users[[user]] <- utils::modifyList(
        x = current_value,
        val = list(user = user, password = password)
    )
    saveRDS(credentials, file)
}
empty_credentials <- function() {
    list(users = list(), tokens = list())
}
#' @rdname credential_helpers
#' @export
set_credentials <- function(api, file) {
    if (!file.exists(file)) {
        saveRDS(empty_credentials(), file)
    }
    api_attr(api, "credentials") <- file
    invisible(api)
}
new_token <- function(credentials, user, valid_days = 30) {
    token <- random_id(16)
    expiry <- Sys.time() + valid_days * 60 * 60 * 24
    credentials$users[[user]]$token <- token
    credentials$tokens[[token]]$expiry <- expiry
    credentials$tokens[[token]]$user <- user
    credentials
}
#' @rdname credential_helpers
#' @export
get_token <- function(req) {
    gsub("^.*//", "", req$HTTP_AUTHORIZATION)
}
#' @rdname credential_helpers
#' @export
get_token_user <- function(api, token) {
    if (!length(token)) {
        api_stop(401L, "Token is missing")
    }
    file <- api_attr(api, "credentials")
    if (is.null(file)) {
        credentials <- empty_credentials()
    } else if (file.exists(file)) {
        credentials <- readRDS(file)
    } else {
        stop("Credential file not found", call. = FALSE)
    }
    if (!token %in% names(credentials$tokens)) {
        api_stop(401L, "Invalid token")
    }
    if (Sys.time() > credentials$tokens[[token]]$expiry) {
        api_stop(401L, "Token expired")
    }
    user <- credentials$tokens[[token]]$user
    user
}
api_workdir <- function(api) {
    api$work_dir
}
#' Resolve or initialise a user workspace directory
#'
#' @param api An openeocraft API object that stores the base work directory.
#'
#' @param user User identifier whose workspace should be prepared.
#'
#' @return Normalised path to the workspace directory.
#'
#' @export
api_user_workspace <- function(api, user) {
    if (!dir.exists(api_workdir(api))) {
        dir.create(api_workdir(api), recursive = TRUE)
        dir.create(file.path(api_workdir(api), "workspace"), recursive = TRUE)
    }
    workspace_dir <- file.path(api_workdir(api), "workspace", user)
    if (!dir.exists(workspace_dir)) {
        dir.create(workspace_dir, recursive = TRUE)
    }
    workspace_dir
}
create_env <- function(api, user, job, req) {
    list(
        openeocraft = TRUE,
        api = api,
        user = user,
        job = job,
        req = req
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
    # TODO: improve file formats API enabling the registration of
    #   additional formats
    # Define the output formats
    output_formats <- list(
        GeoTiff = list(
            title = "GeoTiff",
            description = "Export data to GeoTiff.",
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
            description = "Export data to NetCDF.",
            gis_data_types = list("raster"),
            parameters = list(
                format = list(
                    type = "string",
                    description = "NetCDF"
                )
            )
        ),
        JSON = list(
            title = "JSON Data Serialization",
            description = "Export data to JSON.",
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
    input_formats <- list(
        GeoTiff = list(
            title = "GeoTiff",
            description = "GeoTiff is one of the most widely supported raster formats. This backend allows reading from GeoTiff to create raster data cubes.",
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
        input = input_formats,
        output = output_formats
    )
}

file_formats_auth <- function(doc) {
    output_formats <- list(
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
    input_formats <- list()

    # return the list of supported formats
    list(
        input = utils::modifyList(doc$input, input_formats),
        output = utils::modifyList(doc$output, output_formats)
    )
}

#' Build signed URLs to artefacts in the workspace
#'
#' @param host Service root URL.
#'
#' @param user User identifier encoded into the token parameter.
#'
#' @param job_id Identifier of the job whose files are requested.
#'
#' @param file Relative file path inside the job folder.
#'
#' @param folder Root-level folder under `/files/root`.
#'
#' @return A character string containing the absolute URL.
#'
#' @name workspace_url_helpers
NULL

#' @rdname workspace_url_helpers
#' @export
make_job_files_url <- function(host, user, job_id, file) {
    token <- base64enc::base64encode(charToRaw(user))
    file <- file.path("/files/jobs", job_id, file)
    paste0(host, file, "?token=", token)
}
#' @rdname workspace_url_helpers
#' @export
make_workspace_files_url <- function(host, user, folder, file) {
    token <- base64enc::base64encode(charToRaw(user))
    file <- file.path("/files/root", folder, file)
    paste0(host, file, "?token=", token)
}
