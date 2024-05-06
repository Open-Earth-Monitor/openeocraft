#' Register API Endpoints
#'
#' Registers non-preemptible endpoints for a given API from a predefined endpoint list.
#'
#' @param api An object representing the API to which endpoints will be added.
#' @param pr A list containing endpoint configurations under the `endpoints` and `__no-preempt__` attributes.
#' @return The modified API object with newly registered endpoints.
#' @export
api_reg_endpoints <- function(api, pr) {
  endpoints <- lapply(pr$endpoints$`__no-preempt__`, function(x) {
    list(path = format_endpoint(x$path), methods = list(x$verbs))
  })
  set_attr(api, "endpoints", endpoints)
}

#' Fetch Well-Known Information of the API
#'
#' Retrieves well-known information related to the API, such as available versions.
#'
#' @param api An API object containing details such as the API version.
#' @param req The request object.
#' @param res The response object.
#' @return A list containing API version details and production status.
#' @export
api_wellknown <- function(api, req, res) {
  host <- get_host(api, req)
  list(
    versions = list(
      list(
        url = get_link(host, "/"),
        production = FALSE,
        api_version = api$api_version
      )
    )
  )
}

#' Generate API Credential Token
#'
#' Extracts credentials from the request and generates an access token.
#'
#' @param api An object representing the API.
#' @param req The request object, containing authorization headers.
#' @param res The response object.
#' @return A list containing the generated access token.
#' @export
api_credential <- function(api, req, res) {
  auth <- gsub("Basic ", "",req$HTTP_AUTHORIZATION)
  auth <- rawToChar(base64enc::base64decode(auth))
  # print(auth) # "rolf:123456"
  # TODO: implement token generator based on authentication
  token = "b34ba2bdf9ac9ee1"
  list(access_token = token)
}

#' Retrieve API Landing Page Details
#'
#' Fetches the landing page details of the API, including metadata and link information.
#'
#' @param api An object representing the API.
#' @param req The request object.
#' @param res The response object.
#' @return A list containing the landing page details.
#' @export
api_landing_page <- function(api, req, res) {
  host <- get_host(api, req)
  list(
    id = api$id,
    title = api$title,
    description = api$description,
    backend_version = api$backend_version,
    stac_version = "1.0.0",
    type = "Catalog",
    api_version = api$api_version,
    production = FALSE,
    endpoints = get_endpoints(api),
    links = list(
      # TODO: list other types of links
      new_link(
        href = get_link(host, "/"),
        rel = "self"
      )
    )
  )
}

#' Retrieve API Processes Information
#'
#' Fetches information about the available processes for the API.
#'
#' @param api An object representing the API.
#' @param req The request object.
#' @param res The response object.
#' @return A list containing details of available processes and relevant links.
#' @export
api_processes <- function(api, req, res) {
  host <- get_host(api, req)
  processes <- get_attr(api, "processes")
  processes_list <- list(
    processes = unname(processes),
    links = list(
      new_link(
        rel = "self",
        href = get_link(host, "/processes")
      )
    )
  )
  processes_list
}

#' Retrieve API Collections Information
#'
#' Fetches information about the collections available in the API.
#'
#' @param api An object representing the API.
#' @param req The request object.
#' @param res The response object.
#' @return Details of the available collections.
#' @export
api_collections <- function(api, req, res) {
  get_collections(api, req, res)
}

#' Retrieve Specific API Collection Information
#'
#' Fetches information about a specific collection identified by `collection_id`.
#'
#' @param api An object representing the API.
#' @param req The request object.
#' @param res The response object.
#' @param collection_id A string identifier for the collection to be retrieved.
#' @return Details of the specified collection.
#' @export
api_collection <- function(api, req, res, collection_id) {
  get_collections(api, req, res, collection_id)
}

#' Process and Return API Result
#'
#' Processes input data according to a specified process graph and returns the result.
#'
#' @param api An object representing the API.
#' @param req The request object, expected to contain a body with processing details.
#' @param res The response object.
#' @return The result of the processing operation.
#' @export
api_result <- function(api, req, res) {
  p <- req$body
  if ("process" %in% names(p))
    p <- p$process
  run_pgraph(api, p)
}


#' Manage API Jobs
#'
#' Handles the creation, updating, fetching, and deletion of jobs based on the request method and parameters.
#'
#' @param api An object representing the API.
#' @param req The request object, indicating the method and potential body for creating or updating jobs.
#' @param res The response object.
#' @param job_id Optional; a string identifier for the job if specific job actions are required.
#' @param subroute Optional; specifies a particular job action such as 'estimate', 'logs', or 'results'.
#' @return Depending on the request, this might return job details, creation confirmation, or the results of a specific job.
#' @export
api_jobs <- function(api, req, res, job_id = NULL, subroute = NULL) {
  # Handling requests that involve a specific job
  if (!is.null(job_id)) {
    if (is.null(subroute)) {
      switch(req$REQUEST_METHOD,
             "GET" = return(job_info(job_id)),
             "PATCH" = return(job_update(job_id, req$body)),
             "DELETE" = return(job_delete(job_id)),
             stop("Unsupported method"))
    } else {
      # Subroutes like /estimate, /logs, /results
      switch(subroute,
             "estimate" = {
               if (req$REQUEST_METHOD == "GET") {
                 return(job_estimate(job_id))
               } else {
                 stop("Unsupported method for estimate")
               }
             },
             "logs" = {
               if (req$REQUEST_METHOD == "GET") {
                 return(job_logs(job_id))
               } else {
                 stop("Unsupported method for logs")
               }
             },
             "results" = {
               switch(req$REQUEST_METHOD,
                      "GET" = return(job_get_results(job_id)),
                      "POST" = return(job_start(job_id)),
                      "DELETE" = return(job_delete(job_id)),
                      stop("Unsupported method for results"))
             },
             stop("Invalid subroute"))
    }
  } else {
    # Handling requests that do not specify a job_id
    switch(req$REQUEST_METHOD,
           "GET" = return(jobs_list_all()),
           "POST" = return(job_create(req$body)),
           stop("Unsupported method"))
  }
}
