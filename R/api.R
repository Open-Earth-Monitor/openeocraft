#' @export
api_reg_endpoints <- function(api, pr) {
  endpoints <- lapply(pr$endpoints$`__no-preempt__`, function(x) {
    list(path = format_endpoint(x$path), methods = list(x$verbs))
  })
  set_attr(api, "endpoints", endpoints)
}

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

#' @export
api_credential <- function(api, req, res) {
  auth <- gsub("Basic ", "",req$HTTP_AUTHORIZATION)
  auth <- rawToChar(base64enc::base64decode(auth))
  # print(auth) # "rolf:123456"
  # TODO: implement token generator based on authentication
  token = "b34ba2bdf9ac9ee1"
  list(access_token = token)
}

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

#' @export
api_collections <- function(api, req, res) {
  get_collections(api, req, res)
}

#' @export
api_collection <- function(api, req, res, collection_id) {
  get_collections(api, req, res, collection_id)
}

#' @export
api_result <- function(api, req, res) {
  p <- req$body
  if ("process" %in% names(p))
    p <- p$process
  run_pgraph(api, p)
}
