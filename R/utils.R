#' @import sits
#' @importFrom jsonlite read_json
#' @importFrom base64enc base64decode
NULL

as_name <- function(x) {
  as.name(x)
}

as_call <- function(id, args) {
  as.call(c(list(as_name(id)), lapply(args, function(x) x)))
}

make_fn <- function(params, body = NULL, env = parent.frame()) {
  fn <- function() {}
  formals(fn) <- params
  if (!is.null(body))
    body(fn) <- body
  environment(fn) <- env
  fn
}

param <- function(name, default = NULL) {
  par <- alist(x = )
  if (!is.null(default))
    par <- list(x = default)
  names(par) <- name
  par
}

format_endpoint <- function(x) {
  x <- gsub("<([^:]+):[^>]+>", "<\\1>", x)
  gsub("<([^>]+)>", "{\\1}", x)
}

placeholders <- function(x, schema = "openeo") {
  if (is.list(x)) {
    if (!is.null(names(x)))
      if ("$ref" %in% names(x) && is.character(x[["ref"]]))
        if (grepl(paste0("^", schema, ":"), x[["$ref"]]))
          return(gsub(paste0("^", schema, ":"), "", x[["$ref"]]))
    unique(unlist(lapply(x, placeholders)))
  } else
    NULL
}

#' @export
get_host <- function(api, req) {
  if ("HTTP_HOST" %in% names(req))
    return(paste0(req$rook.url_scheme, "://", req$HTTP_HOST))
  paste0(req$rook.url_scheme, "://", req$SERVER_NAME, req$SERVER_PORT)
}

#' @export
get_method <- function(req) {
  req$REQUEST_METHOD
}

get_link <- function(host, ...) {
  dots <- c(...)
  segments <- unname(dots)
  params <- NULL
  if (!is.null(names(dots))) {
    segments <- unname(dots[names(dots) == ""])
    params <- dots[names(dots) != ""]
  }
  path <- paste0(segments, collapse = "/")
  href <- paste0(host, path)
  query <- paste(names(params), unname(params), sep = "=", collapse = "&")
  if (query != "") href <- paste0(href, "?", query)
  href
}
