#' Link helper functions
#'
#' These functions modify links in an document by adding new links or
#' updating existing ones.
#'
#' \itemize{
#'
#' \item `add_link`: This function adds a new link to the document by
#'   appending it to the existing links.
#'
#' \item `update_link`: This function updates an existing link in the
#'   document by replacing it with a new link. The existing link is
#'   identified by its relationship (rel). If multiple links with
#'   the same relationship exist, all of them are replaced.
#'
#' }
#'
#' @param doc The document to which the link will be added.
#'
#' @param rel The relationship of the link (e.g., "self", "child").
#'
#' @param href The URL of the linked resource.
#'
#' @param ... Additional attributes to include in the link.
#'
#' @return The updated document with the new link added.
#'
#' @name link_functions
NULL
#' @rdname link_functions
#' @export
add_link <- function(doc, rel, href, ...) {
  doc$links <- c(doc$links, list(new_link(rel, href, ...)))
  doc
}
#' @rdname link_functions
#' @export
update_link <- function(doc, rel, href, ...) {
  select <- vapply(doc$links, \(x) !is.null(x$rel) && x$rel != rel, logical(1))
  doc$links <- doc$links[select]
  add_link(doc, rel, href, ...)
}
#' @rdname link_functions
#' @export
delete_link <- function(doc, rel, ...) {
  select <- vapply(doc$links, \(x) !is.null(x$rel) && x$rel != rel, logical(1))
  doc$links <- doc$links[select]
  doc
}
#' @keywords internal
new_link <- function(rel, href, ...) {
  dots <- list(...)
  not_null <- !vapply(dots, is.null, logical(1), USE.NAMES = FALSE)
  c(list(rel = rel, href = href), dots[not_null])
}
#' Build absolute URLs from host and path fragments
#'
#' @param host Base service URL including protocol and host name.
#'
#' @param ... Character vectors representing path fragments or named query
#'   parameters.
#'
#' @return A character string containing the composed URL.
#'
#' @export
make_url <- function(host, ...) {
  dots <- c(...)
  segments <- unname(dots)
  params <- NULL
  if (!is.null(names(dots))) {
    segments <- unname(dots[names(dots) == ""])
    params <- dots[names(dots) != ""]
  }
  path <- paste0(segments, collapse = "/")
  url <- paste0(host, path)
  query <- paste(names(params), unname(params), sep = "=", collapse = "&")
  if (query != "") url <- paste0(url, "?", query)
  url
}
#' @keywords internal
root_url <- function(api, req) {
  make_url(get_host(api, req), "/")
}
#' @keywords internal
self_url <- function(api, req) {
  make_url(get_host(api, req), get_path(req))
}
#' @keywords internal
parent_url <- function(api, req) {
  sub("((://)?[^/]+)/[^/]*$", "\\1", self_url(api, req))
}
#' @keywords internal
link_root <- function(doc, api, req) {
  update_link(doc, "root", root_url(api, req), type = "application/json")
}
#' @keywords internal
link_self <- function(doc, api, req, type) {
  update_link(doc, "self", self_url(api, req), type = type)
}
#' @keywords internal
link_parent <- function(doc, api, req) {
  update_link(doc, "parent", parent_url(api, req), type = "application/json")
}
#' @keywords internal
links_navigation <- function(doc,
                             api,
                             req,
                             endpoint,
                             limit,
                             page, ...,
                             type) {
  host <- get_host(api, req)
  pages <- get_pages(doc, limit)
  if (page > 1 && page <= pages) {
    url <- make_url(host, endpoint, limit = limit, page = page - 1, ...)
    doc <- update_link(doc, "prev", url, type = type)
  }
  if (page < pages) {
    url <- make_url(host, endpoint, limit = limit, page = page + 1, ...)
    doc <- update_link(doc, "next", url, type = type)
  }
  doc
}
#' @keywords internal
links_navigagion_post <- function(doc,
                                  api,
                                  req,
                                  endpoint,
                                  limit,
                                  page, ...,
                                  type,
                                  merge) {
  host <- get_host(api, req)
  pages <- get_pages(doc, limit)
  if (page > 1 && page <= pages)
    doc <- add_link(
      doc = doc,
      rel = "prev",
      href = make_url(host, endpoint),
      body = list(
        page = page - 1,
        ...
      ),
      merge = merge,
      type = type
    )
  if (page < pages)
    doc <- add_link(
      doc = doc,
      rel = "next",
      href = make_url(host, endpoint),
      body = list(
        page = page + 1,
        ...
      ),
      merge = merge,
      type = type
    )
  doc
}
#' @keywords internal
link_spec <- function(doc, api, req) {
  spec_endpoint <- api_attr(api, "spec_endpoint")
  if (is.null(spec_endpoint)) return(doc)
  url <- make_url(get_host(api, req), spec_endpoint)
  doc <- update_link(
    doc = doc,
    rel = "service-spec",
    href = url,
    type = "application/vnd.oai.openapi+json;version=3.0",
    title = "API conformance classes implemented by this server"
  )
  doc
}
#' @keywords internal
link_docs <- function(doc, api, req) {
  docs_endpoint <- api_attr(api, "docs_endpoint")
  if (is.null(docs_endpoint)) return(doc)
  url <- make_url(get_host(api, req), docs_endpoint)
  doc <- update_link(
    doc = doc,
    rel = "service-doc",
    href = url,
    type = "text/html",
    title = "The API documentation"
  )
  doc
}
