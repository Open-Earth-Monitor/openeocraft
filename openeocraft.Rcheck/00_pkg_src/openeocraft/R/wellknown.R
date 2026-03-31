#' `Wellknown` helper functions
#'
#' These functions modify and creates the API wellknown versions document.
#'
#' \itemize{
#'
#' \item `add_wellknown_version`: This function adds a new version entry
#'   to the document by appending it to the existing list.
#'
#' \item `update_wellknown_version`: This function updates an existing
#'   entry in the `wellknown` versions document by replacing it with a
#'   new entry. The existing entry is identified by its `api_version`.
#'   If multiple entries with the same version exist, all of them are
#'   replaced.
#'
#' }
#'
#' @param doc The document to which the version entry will be added/modified.
#'
#' @param api_version Version number of the openEO specification the
#'   back-end's entry implements.
#'
#' @param url Absolute URLs to the service.
#'
#' @param production Specifies whether the implementation is ready to be used
#'   in production (`TRUE`) or not (`FALSE`, default).
#'
#' @param ... Additional attributes to include in the version entry.
#'
#' @return For API creation functions, returns a api object.
#'   For API handling functions, returns the document to return
#'   as response.
#'
#' @name wellknown_functions
#'
NULL
#' @rdname wellknown_functions
#' @export
add_wellknown_version <- function(doc,
                                  api_version,
                                  url,
                                  production = FALSE, ...) {
    doc$versions <- c(doc$versions, list(
        new_wellknown_version(api_version, url, production, ...)
    ))
    doc
}
#' @rdname wellknown_functions
#' @export
update_wellknown_version <- function(doc,
                                     api_version,
                                     url,
                                     production = FALSE, ...) {
    select <- vapply(doc$versions, \(x) {
        !is.null(x$api_version) && x$api_version != api_version
    }, logical(1))
    doc$versions <- doc$versions[select]
    add_wellknown_version(doc, api_version, url, production, ...)
}
#' @keywords internal
new_wellknown_version <- function(api_version, url, production, ...) {
    dots <- list(...)
    not_null <- !vapply(dots, is.null, logical(1), USE.NAMES = FALSE)
    c(
        list(api_version = api_version, url = url, production = production),
        dots[not_null]
    )
}
