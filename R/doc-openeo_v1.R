#' @rdname doc_handling
#' @export
doc_wellknown.openeo_v1 <- function(api, req) {
  host <- get_host(api, req)
  doc <- update_wellknown_version(
    doc = list(),
    api_version = api$api_version,
    url = get_link(host, "/"),
    production = api$production
  )
  for (x in get_wellknown_versions(api)) {
    doc <- update_wellknown_version(
      doc = doc,
      api_version = x$api_version,
      url = x$url,
      production = x$production
    )
  }
  doc
}
#' @rdname doc_handling
#' @export
doc_landing_page.openeo_v1 <- function(api, req) {
  # TODO: support to billing key
  doc <- list(
    type = "Catalog",
    id = api$id,
    title = api$title,
    description = api$description,
    backend_version = api$backend_version,
    stac_version = api$stac_api$get("stac_version"),
    api_version = api$api_version,
    production = api$production,
    endpoints = get_endpoints(api),
    conformsTo = api$conforms_to
  )
  doc <- links_landing_page(doc, api, req)
  doc
}
#' @rdname doc_handling
#' @export
doc_processes.openeo_v1 <- function(api, req) {
  procs <- api_attr(api, "processes")
  procs <- list(
    processes = unname(procs)
  )
  procs
}
#' @rdname doc_handling
#' @export
doc_conformance.openeo_v1 <- function(api, req) {
  doc <- list(conformsTo = api$conforms_to)
  doc
}
#' @keywords internal
#' @export
links_landing_page.openeo_v1 <- function(doc, api, req) {
  # TODO:
  # It is highly RECOMMENDED to provide links with the following rel (relation) types:
  #   - version-history: A link back to the Well-Known URL (including /.well-known/openeo, see the corresponding endpoint for details) to allow clients to work on the most recent version.
  #   - terms-of-service: A link to the terms of service. If a back-end provides a link to the terms of service, the clients MUST provide a way to read the terms of service and only connect to the back-end after the user agreed to them. The user interface MUST be designed in a way that the terms of service are not agreed to by default, i.e. the user MUST explicitly agree to them.
  #   - privacy-policy: A link to the privacy policy (GDPR). If a back-end provides a link to a privacy policy, the clients MUST provide a way to read the privacy policy and only connect to the back-end after the user agreed to them. The user interface MUST be designed in a way that the privacy policy is not agreed to by default, i.e. the user MUST explicitly agree to them.
  #   - service-desc or service-doc: A link to the API definition. Use service-desc for machine-readable API definition and service-doc for human-readable API definition. Required if full OGC API compatibility is desired.
  #   - conformance: A link to the Conformance declaration (see /conformance). Required if full OGC API compatibility is desired.
  #   - data: A link to the collections (see /collections). Required if full OGC API compatibility is desired.
  #   - create-form: A link to a user registration page.
  #   - recovery-form: A link to a page where a user can recover a user account (e.g. to reset the password or send a reminder about the username to the user's email account).

  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/json")
  doc <- link_spec(doc, api, req)
  doc <- link_docs(doc, api, req)
  host <- get_host(api, req)
  doc <- update_link(
    doc = doc,
    rel = "conformance",
    href = make_url(host, "/conformance"),
    type = "application/json"
  )
  doc <- update_link(
    doc = doc,
    rel = "data",
    href = make_url(host, "/collections"),
    type = "application/json"
  )
  doc <- update_link(
    doc = doc,
    rel = "version-history",
    href = make_url(host, "/.well-known/openeo"),
    type = "application/json"
  )
  doc
}
