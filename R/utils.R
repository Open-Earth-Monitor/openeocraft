#' @import plumber
#' @import sits
#' @importFrom jsonlite read_json
NULL

format_endpoint <- function(x) {
  x <- gsub("<([^:]+):[^>]+>", "<\\1>", x)
  gsub("<([^>]+)>", "{\\1}", x)
}

list_endpoints <- function() {
  api <- get_api()
  lapply(api$endpoints$`__no-preempt__`, function(x) {
    list(path = format_endpoint(x$path), methods = list(x$verbs))
  })
}

placeholders <- function(x, schema = "sits-openeo") {
  if (is.list(x)) {
    if (!is.null(names(x)))
      if ("$ref" %in% names(x) && is.character(x[["ref"]]))
        if (grepl(paste0("^", schema, ":"), x[["$ref"]]))
          return(gsub(paste0("^", schema, ":"), "", x[["$ref"]]))
    unique(unlist(lapply(x, placeholders)))
  } else
    NULL
}
