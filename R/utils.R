#' @import plumber
#' @import sits
#' @importFrom jsonlite read_json
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

list_endpoints <- function() {
  api <- get_api()
  lapply(api$endpoints$`__no-preempt__`, function(x) {
    list(path = format_endpoint(x$path), methods = list(x$verbs))
  })
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
