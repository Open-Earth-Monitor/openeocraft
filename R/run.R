
#' @export
load_processes <- function(api, processes_file) {
  stopifnot(file.exists(processes_file))
  setup_namespace(api)
  # TODO: split environments
  eval(parse(processes_file, encoding = "UTF-8"), envir = get_namespace(api))
  api_attr(api, "processes") <- list()
  process_decorators(api, processes_file, decorator = "openeo-process")
  api
}
#' @export
current_env <- function() {
  n <- sys.nframe()
  if (n < 2)
    api_stop(500, "invalid evaluation environment")
  for (i in seq(2, n)) {
    env <- parent.frame(i)
    if (exists("openeocraft", env))
      return(env)
  }
  api_stop(500, "invalid evaluation environment")
}
#' @export
current_api <- function() {
  env <- current_env()
  env$api
}
#' @export
current_user <- function() {
  env <- current_env()
  env$user
}
#' @export
current_job <- function() {
  env <- current_env()
  env$job
}
#' @export
current_request <- function() {
  env <- current_env()
  env$req
}
get_namespace <- function(api) {
  api_attr(api, "namespace")
}

setup_namespace <- function(api) {
  namespace <- new.env(parent = emptyenv())
  api_attr(api, "namespace") <- namespace
  load_rlang(api)
  api
}

add_process <- function(api, process) {
  processes <- api_attr(api, "processes")
  processes[[process$id]] <- process
  api_attr(api, "processes") <- processes
  api
}

load_rlang <- function(api) {
  export_fn <- function(...) {
    fn_list <- list(...)
    for (fn in fn_list) {
      value <- eval(as.name(fn), envir = environment())
      assign(fn, value, envir = get_namespace(api), inherits = FALSE)
    }
  }
  export_fn(":::", "::", ":")
  export_fn("{", "(")
  export_fn("=", "<-", "<<-", "$<-", "[<-", "[[<-")
  export_fn("$", "[", "[[")
  export_fn("*", "+", "-", "/", "%%", "%/%", "%*%", "%in%")
  export_fn("==", "<", ">", "<=", ">=", "!=")
  export_fn("&", "&&", "|", "||", "!")
  export_fn("if", "for", "while", "repeat", "break", "next")
  export_fn("function", "return")
  export_fn("c", "list", "stop")
  invisible(NULL)
}

run_pgraph <- function(api, req, user, job, pg) {
  if ("process" %in% names(pg))
    pg <- pg$process
  expr <- pgraph_expr(pg)
  # TODO: need to define a scope with api and user objects
  # a possible solution is load the processes per request
  env <- create_env(api, user, job, req)
  eval(expr, envir = env, enclos = get_namespace(api))
}
