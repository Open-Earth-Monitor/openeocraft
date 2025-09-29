#' Manage the openeocraft process runtime
#'
#' These helpers prepare and interact with the sandboxed environment used to
#' evaluate openEO process graphs.
#'
#' @param api An openeocraft API object.
#'
#' @param processes_file Path to a file that defines processes via
#'   decorators.
#'
#' @param env A runtime environment produced by `current_env()`.
#'
#' @return `load_processes()` returns the API object with its namespace
#'   populated; `current_env()` returns the current runtime environment; and
#'   `get_job_dir()` returns the file path backing the active job.
#'
#' @name process_runtime
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
#' @rdname process_runtime
#' @export
current_env <- function() {
    n <- sys.nframe()
    if (n < 2) {
        api_stop(500, "invalid evaluation environment")
    }
    for (i in seq(2, n)) {
        env <- parent.frame(i)
        if (exists("openeocraft", env)) {
            return(as.list(env))
        }
    }
    api_stop(500, "invalid evaluation environment")
}
#' @rdname process_runtime
#' @export
get_job_dir <- function(env = NULL) {
    if (is.null(env)) {
        env <- current_env()
    }
    job_get_dir(env$api, env$user, env$job$id)
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
    # load openeocraft runtime functions
    export_fn("current_env", "get_job_dir")
    export_fn("substitute", "quote")
    invisible(NULL)
}
run_pgraph <- function(api, req, user, job, pg) {
    if (is.null(pg)) {
        return(NULL)
    }
    if ("process" %in% names(pg)) {
        pg <- pg$process
    }
    expr <- pgraph_expr(pg)
    # TODO: need to define a scope with api and user objects
    # a possible solution is load the processes per request
    env <- create_env(api, user, job, req)
    eval(expr, envir = env, enclos = get_namespace(api))
}
