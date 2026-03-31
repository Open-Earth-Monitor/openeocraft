#' Manage the openeocraft process runtime
#'
#' These helpers prepare and interact with the sandboxed environment used to
#' evaluate openEO process graphs.
#'
#' `load_processes()` builds an isolated evaluation namespace ([get_namespace()])
#' for process graph execution, sources `processes_file` into it (defining
#' process functions), clears the API process registry, then runs
#' [process_decorators()] so each `#* @openeo-process` block registers JSON
#' metadata from `dirname(processes_file)/processes/*.json`.
#'
#' `current_env()` walks parent frames from a process graph evaluation to find
#' the frame that contains `openeocraft` (injected by the job runner); that list
#' holds `api`, `user`, `job`, etc.
#'
#' `get_job_dir()` resolves the on-disk workspace directory for the active job.
#'
#' @param api An openeocraft API object.
#'
#' @param processes_file Path to an R file that defines processes via
#'   `#* @openeo-process` chunks (see [process_decorators()]).
#'
#' @param env A runtime environment produced by `current_env()`.
#'
#' @return `load_processes()` returns the API object with its namespace
#'   populated; `current_env()` returns the current runtime environment as a
#'   `list`; `get_job_dir()` returns the file path backing the active job.
#'
#' @name process_runtime
#' @seealso [process_decorators()], [`openeo-process`], [run_pgraph()]
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

#' Get API process-graph evaluation namespace
#'
#' @param api API object.
#' @return An environment (`emptyenv()` parent) where process functions from
#'   `processes_file` are defined and base operators are bound for sandboxed
#'   [run_pgraph()] evaluation.
#' @keywords internal
get_namespace <- function(api) {
    api_attr(api, "namespace")
}

#' Initialise API namespace and bind safe primitives
#'
#' Creates a fresh environment on `api`, then [load_rlang()] copies a restricted
#' set of language primitives and helpers (`::`, `[`, `if`, `function`, etc.)
#' plus [current_env()] and [get_job_dir()] for use inside process code.
#'
#' @param api API object.
#' @return `NULL`, invisibly (updates `api` attribute `"namespace"`).
#' @keywords internal
setup_namespace <- function(api) {
    namespace <- new.env(parent = emptyenv())
    api_attr(api, "namespace") <- namespace
    load_rlang(api)
    api
}

#' Append one process descriptor to the API registry
#'
#' @param api API object.
#' @param process A `list` with at least `id` (character), typically as read
#'   from `processes/<id>.json`.
#' @return Updated `api` (invisibly, via side effect on `api` attribute
#'   `"processes"`).
#' @seealso [`openeo-process`]
#' @keywords internal
add_process <- function(api, process) {
    processes <- api_attr(api, "processes")
    processes[[process$id]] <- process
    api_attr(api, "processes") <- processes
    api
}

#' Copy core R forms into the process evaluation namespace
#'
#' Binds operators, control flow, subsetting, and namespace operators so parsed
#' process graphs can be evaluated with [run_pgraph()] without inheriting the
#' full global environment.
#'
#' @param api API object.
#' @return `NULL`, invisibly.
#' @keywords internal
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
    export_fn("*", "+", "-", "/", "^", "%%", "%/%", "%*%", "%in%")
    export_fn("==", "<", ">", "<=", ">=", "!=")
    export_fn("&", "&&", "|", "||", "!")
    export_fn("if", "for", "while", "repeat", "break", "next")
    export_fn("function", "return")
    export_fn("c", "list", "stop", "character")
    # load openeocraft runtime functions
    export_fn("current_env", "get_job_dir")
    export_fn("substitute", "quote")
    invisible(NULL)
}

#' Evaluate a parsed openEO process graph
#'
#' Converts `pg` to an R expression with [pgraph_expr()], builds the job
#' evaluation environment with [create_env()], then `eval(expr, envir = env,
#' enclos = get_namespace(api))` so process calls resolve to functions loaded
#' by [load_processes()].
#'
#' @param api API object.
#' @param req Plumber request (passed through to [create_env()]).
#' @param user Authenticated user id / object.
#' @param job Job metadata list (`id`, etc.).
#' @param pg Process graph `list` (or `list(process = ...)` wrapper).
#' @return Value of evaluating the graph (job result), or `NULL` if `pg` is
#'   `NULL`.
#' @seealso [load_processes()], [pgraph_expr()], [create_env()]
#' @keywords internal
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
