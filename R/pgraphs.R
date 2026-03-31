#' Test whether a list is a single process node
#'
#' @param node A list with optional names.
#'
#' @return Logical scalar: `TRUE` if `node` has `process_id` and `arguments` of
#'   the expected types.
#'
#' @keywords internal
is_pnode <- function(node) {
    if (!is.list(node) || is.null(names(node))) {
        return(FALSE)
    }
    if (!all(c("process_id", "arguments") %in% names(node))) {
        return(FALSE)
    }
    if (!is.character(node$process_id)) {
        return(FALSE)
    }
    TRUE
}

#' Test whether a list is a valid process graph wrapper
#'
#' @param pg A list containing `process_graph`.
#'
#' @return Logical scalar: `TRUE` if every element is a node and exactly one
#'   node is marked as the result.
#'
#' @keywords internal
is_pgraph <- function(pg) {
    if (!is.list(pg) || is.null(names(pg))) {
        return(FALSE)
    }
    if (!"process_graph" %in% names(pg)) {
        return(FALSE)
    }
    if (!all(vapply(pg$process_graph, is_pnode, logical(1)))) {
        return(FALSE)
    }
    if (sum(vapply(pg$process_graph, pgraph_result, logical(1))) != 1) {
        return(FALSE)
    }
    TRUE
}

#' Classify an openEO argument value for dispatch
#'
#' @param x Any R object representing a process argument (scalar, list,
#'   `from_node`, nested graph, etc.).
#'
#' @return Character string: `"null"`, `"character"`, `"numeric"`, `"logical"`,
#'   `"array"`, `"object"`, `"result_reference"`, `"user_defined_process"`, or
#'   `"parameter_reference"`.
#'
#' @keywords internal
arg_type <- function(x) {
    if (is.null(x)) {
        return("null")
    }
    if (is.character(x)) {
        return("character")
    }
    if (is.numeric(x)) {
        return("numeric")
    }
    if (is.logical(x)) {
        return("logical")
    }
    if (is.list(x)) {
        if (is.null(names(x))) {
            return("array")
        }
        if ("from_node" %in% names(x)) {
            return("result_reference")
        }
        if ("process_graph" %in% names(x)) {
            return("user_defined_process")
        }
        if ("from_parameter" %in% names(x)) {
            return("parameter_reference")
        }
        "object"
    }
}

#' Dispatch on \code{arg_type()} with a shared fallback
#'
#' @param x Argument value.
#' @param ... Named branches passed to \code{base::switch()} (must cover all
#'   types returned by \code{arg_type()} or rely on unnamed default handling).
#'
#' @return Result of the matched branch.
#'
#' @details
#' Throws `stop("Not supported argument type.")` if `arg_type(x)` is unknown.
#'
#' @keywords internal
arg_switch <- function(x, ...) {
    switch(arg_type(x),
        ...,
        stop("Not supported argument type.")
    )
}

#' Build a `list(...)` call from process arguments
#'
#' @param args List of argument values.
#' @param env Environment holding evaluated sibling nodes.
#'
#' @return A `call` object.
#'
#' @keywords internal
list_expr <- function(args, env) {
    args <- pnode_args(args, env = env)
    as.call(c(as.name("list"), args))
}

#' Find the process node marked as the graph result
#'
#' @param pg Validated process graph list.
#'
#' @return The result node list, or `NULL` if none found.
#'
#' @keywords internal
starting_pnode <- function(pg) {
    for (node in pg$process_graph) {
        if (pgraph_result(node)) {
            return(node)
        }
    }
}

#' Convert each process argument to an R expression fragment
#'
#' @param args Named list of arguments from one node.
#' @param env Environment of node id -> partially built expressions.
#'
#' @return List of language objects suitable for \code{as.call()}.
#'
#' @keywords internal
pnode_args <- function(args, env) {
    lapply(args, function(arg) {
        arg_switch(
            arg,
            null = , character = , numeric = , logical = arg,
            array = , object = list_expr(arg, env = env),
            result_reference = {
                node <- get(arg$from_node, envir = env, inherits = FALSE)
                pnode_expr(node, env = env)
            },
            user_defined_process = pgraph_expr(arg, parent = env),
            parameter_reference = as_name(arg$from_parameter)
        )
    })
}

#' Build a `call` for one process node
#'
#' @param node A process node list.
#' @param env Environment for resolving `from_node` references.
#'
#' @return A `call` object `process_id(...)`.
#'
#' @keywords internal
pnode_expr <- function(node, env) {
    args <- pnode_args(node$arguments, env = env)
    as_call(node$process_id, args = args)
}

#' Whether a node is flagged as the graph output
#'
#' @param pg Process node list (argument name matches legacy code; not a full
#'   graph).
#'
#' @return Logical scalar.
#'
#' @keywords internal
pgraph_result <- function(pg) {
    !is.null(pg$result) && pg$result
}

#' Collapse user-defined-process parameters to \code{param()} formals
#'
#' @param pg Process graph list with `parameters` entry.
#'
#' @return Named list suitable for \code{make_fn()} formals.
#'
#' @keywords internal
pgraph_params <- function(pg) {
    unlist(
        lapply(pg$parameters, function(x) param(x$name, x$default)),
        recursive = FALSE,
        use.names = TRUE
    )
}

#' Turn a process graph into a single R expression
#'
#' @param pg A list passing \code{is_pgraph()}.
#' @param parent Optional parent environment for nested graphs.
#'
#' @return Language object (typically a nested `call`).
#'
#' @details
#' Uses \code{stopifnot(is_pgraph(pg))}. Builds an environment from
#' `process_graph` keys and starts from \code{starting_pnode()}.
#'
#' @keywords internal
pgraph_expr <- function(pg, parent = NULL) {
    stopifnot(is_pgraph(pg))
    if (is.null(parent)) {
        parent <- emptyenv()
    }
    pnode <- starting_pnode(pg)
    env <- list2env(pg$process_graph, parent = parent)
    pnode_expr(pnode, env = env)
}

#' Wrap \code{pgraph_expr()} as an executable function
#'
#' @param pg Process graph list.
#' @param parent Passed to \code{pgraph_expr()}.
#'
#' @return A `function` with formals from \code{pgraph_params()} and body from
#'   \code{pgraph_expr()}.
#'
#' @keywords internal
pgraph_fn <- function(pg, parent = NULL) {
    par <- pgraph_params(pg)
    expr <- pgraph_expr(pg, parent = parent)
    make_fn(par, body = expr, env = parent.frame())
}
