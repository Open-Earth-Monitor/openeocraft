is_pnode <- function(node) {
  if (!is.list(node) || is.null(names(node)))
    return(FALSE)
  if (!all(c("process_id", "arguments") %in% names(node)))
    return(FALSE)
  if (!is.character(node$process_id))
    return(FALSE)
  return(TRUE)
}

is_pgraph <- function(pg) {
  if (!is.list(pg) || is.null(names(pg)))
    return(FALSE)
  if (!"process_graph" %in% names(pg))
    return(FALSE)
  if (!all(vapply(pg$process_graph, is_pnode, logical(1))))
    return(FALSE)
  if (base::sum(vapply(pg$process_graph, pgraph_result, logical(1))) != 1)
    return(FALSE)
  return(TRUE)
}

arg_type <- function(x) {
  if (is.null(x))
    return("null")
  if (is.character(x))
    return("character")
  if (is.numeric(x))
    return("numeric")
  if (is.logical(x))
    return("logical")
  if (is.list(x)) {
    if (is.null(names(x)))
      return("array")
    if ("from_node" %in% names(x))
      return("result_reference")
    if ("process_graph" %in% names(x))
      return("user_defined_process")
    if ("from_parameter" %in% names(x))
      return("parameter_reference")
    return("object")
  }
}

arg_switch <- function(x, ...) {
  switch(arg_type(x), ..., stop("Not supported argument type."))
}

list_expr <- function(args, env) {
  args <- pnode_args(args, env = env)
  as.call(c(as.name("list"), args))
}

starting_pnode <- function(pg) {
  for (node in pg$process_graph)
    if (pgraph_result(node))
      return(node)
}

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

pnode_expr <- function(node, env) {
  args <- pnode_args(node$arguments, env = env)
  as_call(node$process_id, args = args)
}

pgraph_result <- function(pg) {
  !is.null(pg$result) && pg$result
}

pgraph_params <- function(pg) {
  unlist(
    lapply(pg$parameters, function(x) param(x$name, x$default)),
    recursive = FALSE,
    use.names = TRUE
  )
}

pgraph_expr <- function(pg, parent = NULL) {
  stopifnot(is_pgraph(pg))
  if (is.null(parent))
    parent <- emptyenv()
  pnode <- starting_pnode(pg)
  env <- list2env(pg$process_graph, parent = parent)
  pnode_expr(pnode, env = env)
}

pgraph_fn <- function(pg, parent = NULL) {
  par <- pgraph_params(pg)
  expr <- pgraph_expr(pg, parent = parent)
  make_fn(par, body = expr, env = parent.frame())
}

run_pgraph <- function(api, req, user, job, pg) {
  if ("process" %in% names(pg))
    pg <- pg$process
  expr <- pgraph_expr(pg)
  # TODO: need to define a scope with api and user objects
  eval(expr, envir = list(api = api, user = user, job = job, req = req),
       enclos = get_namespace(api))
}
