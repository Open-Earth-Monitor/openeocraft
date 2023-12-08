new_result <- function(description, schema) {
  list(
    description = description,
    schema = schema
  )
}

new_par <- function(name, description, schema, default, optional) {
  list(
    name = name,
    desciption = description,
    schema = schema,
    default = default,
    optional = optional
  )
}

new_proc <- function(id, summary, description, categories,
                     parameters, returns, links) {
  list(
    id = id,
    summary = summary,
    description = description,
    categories = categories,
    parameters = parameters,
    returns = returns,
    links = links
  )
}

is_pnode <- function(node) {
  if (!is.list(node) || is.null(names(node)))
    return(FALSE)
  if (!all(c("process_id", "arguments") %in% names(node)))
    return(FALSE)
  if (!is.character(node$process_id))
    return(FALSE)
  return(TRUE)
}

pgraph_result <- function(x) {
  !is.null(x$result) && x$result
}

is_pgraph <- function(p) {
  if (!is.list(p) || is.null(names(p)))
    return(FALSE)
  if (!"process_graph" %in% names(p))
    return(FALSE)
  if (!all(vapply(p$process_graph, is_pnode, logical(1))))
    return(FALSE)
  if (sum(vapply(p$process_graph, pgraph_result, logical(1))) != 1)
    return(FALSE)
  return(TRUE)
}

# pgraph_par_default <- function(p, parent = NULL) {
#   if (is.null(parent))
#     parent <- emptyenv()
#   par_env <- list2env(list(), parent = parent)
#   if (!"parameters" %in% names(p))
#     return(par_env)
#   for (par in p$parameters)
#     if ("default" %in% names(par) &&
#         !exists(par$name, envir = parent, inherits = TRUE))
#       assign(par$name, par$default, inherits = FALSE)
#   return(par_env)
# }

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
  switch(x, ..., stop("Not supported argument type."))
}


# TODO: Bind the execution of defined functions with the process graph execution
# TODO: Start the execution from the node with result = TRUE

get_starting_node <- function(p) {
  for (node in p$process_graph)
    if (pgraph_result(node))
      return(node)
}

run_args <- function(args, p_env) {
  lapply(args, function(arg) {
    arg_switch(
      arg,
      null = , character = , numeric = , logical = arg,
      array = , object = lapply(arg, run_args, p_env = p_env),
      result_reference = {
        value <- get(arg$from_node, envir = p_env, inherits = TRUE)
        value <- run_process(value, parent = p_env)
        assign(arg$from_node, value, envir = p_env, inherits = TRUE)
      },
      user_defined_process = run_process(arg, parent = p_env),
      parameter_reference = arg
    )
  })
}

run_node <- function(node, p_env) {
  args <- run_args(node$arguments, p_env = p_env)
  fn <- get(node$process_id, envir = p_env)
  do.call(fn, args = args, envir = p_env)
}

run_process <- function(p, parent = NULL) {
  stopifnot(is_pgraph(p))
  p_env <- list2env(p$process_graph, parent = parent)
  node <- get_starting_node(p$process_graph)
  run_node(node, p_env = p_env)
}

sits_cube <- function(id, bands, tiles, roi, start_date, end_date, platform) {

}
