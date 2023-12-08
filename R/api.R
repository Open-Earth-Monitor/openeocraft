.openeo <- new.env()

#* @apiTitle Plumber Example API

#* Information about the back-end
#* @get /
function() {
  my_endpoint <- "/"
  list(
    api_version = "1.2.0",
    backend_version = "0.1.0",
    stac_version = "1.0.0",
    type = "Catalog",
    id = "sits-openeo",
    title = "SITS implementation on openEO",
    description = "This is the implmentation of SITS package on openEO framework.",
    production = FALSE,
    endpoints = list_endpoints(),
    links = list(
      href = paste0(get_host(), ":", get_port(), my_endpoint),
      rel = "self"
    )
  )
}

list_collections <- function() {
  collections <- list()
  collections_id <- character()
  for (source in sits:::.sources()) {
    for (collection in sits:::.source_collections(source)) {
      platform <- sits:::.source_collection_satellite(source, collection)
      instrument <- sits:::.source_collection_sensor(source, collection)
      collections <- c(collections, list(
        list(
          id = paste(source, collection, sep = "/"),
          title = paste0(platform, " (", instrument, ")"),
          constellation = paste(platform, instrument, sep = "/"),
          description = "(Include a description on sits collections metadata)"
        )
      ))
      collections_id <- c(collections_id, paste(source, collection, sep = "/"))
    }
  }
  names(collections) <- collections_id
  assign("collections", collections, .openeo)
}

#* Basic metadata for all datasets
#* @serializer unboxedJSON
#* @get /collections
function() {
  my_endpoint <- "/collections"
  if (!exists("collections", envir = .openeo, inherits = FALSE))
    list_collections()
  collections <- list(
    collections = unname(get("collections", envir = .openeo, inherits = FALSE)),
    links = list(list(
      href = paste0(get_host(), ":", get_port(), my_endpoint),
      rel = "self"
    ))
  )
  collections
}

#* Full metadata for a specific dataset
#* @param collection_id Collection identifier
#* @serializer unboxedJSON
#* @get /collections/<collection_id>
function(collection_id) {
  collection_id <- URLdecode(collection_id)
  my_endpoint <- paste("/collections", collection_id, sep = "/")
  if (!exists("collections", envir = .openeo, inherits = FALSE))
    list_collections()
  collections <- get("collections", envir = .openeo, inherits = FALSE)
  stopifnot(collection_id %in% names(collections))
  collection <- collections[[collection_id]]
  list(
    stac_version = "1.0.0",
    stac_extensions = list(),
    id = collection$id,
    title = collection$title,
    description = collection$description,
    license = "proprietary",
    extent = list(
      spatial = list(bbox = unname(collection$bbox)),
      temporal = list(interval = list(unname(collection$interval)))
    ),
    links = list(
      list(
        rel = "root",
        href = paste0(get_host(), ":", get_port(), "/collections")
      ),
      list(
        rel = "self",
        href = paste0(get_host(), ":", get_port(), my_endpoint)
      )
    ),
    `cube:dimensions` = list(
      x = list(
        type = "spatial",
        axis = "x",
        extent = list(collection$bbox$xmin, collection$bbox$xmax)
      ),
      y = list(
        type = "spatial",
        axis = "y",
        extent = list(collection$bbox$ymin, collection$bbox$ymax)
      ),
      t = list(
        type = "temporal",
        extent = list(collection$interval$start_date, collection$interval$end_date)
      ),
      bands = list(
        type = "bands",
        values = list(collection$bands)
      )
    ),
    summaries = list(
      constellation = list(collection$constelation),
      `eo:bands` = list()
    )
  )
}

#* HTTP Basic authentication
#* @get /credentials/basic
function() {
  token = "b34ba2bdf9ac9ee1"
  list(access_token = token)
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

pgraph_par_default <- function(p, parent = NULL) {
  if (is.null(parent))
    parent <- emptyenv()
  par_env <- list2env(list(), parent = parent)
  if (!"parameters" %in% names(p))
    return(par_env)
  for (par in p$parameters)
    if ("default" %in% names(par) &&
        !exists(par$name, envir = parent, inherits = TRUE))
      assign(par$name, par$default, inherits = FALSE)
  return(par_env)
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
  stop("Not supported argument type.")
}

arg_switch <- function(x, ...) {
  switch(x, ..., stop("Not supported argument type."))
}


# TODO: Bind the execution of defined functions with the process graph execution
# TODO: Start the execution from the node with result = TRUE

# ns_env <- list2env(
#   list(
#     load_collection = list(...),
#     save_result = list(...),
#     ml_fit_class_random_forest = list(...)
#   )
# )
#
# run_process <- function(p, parent = NULL) {
#   stopifnot(is_pgraph(p))
#   p_env <- pgraph_par_default(p, parent = parent)
#   p_env <- list2env(p$process_graph, envir = p_env)
#   for (node in p$process_graph) {
#     for (arg in node$argument)
#       # Maybe this is just a check?
#       arg <- arg_switch(
#         arg,
#         null = , character = , numeric = , logical = arg,
#         array = , object = lapply(arg, run_process, parent = p_env),
#         result_reference = run_process(
#           get(arg$from_node, envir = p_env, inherits = TRUE),
#           parent = p_env
#         ),
#         user_defined_process = run_process(arg, parent = p_env),
#         parameter_reference = arg
#       )
#   }
#   # Start the execution from the result = TRUE node
#   node <- get_starting_node(p)
#   for (arg in node$argument)
#     # Maybe this is just a check?
#     arg <- arg_switch(
#       arg,
#       null = , character = , numeric = , logical = arg,
#       array = , object = lapply(arg, run_process, parent = p_env),
#       result_reference = ...,
#       user_defined_process = ...,
#       parameter_reference = ...
#     )
#   ...
# }

#* Process and download data synchronously
#* @post /result
function(req, res) {
  process <- req$body$process

}
