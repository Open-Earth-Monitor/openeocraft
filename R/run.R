api <- NULL

create_api <- function(id, title, description, backend_version) {
  structure(
    list(
      id = id,
      title = title,
      description = description,
      backend_version = backend_version,
      api_version = api_version
    ),
    class = c(id, "openeo_api"),
    env = new.env(hash = TRUE, parent = parent.frame())
  )
}

get_env <- function(api) {
  attr(api, "env")
}

get_attr <- function(api, name) {
  if (exists(name, envir = get_env(api)))
    get(name, envir = get_env(api), inherits = FALSE)
}

set_attr <- function(api, name, value) {
  assign(name, value, envir = get_env(api), inherits = FALSE)
  api
}

start_api <- function(api, envir) {
  api_file <- system.file("R/api.R", package = "openeocraft")
  plumb <- plumber::pr(api_file, envir = envir)
  set_plumb(api, plumb)
  plumber::pr_run(plumb, host = get_host(api), port = get_port(api))
}

get_plumb <- function(api) {
  get_attr(api, "plumb")
}

set_plumb <- function(api, plumb) {
  set_attr(api, "plumb", plumb)
}

load_processes <- function(api, processes_file) {
  stopifnot(file.exists(processes_file))
  namespace <- new.env(parent = emptyenv())
  set_attr(api, "namespace", namespace)
  # TODO: split environments
  load_rlang(api)
  eval(parse(processes_file, encoding = "UTF-8"), envir = namespace)
  set_attr(api, "processes", list())
  process_decorators(api, processes_file, decorator = "openeo-process")
  api
}

get_namespace <- function(api) {
  get_attr(api, "namespace")
}

set_namespace <- function(api, namespace) {
  set_attr(api, "namespace", namespace)
}

get_processes <- function(api) {
  processes <- get_attr(api, "processes")
  processes_list <- list(
    processes = unname(processes),
    links = list(
      new_link(
        rel = "self",
        href = get_endpoint(api, "/processes")
      )
    )
  )
  processes_list
}

add_process <- function(api, process) {
  processes <- get_attr(api, "processes")
  processes[[process$id]] <- process
  set_attr(api, "processes", processes)
}

load_rlang <- function(api) {
  export_fn <- function(...) {
    fn_list <- list(...)
    for (fn in fn_list) {
      value <- eval(as.name(fn), envir = environment())
      assign(fn, value, envir = get_namespace(api), inherits = FALSE)
    }
    invisible(NULL)
  }
  export_fn(":::", "::", ":")
  export_fn("{", "(")
  export_fn("=", "<-", "<<-")
  export_fn("$", "[", "[[")
  export_fn("*", "+", "-", "/", "%%", "%/%", "%*%")
  export_fn("==", "<", ">", "<=", ">=", "!=")
  export_fn("&", "&&", "|", "||", "!")
  export_fn("if", "for", "while", "repeat", "break", "next")
  export_fn("function", "return", "body<-")
  export_fn("substitute", "list", "length", "is.null", "is.list")
  export_fn("runif", "eval", "environment", "print", "browser", "parent.frame")
}

get_scheme <- function(api) {
  get("scheme", envir = get_env(api), inherits = FALSE)
}

get_host <- function(api) {
  get("host", envir = get_env(api), inherits = FALSE)
}

get_port <- function(api) {
  get("port", envir = get_env(api), inherits = FALSE)
}

get_endpoint <- function(api, path) {
  paste0(get_scheme(api), "://", get_host(api), ":", get_port(api), path)
}

load_users <- function(api, users_file) {
  # TODO: implement secure password file
  users <- list(
    rolf = list(
      password = "123456"
    ),
    brian = list(
      password = "123456"
    )
  )
  set_attr(api, "users", users)
  api
}

authenticate_user <- function(api, user, password) {
  # TODO: implement user authentication
  users <- get_attr(api, "users")
  stopifnot(user %in% names(users) || password == users[[user]]$password)
}

run_api <- function(api, host, port) {
  stopifnot(grepl("^.+://", host))
  set_attr(api, "scheme", gsub("://.*$", "", host))
  set_attr(api, "host", gsub("^.+://", "", host))
  set_attr(api, "port", port)
  start_api(api, envir = environment())
}
