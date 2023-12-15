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

plumb_api <- function(api, envir) {
  api_file <- system.file("R/api.R", package = "openeocraft")
  plumb <- plumber::pr(api_file, envir = envir)
  set_attr(api, "plumb", plumb)
  plumber::pr_run(plumb, host = get_host(api), port = get_port(api))
}

get_plumb <- function(api) {
  get_attr(api, "plumb")
}

load_processes <- function(api, processes_file) {
  stopifnot(file.exists(processes_file))
  processes <- new.env(parent = emptyenv())
  set_attr(api, "processes", processes)
  # TODO: split environments
  load_rlang(processes)
  eval(parse(processes_file, encoding = "UTF-8"), envir = processes)
  run_openeo_decorators(api, processes_file)
  api
}

get_processes <- function(api) {
  get_attr(api, "processes")
}

load_rlang <- function(processes) {
  reg_fn <- function(...) {
    fn_list <- list(...)
    for (fn in fn_list) {
      value <- eval(as.name(fn), envir = environment())
      assign(fn, value, envir = processes, inherits = FALSE)
    }
  }
  reg_fn(":::", "::", ":")
  reg_fn("{", "(")
  reg_fn("=", "<-", "<<-")
  reg_fn("$", "[", "[[")
  reg_fn("*", "+", "-", "/", "%%", "%/%", "%*%")
  reg_fn("==", "<", ">", "<=", ">=", "!=")
  reg_fn("&", "&&", "|", "||", "!")
  reg_fn("if", "for", "while", "repeat", "break", "next")
  reg_fn("function", "return", "body<-")
  reg_fn("substitute", "list", "length", "is.null", "is.list")
  reg_fn("runif", "eval", "environment", "print", "browser", "parent.frame")
  invisible(NULL)
}

get_host <- function(api) {
  get_attr(api, "host")
}

get_port <- function(api) {
  get_attr(api, "port")
}

get_endpoint <- function(api, path) {
  # TODO: how to get the schema of the running server?
  paste0(get_host(api), ":", get_port(api), path)
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
  set_attr(api, "host", host)
  set_attr(api, "port", port)
  plumb_api(api, envir = environment())
}
