#' @export
create_api <- function(id, title, description, backend_version, api_version) {
  plumb_reg_serializers()
  structure(
    list(
      id = id,
      title = title,
      description = description,
      backend_version = backend_version,
      api_version = api_version
    ),
    class = c(id, "openeo_api"), # TODO: choose a better class name!
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

get_endpoints <- function(api) {
  get_attr(api, "endpoints")
}

#' @export
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
  export_fn("=", "<-", "<<-", "$<-", "[<-", "[[<-")
  export_fn("$", "[", "[[")
  export_fn("*", "+", "-", "/", "%%", "%/%", "%*%")
  export_fn("==", "<", ">", "<=", ">=", "!=")
  export_fn("&", "&&", "|", "||", "!")
  export_fn("if", "for", "while", "repeat", "break", "next")
  export_fn("function", "return")
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
