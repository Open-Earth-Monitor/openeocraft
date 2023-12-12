create_api <- function(api_file) {
  api <- plumber::plumb(api_file)
  assign("api", api, envir = .openeo, inherits = FALSE)
  api
}

get_api <- function() {
  get("api", envir = .openeo, inherits = FALSE)
}

load_procs <- function(procs_file) {
  procs <- new.env(parent = emptyenv())
  assign("procs", procs, envir = .openeo, inherits = FALSE)
  load_rlang(procs)
  load_openeo_decorators(procs_file)
}

get_procs <- function() {
  get("procs", envir = .openeo, inherits = FALSE)
}

load_rlang <- function(env) {
  reg_fn <- function(...) {
    fn_list <- list(...)
    for (fn in fn_list)
      assign(fn, eval(as.name(fn)), envir = env, inherits = FALSE)
  }
  reg_fn(":::", "::")
  reg_fn("{", "(")
  reg_fn("$", "[", "[[")
  reg_fn("*", "+", "-", "/")
  reg_fn("=", "<-")
  reg_fn("==", "<", ">", "<=", ">=", "!=")
  reg_fn("&", "&&", "|", "||", "!")
  reg_fn("if", "for")
  reg_fn("function", "return", "body<-")
  reg_fn("substitute", "list", "length", "is.null", "is.list")
  reg_fn("runif")
  invisible(NULL)
}

get_host <- function() {
  get("host", envir = .openeo, inherits = FALSE)
}

get_port <- function() {
  get("port", envir = .openeo, inherits = FALSE)
}

load_users <- function(users_file) {
  # TODO: implement secure password file
  users <- list(
    rolf = list(
      password = "123456"
    ),
    brian = list(
      password = "123456"
    )
  )
  assign("users", users, envir = .openeo, inherits = FALSE)
}

authenticate_user <- function(user, password) {
  # TODO: implement user authentication
  users <- get("users", envir = .openeo, inherits = FALSE)
  stopifnot(user %in% names(users))
  stopifnot(password == users[[user]]$password)
}

run <- function(host, port, users_file = NULL) {
  assign("host", host, envir = .openeo, inherits = FALSE)
  assign("port", port, envir = .openeo, inherits = FALSE)
  load_users(users_file)
  load_collections()
  procs_file <- system.file("R/procs.R", package = "sitsopeneo")
  api_file <- system.file("R/api.R", package = "sitsopeneo")
  load_procs(procs_file)
  api <- create_api(api_file)
  api$run(host = host, port = port)
}
