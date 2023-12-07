.conf <- new.env()

create_api <- function() {
  api_file <- system.file("R/api.R", package = "sitsopeneo")
  api <- plumber::plumb(api_file)
  assign("api", api, envir = .conf)
  api
}

get_api <- function() {
  if (exists("api", .conf, inherits = FALSE))
    get("api", envir = .conf, inherits = FALSE)
}

get_host <- function() {
  if (exists("host", .conf, inherits = FALSE))
    get("host", envir = .conf, inherits = FALSE)
}

get_port <- function() {
  if (exists("port", .conf, inherits = FALSE))
    get("port", envir = .conf, inherits = FALSE)
}

load_users <- function() {
  users <- list(
    rolf = list(
      password = "123456"
    ),
    brian = list(
      password = "123456"
    )
  )
  assign("users", users, envir = .conf)
}

authenticate_user <- function(user, password) {
  users <- get("users", envir = .conf, inherits = FALSE)
  stopifnot(user %in% names(users))
  stopifnot(password == users[[user]]$password)

}

run <- function(host, port) {
  api <- create_api()
  load_users()
  assign("host", host, envir = .conf)
  assign("port", port, envir = .conf)
  api$run(host = host, port = port)
}
