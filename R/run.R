create_api <- function() {
  api_file <- system.file("R/api.R", package = "sitsopeneo")
  api <- plumber::plumb(api_file)
  assign("api", api, envir = .openeo)
  api
}

get_api <- function() {
  if (exists("api", .openeo, inherits = FALSE))
    get("api", envir = .openeo, inherits = FALSE)
}

get_host <- function() {
  if (exists("host", .openeo, inherits = FALSE))
    get("host", envir = .openeo, inherits = FALSE)
}

get_port <- function() {
  if (exists("port", .openeo, inherits = FALSE))
    get("port", envir = .openeo, inherits = FALSE)
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
  assign("users", users, envir = .openeo)
}

authenticate_user <- function(user, password) {
  users <- get("users", envir = .openeo, inherits = FALSE)
  stopifnot(user %in% names(users))
  stopifnot(password == users[[user]]$password)

}

run <- function(host, port) {
  api <- create_api()
  load_users()
  assign("host", host, envir = .openeo)
  assign("port", port, envir = .openeo)
  api$run(host = host, port = port)
}
