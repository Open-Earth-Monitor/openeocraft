
get_serializer_fn <- function(data) {
  UseMethod("get_serializer_fn", data)
}

#' @export
get_serializer_fn.openeo_json <- function(data) {
  plumber::serializer_json()
}

#' @export
get_serializer_fn.openeo_gtiff <- function(data) {
  # TODO: implement GTiff serializer
}

#' @export
get_serializer_fn.openeo_rds <- function(data) {
  plumber::serializer_rds(version = "3")
}
