#' @importFrom plumber register_serializer
#' @importFrom plumber serializer_json
#' @importFrom plumber serializer_rds

plumb_reg_serializers <- function() {
  # register serialize function
  plumber::register_serializer("serialize_result", function() {
    function(val, req, res, errorHandler) {
      fn <- get_serializer(val)
      fn(val$data, req, res, errorHandler)
    }
  })
}

get_serializer <- function(data) {
  UseMethod("get_serializer", data)
}

#' @export
get_serializer.openeo_json <- function(data) {
  plumber::serializer_json()
}

#' @export
get_serializer.openeo_gtiff <- function(data) {
  # TODO: implement GTiff serializer
}

#' @export
get_serializer.openeo_rds <- function(data) {
  plumber::serializer_rds(version = "3")
}
