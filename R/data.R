data_serializer <- function(x, res) {
  UseMethod("data_serializer", x)
}
#' @export
data_serializer.openeo_json <- function(x, res) {
  res$setHeader("Content-Type", "application/json")
  res$body <- x$data
  res
}
#' @export
data_serializer.openeo_gtiff <- function(x, res) {
  res$setHeader("Content-Type", "image/tiff")
  res$body <- readBin(x$data, what = "raw", n = file.info(x$data)$size)
  res
}
#' @export
data_serializer.openeo_netcdf <- function(x, res) {
  res$setHeader("Content-Type", "application/octet-stream")
  res$body <- readBin(x$data, what = "raw", n = file.info(x$data)$size)
  res
}

#' @export
data_serializer.openeo_rds <- function(x, res) {
  res$setHeader("Content-Type", "application/rds")
  res$body <- readBin(x$data, what = "raw", n = file.info(x$data)$size)
  res
}
