#' @importFrom plumber register_serializer
#' @importFrom plumber serializer_json
#' @importFrom plumber serializer_rds
NULL

#' Register custom plumber serializers for openEO result wrappers
#'
#' @description
#' Registers `"serialize_result"`, which dispatches on `val$data` via
#' `get_serializer()`.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
plumb_reg_serializers <- function() {
    # register serialize function
    plumber::register_serializer("serialize_result", function() {
        function(val, req, res, error_handler) {
            fn <- get_serializer(val)
            fn(val$data, req, res, error_handler)
        }
    })
}

#' Choose a plumber serializer for an openEO result object
#'
#' @param data S3 object carrying openEO format metadata (class determines JSON,
#'   RDS, etc.).
#'
#' @return A serializer **generator** function compatible with plumber
#'   (`function() { function(val, req, res, error_handler) { ... } }` style
#'   for `serialize_result`).
#'
#' @keywords internal
get_serializer <- function(data) {
    UseMethod("get_serializer", data)
}

#' @describeIn get_serializer JSON body via \code{plumber::serializer_json()}
#' @param data Result object with class `openeo_json`.
#' @export
get_serializer.openeo_json <- function(data) {
    plumber::serializer_json()
}

#' @describeIn get_serializer GeoTIFF serializer (not yet implemented)
#' @param data Result object with class `openeo_gtiff`.
#' @export
get_serializer.openeo_gtiff <- function(data) {
    # TODO: implement GTiff serializer
}

#' @describeIn get_serializer Native R RDS via \code{plumber::serializer_rds()}
#' @param data Result object with class `openeo_rds`.
#' @export
get_serializer.openeo_rds <- function(data) {
    plumber::serializer_rds(version = "3")
}
