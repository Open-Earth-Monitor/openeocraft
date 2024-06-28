# TODO allow many files processes definition
#* @openeo-import math.R
#* @openeo-import data.R

#* @openeo-process
save_result <- function(data, format, options = NULL) {
  format <- base::tolower(format)
  supported_formats <- openeocraft::file_formats()
  outputFormats <- supported_formats$output
  if (!(format %in% base::tolower(base::names(outputFormats)))) {
    stop(base::paste("Format", format, "is not supported."))
  }
  work_dir <- openeocraft::user_workspace()
  filename <- base::paste0("result", openeocraft::format_ext(format))
  filename <- base::file.path(work_dir, filename)
  if (format == "rds") {
    base::saveRDS(data, filename)
  } else {
    stars::write_stars(data, filename)
  }
  base::structure(
    base::list(data = filename, format = format, options = options),
    class = base::c(base::paste0("openeo_", format))
  )
}

#* @openeo-process
reduce_dimension <- function(data, reducer, dimension, context = NULL) {
  reducer_fn <- function(data, context) {}
  base::body(reducer_fn) <- base::substitute(reducer)
  if (base::is.character(dimension))
    dimension <- base::match(dimension, base::dimnames(data))
  dims <- base::setdiff(base::seq_along(base::dimnames(data)), dimension)
  dimnames <- stars::st_dimensions(data)[[dimension]]$values
  stars::st_apply(data, dims, \(x) {
    base::names(x) <- dimnames
    reducer_fn(x, context = context)
  })
}

#* @openeo-process
load_collection <- function(id,
                            spatial_extent = NULL,
                            temporal_extent = NULL,
                            bands = NULL,
                            properties = NULL) {
  mock_img <- function(bands, size, origin, res, ...) {
    data <- base::data.frame(
      x = base::rep(
        base::rep(
          base::seq(from = origin[[1]], by = res[[1]], length.out = size[[1]]),
          each = size[[2]]
        ),
        times = base::length(bands)
      ),
      y = base::rep(
        base::rep(
          base::seq(from = origin[[2]], by = res[[2]], length.out = size[[2]]),
          times = size[[1]]
        ),
        times = base::length(bands)
      ),
      bands = base::rep(bands, each = size[[1]] * size[[2]])
    )
    extra <- base::as.data.frame(base::list(...))
    if (base::nrow(extra))
      data <- base::cbind(data, extra)
    data$values <- stats::runif(size[[1]] * size[[2]] * base::length(bands))
    data
  }

  spatial_extent <- base::c(
    west = -45.0,
    east = -45.0 * 10 * 0.009,
    south = -10.0 * 10 * -0.009,
    north = -10.0
  )
  temporal_extent <- base::c(
    t0 = "2020-01-01",
    t1 = "2021-01-01"
  )
  bands <- base::c("B02", "B04", "B08")

  stars::st_as_stars(
    base::rbind(
      mock_img(
        bands = bands,
        size = base::c(10, 10),
        origin = base::c(-45.0, -10.0),
        res = base::c(0.009, 0.009),
        t = "2020-01-01"
      ),
      mock_img(
        bands = bands,
        size = base::c(10, 10),
        origin = base::c(-45.0, -10.0),
        res = base::c(0.009, 0.009),
        t = "2021-01-01"
      )
    ),
    dims = 1:4,
    xy = 1:2,
    y_decreasing = TRUE,
    coords = 1:4
  )
}

#* @openeo-process
multiply <- function(x, y) {
  x * y
}

#* @openeo-process
divide <- function(x, y) {
  x / y
}

#* @openeo-process
subtract <- function(x, y) {
  x - y
}

#* @openeo-process
add <- function(x, y) {
  x + y
}

#* @openeo-process
array_element <- function(data, index = NULL, label = NULL,
                          return_nodata = FALSE) {
  if (!base::is.null(index))
    return(data[[index]])
  if (!base::is.null(label))
    return(data[[label]])
  data
}

# TODO: implement extended annotations
#* @openeo-process sum
#*
#* This is the title
#*
#* This is the first line of the description
#* This is the second line of the description
#*
#* This is the third line of the description and the
#* previous line is a blank line.
#*
#* @param data string
#* @param ignore_nodata logical
#* @return data_cube
sum <- function(data, ignore_nodata = TRUE) {
  base::sum(base::unlist(data), na.rm = ignore_nodata)
}

#* @openeo-process
min <- function(data, ignore_nodata = TRUE) {
  base::min(base::unlist(data), na.rm = ignore_nodata)
}
