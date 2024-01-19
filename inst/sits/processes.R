# TODO allow many files processes definition
#* @openeo-import math.R
#* @openeo-import data.R

#* @openeo-process
save_result <- function(data, format, options = NULL) {
  return(list(data = data, format = format))
  saveRDS(list(data = data, format = format), "result.rds")
  # TRUE <-- this is not true
}

# this is a constant
my_constant <- 10

#* @openeo-process
reduce_dimension <- function(data, reducer, dimension, context = NULL) {
  reducer_fn <- function(data, context = NULL) {}
  body(reducer_fn) <- substitute(reducer)
  reducer_fn(data, context = context)
}

#* @openeo-process
load_collection <- function(id, spatial_extent = NULL, temporal_extent = NULL,
                            bands = NULL, properties = NULL) {
  list(
    B01 = runif(10),
    B02 = runif(10),
    B03 = runif(10),
    B04 = runif(10),
    B05 = runif(10),
    B06 = runif(10),
    B07 = runif(10),
    B08 = runif(10)
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
array_element <- function(data, index = NULL, label = NULL,
                          return_nodata = FALSE) {
  if (!is.null(index))
    return(data[[index]])
  if (!is.null(label))
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
  if (is.list(data) && length(data) > 0) {
    result <- NULL
    for (value in data) {
      if (is.null(result))
        result <- eval(value, envir = parent.frame())
      else {
        result <- result + eval(value, envir = parent.frame())
      }
    }
  } else if (!is.list(data)) {
    result <- base::sum(result)
  } else
    result <- data
  result
}

#* @openeo-process
min <- function(data, ignore_nodata = TRUE) {
  base::min(data)
}
