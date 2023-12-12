#* @openeo save_result
function(data, format, options = NULL) {
  list(data = data, format = format)
}


#* @openeo reduce_dimension
function(data, reducer, dimension, context = NULL) {
  reducer_fn <- function(data, context = NULL) {}
  body(reducer_fn) <- substitute(reducer)
  reducer_fn(data, context = context)
}

#* @openeo load_collection
function(id, spatial_extent = NULL, temporal_extent = NULL,
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

#* @openeo multiply
function(x, y) {
  x * y
}

#* @openeo divide
function(x, y) {
  x / y
}

#* @openeo subtract
function(x, y) {
  x - y
}

#* @openeo array_element
function(data, index = NULL, label = NULL,
                          return_nodata = FALSE) {
  if (!is.null(index))
    return(data[[index]])
  if (!is.null(label))
    return(data[[label]])
  data
}

#* @openeo sum
function(data, ignore_nodata = TRUE) {
  if (is.list(data) && length(data) > 0) {
    result <- NULL
    for (value in data) {
      if (is.null(result))
        result <- value
      else
        result <- result + value
    }
  } else if (!is.list(data)) {
    result <- base::sum(result)
  } else
    result <- data
  result
}

#* @openeo min
function(data, ignore_nodata = TRUE) {
  base::min(data)
}
