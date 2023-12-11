.namespace <- new.env(hash = TRUE, parent = emptyenv())

reg_fn <- function(fn, parent = NULL) {
  fn_name <- gsub("^.+::", "", deparse(substitute(fn)))
  if (!is.null(parent))
    environment(fn) <- parent
  assign(fn_name, fn, envir = .namespace, inherits = FALSE)
}

reg_rlang <- function() {
  reg_fn(`{`)
  reg_fn(`(`)
  reg_fn(`::`)
  reg_fn(`$`)
  reg_fn(`[`)
  reg_fn(`[[`)
  reg_fn(`*`)
  reg_fn(`+`)
  reg_fn(`-`)
  reg_fn(`/`)
  reg_fn(`=`)
  reg_fn(`<-`)
  reg_fn(`==`)
  reg_fn(`<`)
  reg_fn(`>`)
  reg_fn(`<=`)
  reg_fn(`>=`)
  reg_fn(`!=`)
  reg_fn(`&`)
  reg_fn(`&&`)
  reg_fn(`|`)
  reg_fn(`||`)
  reg_fn(`!`)
  reg_fn(`if`)
  reg_fn(`for`)
  reg_fn(`function`)
  reg_fn(`return`)
  reg_fn(`list`)
  reg_fn(`substitute`)
  reg_fn(`is.null`)
  reg_fn(`is.list`)
  reg_fn(`body<-`)
  reg_fn(`length`)
  reg_fn(stats::runif)
}

reg_namespace <- function(file) {
  file <- system.file("R/namespace.R", package = "sitsopeneo")
  lines <- readLines(file)
  lines <- lines[which(grepl("^#\\*[ ]+@openeo", lines)) + 1]
  reg_fn(save_result, parent = .namespace)
  reg_fn(reduce_dimension, parent = .namespace)
  reg_fn(load_collection, parent = .namespace)
  reg_fn(multiply, parent = .namespace)
  reg_fn(divide, parent = .namespace)
  reg_fn(subtract, parent = .namespace)
  reg_fn(array_element, parent = .namespace)
  reg_fn(sum, parent = .namespace)
  reg_fn(min, parent = .namespace)
}

#* @openeo
save_result <- function(data, format, options = NULL) {
  list(data = data, format = format)
}

#* @openeo
reduce_dimension <- function(data, reducer, dimension, context = NULL) {
  reducer_fn <- function(data, context = NULL) {}
  body(reducer_fn) <- substitute(reducer)
  reducer_fn(data, context = context)
}

#* @openeo
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

#* @openeo
multiply <- function(x, y) {
  x * y
}

#* @openeo
divide <- function(x, y) {
  x / y
}

#* @openeo
subtract <- function(x, y) {
  x - y
}

#* @openeo
array_element <- function(data, index = NULL, label = NULL,
                          return_nodata = FALSE) {
  if (!is.null(index))
    return(data[[index]])
  if (!is.null(label))
    return(data[[label]])
  data
}

#* @openeo
sum <- function(data, ignore_nodata = TRUE) {
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

#* @openeo
min <- function(data, ignore_nodata = TRUE) {
  base::min(data)
}

reg_rlang()
