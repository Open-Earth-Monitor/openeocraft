.namespace <- list2env(list(
  `::` = `::`,
  `$` = `$`,
  `{` = `{`,
  `(` = `(`,
  `[` = `[`,
  `[[` = `[[`,
  `*` = `*`,
  `+` = `+`,
  `-` = `-`,
  `/` = `/`,
  `=` = `=`,
  `==` = `==`,
  `<` = `<`,
  `>` = `>`,
  `<=` = `<=`,
  `>=` = `>=`,
  `!=` = `!=`,
  `&` = `&`,
  `&&` = `&&`,
  `|` = `|`,
  `||` = `||`,
  `!` = `!`,
  `<-` = `<-`,
  `if` = `if`,
  `for` = `for`,
  `function` = `function`,
  `return` = `return`,
  `list` = `list`,
  `substitute` = `substitute`,
  `is.null` = `is.null`,
  `is.list` = `is.list`,
  `body<-` = `body<-`,
  `length` = `length`,
  `print` = `print`,
  `eval` = `eval`,
  `runif` = stats::runif
), parent = emptyenv())

add_fn <- function(fn) {
  fn_name <- deparse(substitute(fn))
  environment(fn) <- .namespace
  assign(fn_name, fn, envir = .namespace, inherits = FALSE)
}

save_result <- function(data, format, options = NULL) {
  list(data = data, format = format)
}

reduce_dimension <- function(data, reducer, dimension, context = NULL) {
  reducer_fn <- function(data, context = NULL) {}
  body(reducer_fn) <- substitute(reducer)
  reducer_fn(data, context = context)
}

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

multiply <- function(x, y) {
  x * y
}

divide <- function(x, y) {
  x / y
}

subtract <- function(x, y) {
  x - y
}

array_element <- function(data, index = NULL, label = NULL,
                         return_nodata = FALSE) {
  if (!is.null(index))
    return(data[[index]])
  if (!is.null(label))
    return(data[[label]])
  data
}

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

min <- function(data, ignore_nodata = TRUE) {
  base::min(data)
}

add_fn(save_result)
add_fn(reduce_dimension)
add_fn(load_collection)
add_fn(multiply)
add_fn(divide)
add_fn(subtract)
add_fn(array_element)
add_fn(sum)
add_fn(min)
