call_decorator <- function(decor, src, file, line) {
  decor <- gsub("\\s+", " ", gsub("^[^@]+@", "", decor))
  decor <- as.list(strsplit(decor, " ")[[1]])
  expr <- tryCatch({
    parse(text = src)[[1]]
  },
  error = function(e) {
    stop("Cannot parse code at ", basename(file), "@", line)
  })
  eval(do.call(call, args = c(list(decor[[1]], list(expr)), decor[-1])))
}

load_decorators <- function(file, pattern) {
  src <- readLines(file)
  pattern <- paste0("^\\s*#\\*\\s+@", pattern, "\\s?")
  lines <- c(which(grepl(pattern, src)), length(src))
  for (i in seq_len(length(lines) - 1)) {
    call_decorator(
      decor = src[lines[[i]]],
      src = src[seq(lines[[i]], lines[[i + 1]])],
      file = file,
      line = lines[[i]]
    )
  }
}

load_openeo_decorators <- function(file) {
  load_decorators(file, "openeo")
}

# decorator
openeo <- function(expr, fn_name = NULL) {
  # extract from list
  expr <- expr[[1]]
  # isolate eval
  .eval <- function(expr)
    eval(expr, envir = environment())
  # check value
  value <- .eval(expr)
  if (!"function" %in% class(value))
    stop("Cannot define API function on a value of class '",
         class(value)[[1]], "'. Expecting a `function`.")
  # anonymous function
  if ((as.character(expr[[1]]) == "function") && (is.null(fn_name)))
    stop("Cannot define unknamed API function.")
  if (is.null(fn_name))
    fn_name <- as.character(expr[[2]])
  environment(value) <- get_procs()
  assign(fn_name, value, envir = get_procs(), inherits = FALSE)
}
