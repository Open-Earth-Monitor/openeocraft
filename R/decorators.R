run_decorator <- function(decor, src, file, line) {
  decor <- gsub("\\s+", " ", gsub("^[^@]+@", "", decor))
  decor <- as.list(strsplit(decor, " ")[[1]])
  expr <- tryCatch({
    parse(text = src)[[1]]
  },
  error = function(e) {
    stop("Cannot parse code at ", basename(file), "@", line)
  })
  args <- c(list(decor[[1]], list(expr)), decor[-1], list(file = file))
  eval(do.call(call, args = args))
}

run_decorators <- function(api, file, pattern) {
  src <- readLines(file)
  pattern <- paste0("^\\s*#\\*\\s+@", pattern, "\\s?")
  lines <- c(which(grepl(pattern, src)), length(src))
  for (i in seq_len(length(lines) - 1)) {
    run_decorator(
      decor = src[lines[[i]]],
      src = src[seq(lines[[i]], lines[[i + 1]])],
      file = file,
      line = lines[[i]]
    )
  }
}

run_openeo_decorators <- function(api, file) {
  run_decorators(api, file, "openeo-process")
  # run_decorators(api, file, "param")
}

# decorators
empty_name <- function() {
  formals(function(x) {})[["x"]]
}

`openeo-process` <- function(expr, ..., file) {
  # extract from list
  expr <- expr[[1]]
  fn_name <- as.character(expr[[2]])
  arg_names <- names(expr[[3]][[2]])
  arg_defaults <- unname(expr[[3]][[2]])
  process <- list(
    id = fn_name,
    parameters = lapply(seq_along(arg_names), function(i) {
      if (is.null(arg_defaults[[i]]) || (arg_defaults[[i]] != empty_name())) {
        return(list(
          name = arg_names[[i]],
          default = arg_defaults[[i]],
          optional = TRUE
        ))
      }
      list(
        name = arg_names[[i]],
        optional = FALSE
      )
    }))
  dir <- dirname(path.expand(file))
  process_file <- file.path(dir, paste0(fn_name, ".json"))
  if (!file.exists(process_file))
    jsonlite::write_json(
      x = process,
      path = process_file,
      pretty = TRUE,
      auto_unbox = TRUE,
      null = "null"
    )
}
