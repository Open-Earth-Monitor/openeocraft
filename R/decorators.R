run_decorator <- function(api, source, file, line) {
  decorator <- gsub("\\s+", " ", gsub("^[^@]+@", "", source[[1]]))
  decorator <- as.list(strsplit(decorator, " ")[[1]])
  expr <- tryCatch({
    parse(text = source)[[1]]
  },
  error = function(e) {
    stop("Cannot parse code at ", basename(file), "@", line)
  })
  args <- c(list(decorator[[1]], api, list(expr)), decorator[-1],
            list(file = file))
  eval(do.call(call, args = args))
}

process_decorators <- function(api, file, decorator) {
  source <- readLines(file)
  pattern <- paste0("^\\s*#\\*\\s+@", decorator, "\\s?")
  lines <- c(which(grepl(pattern, source)), length(source))
  for (i in seq_len(length(lines) - 1)) {
    run_decorator(
      api = api,
      source = source[seq(lines[[i]], lines[[i + 1]])],
      file = file,
      line = lines[[i]]
    )
  }
}

empty_name <- function() {
  formals(function(x) {})[["x"]]
}

# decorators
# <decorator-function> := function(<api>, <expr>, [decorator-params...], <proc_file>)
`openeo-process` <- function(api, expr, ..., file) {
  # extract from list
  expr <- expr[[1]]
  fn_name <- as.character(expr[[2]])
  # check and create the directory
  dir <- file.path(dirname(path.expand(file)), "processes")
  if (!dir.exists(dir))
    dir.create(dir)
  stopifnot(dir.exists(dir))
  # get the process json file
  process_file <- file.path(dir, paste0(fn_name, ".json"))
  if (!file.exists(process_file)) {
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
    jsonlite::write_json(
      x = process,
      path = process_file,
      pretty = TRUE,
      auto_unbox = TRUE,
      null = "null"
    )
  }
  stopifnot(file.exists(process_file))
  process <- jsonlite::read_json(process_file)
  add_process(api, process)
  invisible(NULL)
}
