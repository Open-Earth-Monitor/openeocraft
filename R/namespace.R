.openeo <- new.env()
assign("namespace", new.env(parent = emptyenv()), envir = .openeo)

namespace <- function() {
  get("namespace", envir = .openeo)
}

reg_fn <- function(fn, parent = NULL) {
  fn_name <- gsub("^.+::", "", deparse(substitute(fn)))
  if (!is.null(parent))
    environment(fn) <- parent
  assign(fn_name, fn, envir = namespace(), inherits = FALSE)
}

load_rlang <- function() {
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

load_namespace <- function(file) {
  load_rlang()
  src <- readLines(file)
  decor <- c(which(grepl("^\\s*#\\*\\s+@openeo\\s*$", src)), length(src))
  env <- new.env(size = length(decor))
  for (i in seq_len(length(decor) - 1)) {
    expr <- tryCatch({
      parse(text = src[seq(decor[[i]], decor[[i + 1]])])[[1]]
    },
    error = function(e) {
      stop("Cannot parse code at ", basename(file), "@", decor[[i]])
    })
    value <- eval(expr, envir = env)
    if (class(value) != "function")
      stop("Cannot define API function on a value of type ", typeof(value))
    # return the name of the function
    fn_name <- as.character(expr[[2]])
    # get the function object
    fn <- get(fn_name, envir = parent.frame())
    # change parent environment and assign it to namespace
    environment(fn) <- namespace()
    assign(fn_name, fn, envir = namespace(), inherits = FALSE)
  }
}
