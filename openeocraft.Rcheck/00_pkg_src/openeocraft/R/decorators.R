#' @title Decorators for registering openEO processes
#' @name openeocraft_decorators
#' @description
#' Process source files (e.g. `inst/ml/processes.R`) use **Plumber-style**
#' `#*` comment blocks. A line matching `#* @openeo-process` starts a chunk;
#' [process_decorators()] scans the file, collects each chunk from that line up
#' to (but not including) the next `#* @...` line or end-of-file, and calls
#' [run_decorator()]. The first token after `@` names the handler; for
#' `openeo-process` the handler is [`openeo-process`], which registers the
#' assigned function on the API and pairs it with JSON under
#' `dirname(processes_file)/processes/<id>.json`. See [load_processes()] for
#' how the server loads a processes file.
#'
#' @keywords internal
NULL

#' Run one decorator block
#'
#' Parses the first line of `source` for `@decorator` and optional trailing
#' tokens, parses the **entire** `source` vector as one R expression, then
#' `eval()`s `decorator_name(api, list(expr), ..., file = file)`.
#'
#' @param api API object passed to the decorator implementation.
#' @param source Character vector of lines from the decorator line through the
#'   end of the chunk (typically includes `#*` comments and one top-level R
#'   assignment such as `my_process <- function(...) { ... }`).
#' @param file Source file path (for error messages and for [`openeo-process`]).
#' @param line Line number of the decorator (for error messages).
#'
#' @return Value of evaluating the constructed decorator call (often `NULL`
#'   invisibly).
#'
#' @details
#' The first line must look like `#* @openeo-process` (optional extra tokens
#' after the decorator name are passed as additional arguments to the handler).
#' On parse failure, throws `stop()` with `basename(file)` and `line` in the
#' message.
#'
#' @seealso [process_decorators()], [`openeo-process`]
#' @keywords internal
run_decorator <- function(api, source, file, line) {
    decorator <- gsub("\\s+", " ", gsub("^[^@]+@", "", source[[1]]))
    decorator <- as.list(strsplit(decorator, " ")[[1]])
    expr <- tryCatch(
        {
            parse(text = source)[[1]]
        },
        error = function(e) {
            stop("Cannot parse code at ", basename(file), "@", line)
        }
    )
    args <- c(
        list(decorator[[1]], api, list(expr)), decorator[-1],
        list(file = file)
    )
    eval(do.call(call, args = args))
}

#' Scan a file for a given `#* @decorator` tag and evaluate each chunk
#'
#' Finds all lines matching `^\\s*#\\*\\s+@<decorator>` and treats each run
#' from one match to the line before the next match (or EOF) as one chunk.
#'
#' @param api API object.
#' @param file Path to an R source file containing `#* @<decorator>` blocks.
#' @param decorator Decorator name without `@` (e.g. `"openeo-process"`).
#'
#' @return `NULL`, invisibly.
#'
#' @seealso [run_decorator()], [load_processes()]
#' @keywords internal
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

#' Sentinel formal used to distinguish missing arguments from defaults
#'
#' Used when auto-generating openEO parameter metadata from function formals:
#' a formal equal to this sentinel is treated as **required** (no default in
#' JSON); any other formal (including explicit `NULL`) is **optional** with a
#' default in JSON.
#'
#' @return The name object taken from a dummy function's formal `x`.
#'
#' @seealso [`openeo-process`]
#' @keywords internal
empty_name <- function() {
    formals(function(x) {})[["x"]]
}

# decorators
# <decorator-function> :=
#    function(<api>, <expr>, [decorator-params...], <proc_file>)

#' Register an openEO process from `#* @openeo-process`
#'
#' @description
#' Reads or creates `processes/<process_id>.json` in a directory **next to**
#' `file` (`dirname(file)/processes/<id>.json`), merges it into the API process
#' list via [add_process()]. The process `id` and JSON file name equal the
#' assigned function name (LHS of `<-`).
#'
#' @param api API object.
#' @param expr Parsed expression; expected to be an assignment whose RHS is a
#'   `function(...) { ... }`. The LHS must be a symbol (the openEO process id).
#' @param ... Reserved for future decorator arguments (passed from `#*`
#'   line tokens after `@openeo-process`).
#' @param file Path to the processes R source file; used to resolve
#'   `dirname(file)/processes/`.
#'
#' @return `NULL`, invisibly.
#'
#' @details
#' **JSON lifecycle:** If `<id>.json` is missing, a minimal descriptor is
#' written from the function's formal arguments: names become parameter `name`
#' fields; formals matching [empty_name()] are required (`optional: false`),
#' others are optional with `default` taken from the formal's default value.
#' Production deployments should ship hand-written JSON (summary, description,
#' schemas, returns) and treat auto-generation as a bootstrap only.
#'
#' The implementation assigns the function into the API namespace when the
#' processes file is sourced (before decorator registration); this handler only
#' registers metadata for `/processes`.
#'
#' Throws from [stopifnot()] if the `processes` directory cannot be created or
#' the JSON file is missing after write.
#'
#' @seealso [process_decorators()], [load_processes()], [add_process()]
#' @keywords internal
`openeo-process` <- function(api, expr, ..., file) {
    # extract from list
    expr <- expr[[1]]
    fn_name <- as.character(expr[[2]])
    # check and create the directory
    dir <- file.path(dirname(path.expand(file)), "processes")
    if (!dir.exists(dir)) {
        dir.create(dir)
    }
    stopifnot(dir.exists(dir))
    # get the process json file
    process_file <- file.path(dir, paste0(fn_name, ".json"))
    # TODO: use some identification to detect if the file was generated by
    #  openeocraft or not.
    if (!file.exists(process_file)) {
        arg_names <- names(expr[[3]][[2]])
        arg_defaults <- unname(expr[[3]][[2]])
        process <- list(
            id = fn_name,
            parameters = lapply(seq_along(arg_names), function(i) {
                if (is.null(arg_defaults[[i]]) ||
                    (arg_defaults[[i]] != empty_name())) {
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
            })
        )
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
