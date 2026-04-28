# TODO allow many files processes definition
#* @openeo-import math.R
#* @openeo-import data.R

# Internal helpers -------------------------------------------------------
# Resource use: by default use openeocraft.resource_fraction (default 0.75) of
# detected physical CPU cores and, when openeocraft.memsize is unset, of total
# RAM. Override explicitly with options(openeocraft.memsize = 16L) and/or
# options(openeocraft.memsize_auto = FALSE) to restore a fixed 8 GB fallback.
# options(openeocraft.multicores_max = 32L) raises the worker cap (default 16).
#
# Developer documentation in this file uses Google-style blocks (summary,
# Args, Returns, Raises) in `#` comments. openEO-facing text remains in `#*`
# decorators where present. Indentation matches package `R/` code (4 spaces).

# Fraction of machine resources (CPU/RAM) openeocraft may use for workers.
#
# Args:
#   (none)
#
# Returns:
#   Numeric scalar in [0.05, 1], from getOption("openeocraft.resource_fraction")
#   or 0.75 when unset/invalid.
.openeocraft_resource_fraction <- function() {
    f <- base::getOption("openeocraft.resource_fraction", 0.75)
    if (!base::is.numeric(f) || base::length(f) != 1L || base::is.na(f)) {
        return(0.75)
    }
    f <- base::as.numeric(f)
    base::min(1, base::max(0.05, f))
}

# Total system RAM in gigabytes (best effort across OS APIs).
#
# Args:
#   (none)
#
# Returns:
#   Non-negative numeric GB, or NA_real_ if detection fails.
.openeocraft_sys_total_ram_gb <- function() {
    if (base::getRversion() >= "4.4.0" &&
        base::exists("Sys.meminfo", mode = "function")) {
        mi <- base::tryCatch(base::Sys.meminfo(), error = function(...) {
            NULL
        })
        if (!base::is.null(mi)) {
            tr <- mi[["totalram"]]
            if (base::is.null(tr)) {
                tr <- mi[["total"]]
            }
            if (!base::is.null(tr) && base::is.numeric(tr) && tr > 0) {
                return(base::as.numeric(tr) / (1024^3))
            }
        }
    }
    if (base::.Platform$OS.type == "unix" &&
        base::file.exists("/proc/meminfo")) {
        lines <- base::readLines("/proc/meminfo", n = 30L, warn = FALSE)
        idx <- base::grep("^MemTotal:", lines)
        if (base::length(idx) >= 1L) {
            parts <- base::strsplit(lines[[idx[[1]]]], "[: \t]+")[[1]]
            parts <- parts[parts != ""]
            if (base::length(parts) >= 2L) {
                kb <- base::suppressWarnings(base::as.numeric(parts[[2]]))
                if (!base::is.na(kb)) {
                    return(kb / (1024^2))
                }
            }
        }
    }
    if (base::Sys.info()[["sysname"]] == "Darwin") {
        sysctl <- base::Sys.which("sysctl")
        if (base::nzchar(sysctl)) {
            out <- base::tryCatch(
                base::system2(
                    sysctl,
                    args = "-n hw.memsize",
                    stdout = TRUE,
                    stderr = FALSE
                ),
                warning = function(...) {
                    NULL
                },
                error = function(...) {
                    NULL
                }
            )
            if (!base::is.null(out) && base::length(out) >= 1L) {
                b <- base::suppressWarnings(base::as.numeric(out[[1]]))
                if (!base::is.na(b)) {
                    return(b / (1024^3))
                }
            }
        }
    }
    NA_real_
}

# Parallel worker count capped by resource fraction and multicores_max option.
#
# Args:
#   (none)
#
# Returns:
#   Positive integer; at least 1, at most getOption("openeocraft.multicores_max", 16L).
.openeocraft_multicores <- function() {
    frac <- .openeocraft_resource_fraction()
    cores <- base::tryCatch(
        parallel::detectCores(logical = FALSE),
        error = function(...) {
            NA_integer_
        }
    )
    if (base::is.na(cores) || !base::is.numeric(cores) || cores < 1L) {
        cores <- 2L
    }
    usable <- base::max(1L, base::as.integer(base::floor(cores * frac)))
    cap <- base::getOption("openeocraft.multicores_max", 16L)
    if (!base::is.numeric(cap) || base::length(cap) != 1L || base::is.na(cap)) {
        cap <- 16L
    }
    if (!base::is.finite(cap)) {
        return(usable)
    }
    cap <- base::as.integer(cap)
    if (base::is.na(cap) || cap < 1L) {
        cap <- 16L
    }
    base::as.integer(base::min(usable, cap))
}

# Memory budget in GB for sits/torch-style operations.
#
# Args:
#   (none)
#
# Returns:
#   Integer GB: explicit openeocraft.memsize, or auto from total RAM * fraction,
#   or 8 when auto is off or detection fails (clamped to [1, 256] when auto).
.openeocraft_memsize <- function() {
    explicit <- base::getOption("openeocraft.memsize", NULL)
    if (!base::is.null(explicit)) {
        mem <- explicit
        if (!base::is.numeric(mem) ||
            base::length(mem) != 1L || base::is.na(mem)) {
            mem <- 8L
        }
        return(base::as.integer(base::max(1L, mem)))
    }
    if (!base::isTRUE(base::getOption("openeocraft.memsize_auto", TRUE))) {
        return(8L)
    }
    total_gb <- .openeocraft_sys_total_ram_gb()
    if (base::is.na(total_gb) || total_gb <= 0) {
        return(8L)
    }
    frac <- .openeocraft_resource_fraction()
    gb <- base::floor(total_gb * frac)
    base::as.integer(base::max(1L, base::min(gb, 256L)))
}

# Load training/validation samples from URL (RDS), JSON, local RDS path, or pass-through.
#
# Args:
#   x: Character (URL/path/JSON) or already-deserialized R object.
#   param_name: Name used in error messages (default "training_data").
#   wrap_io_errors: If TRUE, download/read errors become stop() with context.
#
# Returns:
#   The loaded or passthrough R object.
#
# Raises:
#   stop() if x is an invalid empty string, or not a loadable URL/file/JSON;
#   optional I/O messages when wrap_io_errors is TRUE.
.openeocraft_load_samples <- function(x,
                                      param_name = "training_data",
                                      wrap_io_errors = FALSE) {
    if (!base::is.character(x) || base::length(x) != 1L) {
        return(x)
    }
    if (base::any(base::is.na(x)) || !base::nzchar(x)) {
        stop(
            base::paste0(param_name, " must be a non-missing, non-empty string when given as text"),
            call. = FALSE
        )
    }
    err <- base::paste0(
        param_name,
        " must be a URL to RDS, a local RDS path, a JSON-serialized object, or an object"
    )
    if (base::isTRUE(base::grepl("^https?://", x))) {
        tmp <- base::tempfile(fileext = ".rds")
        base::on.exit(base::unlink(tmp), add = TRUE)
        if (wrap_io_errors) {
            return(base::tryCatch(
                {
                    utils::download.file(x, tmp, mode = "wb", quiet = TRUE)
                    base::readRDS(tmp)
                },
                error = function(e) {
                    stop(
                        base::sprintf(
                            "Failed to download/read %s from URL: %s",
                            param_name,
                            e$message
                        ),
                        call. = FALSE
                    )
                }
            ))
        }
        utils::download.file(x, tmp, mode = "wb", quiet = TRUE)
        return(base::readRDS(tmp))
    }
    json_obj <- base::tryCatch(
        jsonlite::unserializeJSON(x),
        error = function(...) {
            NULL
        }
    )
    if (!base::is.null(json_obj)) {
        return(json_obj)
    }
    if (base::file.exists(x)) {
        if (wrap_io_errors) {
            return(base::tryCatch(
                base::readRDS(x),
                error = function(e) {
                    stop(
                        base::sprintf(
                            "Failed to read %s from file: %s",
                            param_name,
                            e$message
                        ),
                        call. = FALSE
                    )
                }
            ))
        }
        return(base::readRDS(x))
    }
    stop(err, call. = FALSE)
}

# Wrap sits_train with start/finish messages and elapsed wall time.
#
# sits/torch often drives txtProgressBar to stderr; openEO job logs may show long
# gaps between lines while those bars stream elsewhere.
#
# Args:
#   training_set: sits samples object passed to sits::sits_train.
#   sits_ml_model: Model specification from sits::*_class constructors.
#   log_label: Prefix for message lines (e.g. process name).
#
# Returns:
#   Return value of sits::sits_train.
#
# Raises:
#   Propagates errors from sits::sits_train.
.openeocraft_sits_train_logged <- function(training_set, sits_ml_model, log_label) { # nolint
    t0 <- base::Sys.time()
    base::message(
        "[",
        log_label,
        "] sits_train: starting (CPU torch training can take many minutes; ",
        "progress bars earlier in the job usually come from collection load / regularize, not this step)." # nolint
    )
    out <- sits::sits_train(training_set, sits_ml_model)
    secs <- base::as.numeric(base::difftime(base::Sys.time(), t0, units = "secs"))
    dur <- if (!base::is.na(secs) && secs >= 120) {
        base::paste0(base::round(secs / 60, 1), " min")
    } else if (!base::is.na(secs)) {
        base::paste0(base::round(secs, 1), " s")
    } else {
        "?"
    }
    base::message("[", log_label, "] sits_train: finished in ", dur)
    out
}

# Single-row data.frame of overall accuracy metrics from sits or caret objects.
#
# Args:
#   x: sits_accuracy, confusionMatrix, or object coercible via sits::sits_accuracy.
#
# Returns:
#   data.frame with one row of overall metrics, or empty data.frame if none.
.openeocraft_accuracy_summary_df <- function(x) {
    acc <- if (base::inherits(x, "sits_accuracy") || base::inherits(x, "confusionMatrix")) {
        x
    } else {
        sits::sits_accuracy(x)
    }
    overall <- acc[["overall"]]
    if (base::is.null(overall)) {
        return(base::data.frame())
    }
    base::as.data.frame(base::t(overall), stringsAsFactors = FALSE)
}

# Map param_grid/parameter optimizer name strings to torch optim functions in ml_args.
#
# Args:
#   param_set: Named list; may contain character "optimizer".
#   ml_args: Model argument list to receive optimizer function.
#   optimizer_context: "param_grid" or "parameters" (for error messages).
#
# Returns:
#   list(param_set, ml_args) with optimizer consumed from param_set when known.
#
# Raises:
#   openeocraft::api_stop(400) if optimizer string is unsupported.
.openeocraft_tune_map_optimizer_param <- function(param_set, ml_args, optimizer_context) {
    optimizer_context <- base::match.arg(optimizer_context, c("param_grid", "parameters"))
    if (base::is.null(param_set$optimizer)) {
        return(list(param_set = param_set, ml_args = ml_args))
    }
    if (!base::is.character(param_set$optimizer)) {
        return(list(param_set = param_set, ml_args = ml_args))
    }
    optimizer_fn <- base::switch(param_set$optimizer,
        "adam" = torch::optim_adamw,
        "adabound" = torch::optim_adabound,
        "adabelief" = torch::optim_adabelief,
        "madagrad" = torch::optim_madagrad,
        "nadam" = torch::optim_nadam,
        "qhadam" = torch::optim_qhadam,
        "radam" = torch::optim_radam,
        "swats" = torch::optim_swats,
        "yogi" = torch::optim_yogi,
        NULL
    )
    if (base::is.null(optimizer_fn)) {
        unsupported_msg <- if (optimizer_context == "param_grid") {
            "Unsupported optimizer in param_grid."
        } else {
            "Unsupported optimizer in parameters."
        }
        openeocraft::api_stop(400, unsupported_msg)
    }
    ml_args$optimizer <- optimizer_fn
    param_set$optimizer <- NULL
    list(param_set = param_set, ml_args = ml_args)
}

# Move learning_rate/lr/epsilon/weight_decay from param_set into ml_args$opt_hparams.
#
# Args:
#   param_set: Named list of hyperparameters for one grid/random draw.
#   ml_args: Model args list containing opt_hparams list or NULL.
#
# Returns:
#   list(param_set, ml_args) with recognized keys removed from param_set.
.openeocraft_tune_map_opt_hparams <- function(param_set, ml_args) {
    if (base::is.null(ml_args$opt_hparams) || !base::is.list(ml_args$opt_hparams)) {
        return(list(param_set = param_set, ml_args = ml_args))
    }
    opt_map <- list(
        learning_rate = "lr",
        lr = "lr",
        epsilon = "eps",
        eps = "eps",
        weight_decay = "weight_decay"
    )
    for (name in base::names(opt_map)) {
        if (!base::is.null(param_set[[name]])) {
            ml_args$opt_hparams[[opt_map[[name]]]] <- param_set[[name]]
            param_set[[name]] <- NULL
        }
    }
    list(param_set = param_set, ml_args = ml_args)
}

# Map Random Forest max_variables grid key to sits mtry in ml_args.
#
# Args:
#   param_set: May contain max_variables.
#   ml_args: Receives mtry when max_variables is set.
#
# Returns:
#   list(param_set, ml_args).
.openeocraft_tune_map_rf_params <- function(param_set, ml_args) {
    if (!base::is.null(param_set$max_variables)) {
        ml_args$mtry <- param_set$max_variables
        param_set$max_variables <- NULL
    }
    list(param_set = param_set, ml_args = ml_args)
}

# Evaluate expr with CUDA/MPS disabled and torch availability stubs forced off.
#
# Args:
#   expr: Unevaluated expression (typically a validation call).
#
# Returns:
#   Value of force(expr).
#
# Raises:
#   Propagates errors from expr; may fail if torch namespace bindings change.
.openeocraft_tune_with_force_cpu <- function(expr) {
    base::Sys.setenv(CUDA_VISIBLE_DEVICES = "")
    base::Sys.setenv(TORCH_DISABLE_MPS = "1")
    base::Sys.setenv(TORCH_USE_MPS_FALLBACK = "1")
    if (!base::requireNamespace("torch", quietly = TRUE)) {
        return(base::force(expr))
    }
    torch_ns <- base::asNamespace("torch")
    override <- function(name, replacement) {
        if (!base::exists(name, envir = torch_ns, inherits = FALSE)) {
            return(NULL)
        }
        locked <- base::bindingIsLocked(name, torch_ns)
        if (locked) {
            base::unlockBinding(name, torch_ns)
        }
        original <- base::get(name, envir = torch_ns)
        base::assign(name, replacement, envir = torch_ns)
        if (locked) {
            base::lockBinding(name, torch_ns)
        }
        list(name = name, original = original, locked = locked)
    }
    restore <- function(info) {
        if (base::is.null(info)) {
            return(base::invisible(NULL))
        }
        if (info$locked) {
            base::unlockBinding(info$name, torch_ns)
        }
        base::assign(info$name, info$original, envir = torch_ns)
        if (info$locked) {
            base::lockBinding(info$name, torch_ns)
        }
        base::invisible(NULL)
    }
    cuda_info <- override("cuda_is_available", function() FALSE)
    mps_info <- override("backends_mps_is_available", function() FALSE)
    base::on.exit(
        {
            restore(cuda_info)
            restore(mps_info)
        },
        add = TRUE
    )
    base::force(expr)
}

# Run sits_validate or sits_kfold_validate depending on cv.
#
# Args:
#   ml_method: Fitted ml method from sits.
#   training_obj: Sample data.frame.
#   cv: Fold count; if NULL or <= 1, uses validation_split path.
#   validation_split: Passed to sits_validate when cv <= 1.
#
# Returns:
#   Accuracy/validation object from sits.
.openeocraft_tune_run_validation <- function(ml_method, training_obj, cv, validation_split) {
    if (base::is.null(cv) || cv <= 1) {
        return(sits::sits_validate(
            samples = training_obj,
            validation_split = validation_split,
            ml_method = ml_method
        ))
    }
    sits::sits_kfold_validate(
        samples = training_obj,
        folds = cv,
        ml_method = ml_method,
        multicores = .openeocraft_multicores(),
        progress = FALSE
    )
}

# Evaluate each hyperparameter combination with validation and collect metrics.
#
# Args:
#   model: openEO ML model list (ml_method, ml_args, train, ...).
#   training_obj: Loaded sample data.
#   param_sets: List of named lists, each one hyperparameter combination.
#   cv, validation_split: Passed to validation helper.
#   scoring: Metric name (lowercase) to read from summary_df columns.
#   log_msg: Function(...) for progress logging.
#   optimizer_context: "param_grid" or "parameters" for optimizer mapping.
#
# Returns:
#   List of lists, each c(param_set, list(metric = numeric)).
#
# Raises:
#   stop() on validation errors except GPU memory, which retries with CPU;
#   openeocraft::api_stop from optimizer mapping.
.openeocraft_tune_evaluate_param_sets <- function(model,
                                                  training_obj,
                                                  param_sets,
                                                  cv,
                                                  validation_split,
                                                  scoring,
                                                  log_msg,
                                                  optimizer_context) {
    metric_key <- base::tolower(scoring)
    total_sets <- base::length(param_sets)
    results <- list()
    for (i in base::seq_along(param_sets)) {
        param_set <- param_sets[[i]]
        param_set_raw <- param_set
        start_time <- base::Sys.time()
        mapped <- .openeocraft_tune_map_optimizer_param(
            param_set,
            model$ml_args,
            optimizer_context
        )
        param_set <- mapped$param_set
        base_args <- mapped$ml_args
        mapped <- .openeocraft_tune_map_opt_hparams(param_set, base_args)
        param_set <- mapped$param_set
        base_args <- mapped$ml_args
        mapped <- .openeocraft_tune_map_rf_params(param_set, base_args)
        param_set <- mapped$param_set
        base_args <- mapped$ml_args
        merged_args <- utils::modifyList(base_args, param_set)
        log_msg(
            "Evaluating ",
            i,
            "/",
            total_sets,
            " with params: ",
            jsonlite::toJSON(param_set_raw, auto_unbox = TRUE)
        )
        ml_method <- base::do.call(model$ml_method, merged_args)
        acc_obj <- base::tryCatch(
            .openeocraft_tune_run_validation(ml_method, training_obj, cv, validation_split),
            error = function(e) {
                if (base::grepl("GPU memory", e$message, ignore.case = TRUE)) {
                    log_msg("GPU memory low, retrying with CPU settings.")
                    return(
                        .openeocraft_tune_with_force_cpu(
                            .openeocraft_tune_run_validation(
                                ml_method,
                                training_obj,
                                cv,
                                validation_split
                            )
                        )
                    )
                }
                stop(e)
            }
        )
        summary_df <- .openeocraft_accuracy_summary_df(acc_obj)
        metric_value <- NA_real_
        if (base::is.data.frame(summary_df)) {
            cols <- base::tolower(base::names(summary_df))
            idx_col <- base::match(metric_key, cols)
            if (!base::is.na(idx_col)) {
                metric_value <- summary_df[[idx_col]][1]
            } else {
                numeric_cols <- base::vapply(summary_df, base::is.numeric, logical(1))
                if (base::any(numeric_cols)) {
                    metric_value <- summary_df[[base::which(numeric_cols)[1]]][1]
                }
            }
        }
        elapsed <- base::difftime(base::Sys.time(), start_time, units = "mins")
        log_msg(
            "Completed ",
            i,
            "/",
            total_sets,
            " metric=",
            metric_value,
            " elapsed=",
            base::round(base::as.numeric(elapsed), 2),
            " min"
        )
        results <- c(results, list(base::c(param_set_raw, list(metric = metric_value))))
    }
    results
}

# Pick best tuning run, write tuning_results.json and STAC sidecar, return summary.
#
# Args:
#   results: Output list from .openeocraft_tune_evaluate_param_sets.
#
# Returns:
#   list(best_params, best_score, results) with results as parsed JSON list.
#
# Raises:
#   openeocraft::api_stop(400/500) on empty or all-NA metrics.
.openeocraft_tune_finalize_results <- function(results) {
    if (base::length(results) == 0L) {
        openeocraft::api_stop(400, "Tuning produced no results (empty parameter grid).")
    }
    mets <- base::vapply(
        results,
        function(x) {
            x$metric
        },
        base::numeric(1),
        USE.NAMES = FALSE
    )
    mets_use <- base::replace(mets, base::is.na(mets), -Inf)
    if (!base::any(base::is.finite(mets))) {
        openeocraft::api_stop(
            500,
            "All tuning runs returned a missing metric; check validation errors and scoring name."
        )
    }
    best_idx <- base::which.max(mets_use)
    best_result <- results[[best_idx]]
    env <- openeocraft::current_env()
    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    host <- openeocraft::get_host(env$api, env$req)
    results_json <- jsonlite::toJSON(results, auto_unbox = TRUE, digits = 16)
    results_file <- base::file.path(job_dir, "tuning_results.json")
    base::writeLines(results_json, results_file)
    asset_href <- openeocraft::make_job_files_url(
        host = host,
        user = env$user,
        job_id = env$job$id,
        file = "tuning_results.json"
    )
    collection <- openeocraft::job_empty_collection(env$api, env$user, env$job)
    collection$assets <- list(
        tuning_results = list(
            href = asset_href,
            type = "application/json",
            roles = list("data")
        )
    )
    jsonlite::write_json(
        x = collection,
        path = base::file.path(job_dir, "_collection.json"),
        auto_unbox = TRUE,
        digits = 16
    )
    list(
        best_params = best_result,
        best_score = best_result$metric,
        results = jsonlite::fromJSON(results_json, simplifyVector = FALSE)
    )
}

# Train on full sample set using the best tuning row (same mapping as validation loop).
#
# Args:
#   model: openEO ML model list (ml_method, ml_args, mlm_*).
#   training_obj: sits samples.
#   best_row: List with hyperparameters and metric (metric is ignored).
#   optimizer_context: "param_grid" or "parameters".
#   log_msg: function(...) for progress.
#   log_label: Prefix for sits_train timing message.
#
# Returns:
#   Trained sits model with MLM attributes (same contract as ml_fit output).
#
# Raises:
#   Propagates sits / training errors; GPU OOM may retry on CPU like tuning.
.openeocraft_tune_final_train <- function(model,
                                          training_obj,
                                          best_row,
                                          optimizer_context,
                                          log_msg,
                                          log_label) {
    param_set <- best_row
    param_set$metric <- NULL
    mapped <- .openeocraft_tune_map_optimizer_param(
        param_set,
        model$ml_args,
        optimizer_context
    )
    param_set <- mapped$param_set
    base_args <- mapped$ml_args
    mapped <- .openeocraft_tune_map_opt_hparams(param_set, base_args)
    param_set <- mapped$param_set
    base_args <- mapped$ml_args
    mapped <- .openeocraft_tune_map_rf_params(param_set, base_args)
    param_set <- mapped$param_set
    base_args <- mapped$ml_args
    merged_args <- utils::modifyList(base_args, param_set)
    ml_method <- base::do.call(model$ml_method, merged_args)
    log_msg("Final fit on full training set with best hyperparameters.")
    trained_model <- base::tryCatch(
        .openeocraft_sits_train_logged(training_obj, ml_method, log_label),
        error = function(e) {
            if (base::grepl("GPU memory", e$message, ignore.case = TRUE)) {
                log_msg("GPU memory low on final fit, retrying with CPU settings.")
                return(
                    .openeocraft_tune_with_force_cpu(
                        .openeocraft_sits_train_logged(
                            training_obj,
                            ml_method,
                            log_label
                        )
                    )
                )
            }
            stop(e)
        }
    )
    .openeocraft_attach_ml_fit_metadata(
        trained_model,
        model,
        training_obj,
        ml_args_for_hyperparameters = merged_args
    )
}

# Load a sits data cube from a configured source and collection (openEO process).
#
# Args:
#   id: Composite id "source-collection" resolved against sits_config() sources.
#   spatial_extent: List with west, east, south, north (EPSG:4326 bbox).
#   temporal_extent: Length-2 vector: start and end date for sits_cube.
#   bands: Optional band names (character vector or list).
#   properties: Reserved / passed through for catalog filters when supported.
#
# Returns:
#   sits raster cube; attr(, "roi") is the sf bbox polygon used for the request.
#
# Raises:
#   stop() for invalid id, spatial keys, temporal length, unknown bands, or
#   sits_cube failures (wrapped with "Error in load_collection():" prefix).
#
# Client-facing openEO documentation continues in the #* block below.
#
#* @summary Load a collection
#*
#* @description
#* Loads a collection from the current back-end by its id and returns
#* it as a processable data cube. The data that is added to the data
#* cube can be restricted with the parameters `spatial_extent`,
#* `temporal_extent`, `bands` and `properties`. If no data is available
#* for the given extents, a `NoDataAvailable` exception is thrown.
#*
#* Remarks:
#* - The bands (and all dimensions that specify nominal dimension labels)
#* are expected to be ordered as specified in the metadata if the `bands`
#* parameter is set to `null`.
#* - If no additional parameter is specified this would imply that the
#* whole data set is expected to be loaded. Due to the large size of
#* many data sets, this is not recommended and may be optimized by
#* back-ends to only load the data that is actually required after
#* evaluating subsequent processes such as filters. This means that the
#* values in the data cube should be processed only after the data has
#* been limited to the required extent and as a consequence also to a
#* manageable size.
#*
#* @categories cubes import
#*
#* @param id The collection id.
#* @param spatial_extent Limits the data to load from the collection
#* to the specified bounding box or polygons.
#* - For raster data, the process loads the pixel into the data cube if
#* the point at the pixel center intersects with the bounding box or any
#* of the polygons (as defined in the Simple Features standard by the OGC).
#* - For vector data, the process loads the geometry into the data cube
#* if the geometry is fully *within* the bounding box or any of the
#* polygons (as defined in the Simple Features standard by the OGC).
#* Empty geometries may only be in the data cube if no spatial extent
#* has been provided.
#*
#* The GeoJSON can be one of the following feature types:
#* - a `Polygon` or `MultiPolygon` geometry,
#* - a `Feature` with a `Polygon` or `MultiPolygon` geometry, or
#* - a `FeatureCollection` containing at least one `Feature` with `Polygon`
#*   or `MultiPolygon` geometries.
#* - Empty geometries are ignored.
#*
#* Set this parameter to `null` to set no limit for the spatial extent.
#* Be careful with this when loading large datasets! It is recommended to
#* use this parameter instead of using ``filter_bbox()`` or
#* ``filter_spatial()`` directly after loading unbounded data.
#*
#* @openeo-process
load_collection <- function(id,
                            spatial_extent = NULL,
                            temporal_extent = NULL,
                            bands = NULL,
                            properties = NULL) {
    base::message("[load_collection] START")
    base::on.exit(base::message("[load_collection] END"))
    base::tryCatch(
        {
            # Split ID
            parts <- base::strsplit(id, "-", fixed = TRUE)[[1]]
            if (base::length(parts) < 2) {
                stop(
                    "Invalid collection ID. ",
                    "Expected format: 'source-collection'."
                )
            }
            source <- parts[[1]]
            collection <- base::paste(parts[-1], collapse = "-")

            # Normalize collection name against sits config (case-insensitive).
            config <- sits::sits_config()
            if (!base::is.null(config$sources)) {
                source_names <- base::names(config$sources)
                source_match <- source_names[
                    base::tolower(source_names) == base::tolower(source)
                ][1]
                if (!base::is.na(source_match)) {
                    collections <- config$sources[[source_match]]$collections
                    if (!base::is.null(collections)) {
                        collection_names <- base::names(collections)
                        collection_match <- collection_names[
                            base::tolower(collection_names) ==
                                base::tolower(collection)
                        ][1]
                        if (!base::is.na(collection_match)) {
                            collection <- collection_match
                        }
                    }
                }
            }

            # Normalize band names against collection config if available.
            if (!base::is.null(bands) && base::is.list(bands)) {
                bands <- base::unlist(bands, use.names = FALSE)
            }
            if (!base::is.null(bands) &&
                !base::is.null(config$sources) &&
                !base::is.na(source_match)) {
                collection_config <- config$sources[[source_match]]$collections[[collection]]
                if (!base::is.null(collection_config) &&
                    !base::is.null(collection_config$bands) &&
                    base::is.character(bands)) {
                    config_band_names <- base::names(collection_config$bands)
                    config_band_lower <- base::tolower(config_band_names)
                    bands_lower <- base::tolower(bands)
                    match_idx <- base::match(bands_lower, config_band_lower)
                    if (base::any(base::is.na(match_idx))) {
                        stop(
                            "Invalid bands parameter. Valid names: ",
                            base::paste(config_band_names, collapse = ", ")
                        )
                    }
                    bands <- config_band_names[match_idx]
                }
            }

            # Validate spatial_extent
            required_keys <- c("west", "east", "south", "north")
            if (!base::all(required_keys %in% base::names(spatial_extent))) {
                stop(
                    "Missing keys in spatial_extent. ",
                    "Expected: west, east, south, north"
                )
            }

            # Create sf polygon for ROI
            bbox <- sf::st_bbox(
                c(
                    xmin = spatial_extent$west,
                    ymin = spatial_extent$south,
                    xmax = spatial_extent$east,
                    ymax = spatial_extent$north
                ),
                crs = 4326
            )
            roi <- sf::st_as_sfc(bbox)

            # Validate temporal_extent
            if (!base::is.vector(temporal_extent) ||
                base::length(temporal_extent) != 2) {
                stop(
                    "temporal_extent must be a vector of ",
                    "length 2: [start_date, end_date]"
                )
            }

            # Network / STAC discovery can take minutes; this runs when the cube
            # is first forced (often after upstream process START messages — lazy args).
            base::message(
                "[load_collection] sits_cube(source=",
                source,
                ", collection=",
                collection,
                ") ..."
            )
            data <- sits::sits_cube(
                source = source,
                collection = collection,
                bands = bands,
                roi = roi,
                # sf polygon object
                start_date = temporal_extent[[1]],
                end_date = temporal_extent[[2]]
            )
            base::message("[load_collection] sits_cube() returned")

            base::attr(data, "roi") <- roi
            return(data)
        },
        error = function(e) {
            stop(base::paste("Error in load_collection():", e$message))
        }
    )
}


# Load user-uploaded workspace files and return a data cube-like object.
#
# Args:
#   paths: Character vector of workspace-relative file paths.
#   format: Input format identifier (e.g. GTiff, GeoTIFF, RDS).
#   options: Optional list of loader options. For GeoTIFF, supports
#     source, collection, tile, band, parse_info and delim.
#
# Returns:
#   A cube object for downstream processes. For RDS, returns the R object
#   (or row-bound data.frame for multiple files).
#
# Raises:
#   openeocraft::api_stop(404) for missing files;
#   openeocraft::api_stop(400) for invalid paths or unsupported formats.
#
#* @openeo-process
load_uploaded_files <- function(paths, format, options = list()) {
    base::message("[load_uploaded_files] START")
    base::on.exit(base::message("[load_uploaded_files] END"))

    env <- openeocraft::current_env()
    workspace_root <- base::file.path(
        openeocraft::api_user_workspace(env$api, env$user),
        "root"
    )
    if (!base::dir.exists(workspace_root)) {
        base::dir.create(workspace_root, recursive = TRUE)
    }
    root_norm <- base::normalizePath(workspace_root, winslash = "/", mustWork = TRUE)

    if (base::is.null(paths) || !base::length(paths)) {
        openeocraft::api_stop(400, "Parameter 'paths' must contain at least one file path")
    }
    if (base::is.list(paths)) {
        paths <- base::unlist(paths, use.names = FALSE)
    }
    if (!base::is.character(paths)) {
        openeocraft::api_stop(400, "Parameter 'paths' must be an array of strings")
    }

    if (!base::is.character(format) || base::length(format) != 1L || !base::nzchar(format)) {
        openeocraft::api_stop(400, "Parameter 'format' must be a non-empty string")
    }
    fmt <- base::gsub("[^a-z0-9]", "", base::tolower(format))

    resolve_workspace_file <- function(p) {
        if (!base::is.character(p) || base::length(p) != 1L || !base::nzchar(p)) {
            openeocraft::api_stop(400, "Invalid file path in 'paths'")
        }
        rel <- utils::URLdecode(p)
        rel <- base::sub("^\\./", "", rel)
        rel <- base::sub("^/+", "", rel)
        if (!base::nzchar(rel) || base::grepl("\\\\", rel)) {
            openeocraft::api_stop(400, "Invalid file path in 'paths'")
        }
        parts <- base::strsplit(rel, "/", fixed = TRUE)[[1]]
        if (base::any(parts %in% base::c("", ".", ".."))) {
            openeocraft::api_stop(404, "FileNotFound: path outside workspace")
        }
        abs <- base::gsub("//+", "/", base::file.path(root_norm, rel))
        if (!base::startsWith(abs, base::paste0(root_norm, "/")) && abs != root_norm) {
            openeocraft::api_stop(404, "FileNotFound: path outside workspace")
        }
        if (!base::file.exists(abs) || base::dir.exists(abs)) {
            openeocraft::api_stop(404, base::paste0("FileNotFound: ", rel))
        }
        abs
    }

    file_paths <- base::vapply(paths, resolve_workspace_file, character(1))

    if (fmt == "rds") {
        objs <- base::lapply(file_paths, base::readRDS)
        if (base::length(objs) == 1L) {
            return(objs[[1]])
        }
        if (base::all(base::vapply(objs, base::is.data.frame, logical(1)))) {
            return(base::do.call(base::rbind, objs))
        }
        openeocraft::api_stop(
            400,
            "FormatUnsuitable: multiple RDS files must contain compatible data.frames"
        )
    }

    if (!fmt %in% base::c("gtiff", "geotiff", "tif", "tiff")) {
        openeocraft::api_stop(
            400,
            base::paste0("FormatUnsuitable: unsupported input format '", format, "'")
        )
    }

    if (!base::requireNamespace("sits", quietly = TRUE)) {
        openeocraft::api_stop(500, "Package 'sits' is required to load GeoTIFF files")
    }

    if (base::is.null(options) || !base::is.list(options)) {
        options <- list()
    }

    source <- options[["source"]]
    if (base::is.null(source) || !base::is.character(source) || !base::nzchar(source[[1]])) {
        source <- "MPC"
    }
    collection <- options[["collection"]]
    if (base::is.null(collection) ||
        !base::is.character(collection) ||
        !base::nzchar(collection[[1]])) {
        collection <- "SENTINEL-2-L2A"
    }
    tile <- options[["tile"]]
    if (base::is.null(tile) || !base::is.character(tile) || !base::nzchar(tile[[1]])) {
        tile <- "T000000"
    }
    base_band <- options[["band"]]
    if (base::is.null(base_band) ||
        !base::is.character(base_band) ||
        !base::nzchar(base_band[[1]])) {
        base_band <- "B02"
    }
    parse_info <- options[["parse_info"]]
    if (base::is.null(parse_info)) {
        parse_info <- base::c("X1", "X2", "tile", "band", "date")
    }
    delim <- options[["delim"]]
    if (base::is.null(delim) || !base::is.character(delim) || !base::nzchar(delim[[1]])) {
        delim <- "_"
    }

    stage_dir <- base::tempfile("uploaded-cube-")
    base::dir.create(stage_dir, recursive = TRUE)
    base::on.exit(base::unlink(stage_dir, recursive = TRUE, force = TRUE), add = TRUE)

    staged_bands <- base::character(0)
    for (i in base::seq_along(file_paths)) {
        src <- file_paths[[i]]
        stamp <- base::as.Date(base::file.info(src)$mtime)
        if (base::is.na(stamp)) {
            stamp <- base::as.Date("2000-01-01")
        }
        band <- base_band
        if (base::length(file_paths) > 1L) {
            band <- base::sprintf("B%02d", ((i - 1L) %% 12L) + 1L)
        }
        staged_bands <- base::c(staged_bands, band)
        ext <- tools::file_ext(src)
        if (!base::nzchar(ext)) {
            ext <- "tif"
        }
        staged_name <- base::paste0(
            "upl",
            delim,
            "wrk",
            delim,
            tile,
            delim,
            band,
            delim,
            base::format(stamp, "%Y-%m-%d"),
            ".",
            ext
        )
        dst <- base::file.path(stage_dir, staged_name)
        ok <- base::file.copy(src, dst, overwrite = TRUE)
        if (!base::isTRUE(ok)) {
            openeocraft::api_stop(500, base::paste0("Could not stage file: ", base::basename(src)))
        }
    }

    base::tryCatch(
        {
            sits::sits_cube(
                source = source[[1]],
                collection = collection[[1]],
                data_dir = stage_dir,
                parse_info = parse_info,
                delim = delim[[1]],
                bands = base::unique(staged_bands),
                multicores = .openeocraft_multicores(),
                progress = FALSE
            )
        },
        error = function(e) {
            openeocraft::api_stop(
                400,
                base::paste0("FormatUnsuitable: Data can't be loaded with the requested format. ", e$message)
            )
        }
    )
}


# Random Forest classification model spec for ml_fit (sits_rfor).
#
# Args:
#   num_trees: Number of trees in the forest.
#   max_variables: mtry as number, or "sqrt"/"log2" resolved at train() time.
#   seed: Optional RNG seed before training.
#
# Returns:
#   List with ml_method, ml_args, mlm_* metadata, and train(training_set).
#
# Raises:
#   stop() from train() if max_variables type is invalid; sits_train errors otherwise.
#
#* @openeo-process
mlm_class_random_forest <- function(num_trees = 100,
                                    max_variables = "sqrt",
                                    seed = NULL) {
    base::message("[mlm_class_random_forest] START")
    base::on.exit(base::message("[mlm_class_random_forest] END"))
    # Store seed for reproducibility
    if (!base::is.null(seed)) {
        base::set.seed(seed)
    }
    # For ml_validate/ml_tune compatibility, ml_args uses NULL for mtry when
    # max_variables is a string ("sqrt"/"log2") since sits_rfor expects numeric.
    # The train() function handles string conversion using training data dimensions.
    mtry_for_args <- if (base::is.numeric(max_variables)) max_variables else NULL
    list(
        ml_method = sits::sits_rfor,
        ml_args = list(num_trees = num_trees, mtry = mtry_for_args),
        mlm_task = "classification",
        mlm_architecture = "Random Forest",
        mlm_framework = "R CARET",
        train = function(training_set) {
            base::message("[mlm_class_random_forest::train] START")
            base::on.exit(base::message("[mlm_class_random_forest::train] END"))
            # start preparing max_variables parameter
            n_bands <- base::length(sits::sits_bands(training_set))
            n_times <- base::length(sits::sits_timeline(training_set))
            n_features <- n_bands * n_times
            mtry_final <- max_variables
            if (base::is.character(max_variables)) {
                if (max_variables == "sqrt") {
                    mtry_final <- base::sqrt(n_features)
                } else if (max_variables == "log2") {
                    mtry_final <- base::log2(n_features)
                }
            } else if (base::is.null(max_variables)) {
                mtry_final <- NULL
            } else if (base::is.numeric(max_variables)) {
                mtry_final <- max_variables
            } else {
                stop("Invalid max_variables parameter", call. = FALSE)
            }
            if (!base::is.null(mtry_final)) {
                mtry_final <- base::max(1, base::floor(mtry_final))
            }
            # end preparing max_variables parameter
            model <- sits::sits_rfor(num_trees = num_trees, mtry = mtry_final)
            if (!base::is.null(seed)) {
                base::set.seed(seed)
            }
            .openeocraft_sits_train_logged(
                training_set,
                model,
                "mlm_class_random_forest::train"
            )
        }
    )
}

# Linear-kernel SVM classification model spec (sits_svm).
#
# Args:
#   kernel, degree, coef0, cost, tolerance, epsilon, cachesize: e1071/caret SVM args.
#   seed: Optional RNG seed before training.
#
# Returns:
#   Model spec list with train(training_set) closure.
#
# Raises:
#   Propagates sits training errors.
#
#* @openeo-process
mlm_class_svm <- function(kernel = "radial",
                          degree = 3,
                          coef0 = 0,
                          cost = 10,
                          tolerance = 0.001,
                          epsilon = 0.1,
                          cachesize = 1000,
                          seed = NULL) {
    base::message("[mlm_class_svm] START")
    base::on.exit(base::message("[mlm_class_svm] END"))
    formula <- sits::sits_formula_linear()

    list(
        ml_method = sits::sits_svm,
        ml_args = list(
            formula = formula,
            cachesize = cachesize,
            kernel = kernel,
            degree = degree,
            coef0 = coef0,
            cost = cost,
            tolerance = tolerance,
            epsilon = epsilon
        ),
        mlm_task = "classification",
        mlm_architecture = "SVM",
        mlm_framework = "R CARET",
        train = function(training_set) {
            base::message("[mlm_class_svm::train] START")
            base::on.exit(base::message("[mlm_class_svm::train] END"))
            model <- sits::sits_svm(
                formula = formula,
                cachesize = cachesize,
                kernel = kernel,
                degree = degree,
                coef0 = coef0,
                cost = cost,
                tolerance = tolerance,
                epsilon = epsilon
            )
            if (!base::is.null(seed)) {
                base::set.seed(seed)
            }
            .openeocraft_sits_train_logged(
                training_set,
                model,
                "mlm_class_svm::train"
            )
        }
    )
}

# XGBoost classification model spec (sits_xgboost).
#
# Args:
#   learning_rate, min_split_loss, max_depth, nfold, early_stopping_rounds: booster args.
#   seed: Optional RNG seed before training.
#
# Returns:
#   Model spec list with train(training_set).
#
# Raises:
#   Propagates sits/xgboost errors.
#
#* @openeo-process
mlm_class_xgboost <- function(learning_rate = 0.15,
                              min_split_loss = 1,
                              max_depth = 5,
                              nfold = 5,
                              early_stopping_rounds = 20,
                              seed = NULL) {
    base::message("[mlm_class_xgboost] START")
    base::on.exit(base::message("[mlm_class_xgboost] END"))
    list(
        ml_method = sits::sits_xgboost,
        ml_args = list(
            learning_rate = learning_rate,
            min_split_loss = min_split_loss,
            max_depth = max_depth,
            nfold = nfold,
            early_stopping_rounds = early_stopping_rounds
        ),
        mlm_task = "classification",
        mlm_architecture = "XGBoost",
        mlm_framework = "R CARET",
        train = function(training_set) {
            base::message("[mlm_class_xgboost::train] START")
            base::on.exit(base::message("[mlm_class_xgboost::train] END"))
            model <- sits::sits_xgboost(
                learning_rate = learning_rate,
                min_split_loss = min_split_loss,
                max_depth = max_depth,
                nfold = nfold,
                early_stopping_rounds = early_stopping_rounds
            )
            if (!base::is.null(seed)) {
                base::set.seed(seed)
            }
            .openeocraft_sits_train_logged(
                training_set,
                model,
                "mlm_class_xgboost::train"
            )
        }
    )
}


# MLP (torch) classification model spec (sits_mlp).
#
# Args:
#   layers, dropout_rates: Network shape; optimizer: name mapped to torch optim.
#   learning_rate, epsilon, weight_decay: passed in opt_hparams.
#   epochs, batch_size: training schedule; seed: optional RNG.
#
# Returns:
#   Model spec list with train(training_set).
#
# Raises:
#   stop() if optimizer name is unsupported; torch/sits errors during train.
#
#* @openeo-process
mlm_class_mlp <- function(layers = list(512, 512, 512),
                          dropout_rates = list(0.2, 0.3, 0.4),
                          optimizer = "adam",
                          learning_rate = 0.001,
                          epsilon = 0.00000001,
                          weight_decay = 0.000001,
                          epochs = 100,
                          batch_size = 64,
                          seed = NULL) {
    base::message("[mlm_class_mlp] START")
    base::on.exit(base::message("[mlm_class_mlp] END"))
    # start preparing parameters
    optimizer_fn <- base::switch(optimizer,
        "adam" = torch::optim_adamw,
        "adabound" = torch::optim_adabound,
        "adabelief" = torch::optim_adabelief,
        "madagrad" = torch::optim_madagrad,
        "nadam" = torch::optim_nadam,
        "qhadam" = torch::optim_qhadam,
        "radam" = torch::optim_radam,
        "swats" = torch::optim_swats,
        "yogi" = torch::optim_yogi,
        stop(
            "Unsupported optimizer. currently only 'adam, ",
            "adabound, adabelief, madagrad, nadam, qhadam, ",
            "radam, swats, yogi' are supported.",
            call. = FALSE
        )
    )
    opt_hparams <- list(
        lr = learning_rate,
        eps = epsilon,
        weight_decay = weight_decay
    )
    layers <- base::unlist(layers)
    dropout_rates <- base::unlist(dropout_rates)
    # end preparing parameters
    list(
        ml_method = sits::sits_mlp,
        ml_args = list(
            layers = layers,
            dropout_rates = dropout_rates,
            optimizer = optimizer_fn,
            opt_hparams = opt_hparams,
            epochs = epochs,
            batch_size = batch_size
        ),
        mlm_task = "classification",
        mlm_architecture = "MLP",
        mlm_framework = "Torch for R",
        train = function(training_set) {
            base::message("[mlm_class_mlp::train] START")
            base::on.exit(base::message("[mlm_class_mlp::train] END"))
            model <- sits::sits_mlp(
                layers = layers,
                dropout_rates = dropout_rates,
                optimizer = optimizer_fn,
                opt_hparams = opt_hparams,
                epochs = epochs,
                batch_size = batch_size
            )
            if (!base::is.null(seed)) {
                base::set.seed(seed)
            }
            .openeocraft_sits_train_logged(
                training_set,
                model,
                "mlm_class_mlp::train"
            )
        }
    )
}

# TempCNN classification model spec (sits_tempcnn / torch).
#
# Args:
#   cnn_layers, cnn_kernels, cnn_dropout_rates: CNN stack; dense_layer_*: head.
#   optimizer, learning_rate, epsilon, weight_decay, lr_decay_*: optim and schedule.
#   epochs, batch_size, seed, verbose: training controls.
#
# Returns:
#   Model spec list with train(training_set).
#
# Raises:
#   stop() on unsupported optimizer; GPU OOM may surface from sits/torch.
#
#* @openeo-process
mlm_class_tempcnn <- function(cnn_layers = list(64, 64, 64),
                              cnn_kernels = list(5, 5, 5),
                              cnn_dropout_rates = list(0.2, 0.2, 0.2),
                              dense_layer_nodes = 256,
                              dense_layer_dropout_rate = 0.5,
                              optimizer = "adam",
                              learning_rate = 0.0005,
                              epsilon = 0.00000001,
                              weight_decay = 0.000001,
                              lr_decay_epochs = 1,
                              lr_decay_rate = 0.95,
                              epochs = 150,
                              batch_size = 64,
                              seed = NULL,
                              verbose = FALSE) {
    base::message("[mlm_class_tempcnn] START")
    base::on.exit(base::message("[mlm_class_tempcnn] END"))
    # start preparing parameters
    optimizer_fn <- base::switch(optimizer,
        "adam" = torch::optim_adamw,
        "adabound" = torch::optim_adabound,
        "adabelief" = torch::optim_adabelief,
        "madagrad" = torch::optim_madagrad,
        "nadam" = torch::optim_nadam,
        "qhadam" = torch::optim_qhadam,
        "radam" = torch::optim_radam,
        "swats" = torch::optim_swats,
        "yogi" = torch::optim_yogi,
        stop(
            "Unsupported optimizer. currently only 'adam, ",
            "adabound, adabelief, madagrad, nadam, qhadam, ",
            "radam, swats, yogi' are supported.",
            call. = FALSE
        )
    )
    opt_hparams <- list(
        lr = learning_rate,
        eps = epsilon,
        weight_decay = weight_decay
    )
    cnn_layers <- base::unlist(cnn_layers)
    cnn_kernels <- base::unlist(cnn_kernels)
    cnn_dropout_rates <- base::unlist(cnn_dropout_rates)
    verbose <- base::isTRUE(verbose)
    # end preparing parameters
    list(
        ml_method = sits::sits_tempcnn,
        ml_args = list(
            cnn_layers = cnn_layers,
            cnn_kernels = cnn_kernels,
            cnn_dropout_rates = cnn_dropout_rates,
            dense_layer_nodes = dense_layer_nodes,
            dense_layer_dropout_rate = dense_layer_dropout_rate,
            optimizer = optimizer_fn,
            opt_hparams = opt_hparams,
            lr_decay_epochs = lr_decay_epochs,
            lr_decay_rate = lr_decay_rate,
            epochs = epochs,
            batch_size = batch_size,
            verbose = verbose
        ),
        mlm_task = "classification",
        mlm_architecture = "TempCNN",
        mlm_framework = "Torch for R",
        train = function(training_set) {
            base::message("[mlm_class_tempcnn::train] START")
            base::on.exit(base::message("[mlm_class_tempcnn::train] END"))
            model <- sits::sits_tempcnn(
                cnn_layers = cnn_layers,
                cnn_kernels = cnn_kernels,
                cnn_dropout_rates = cnn_dropout_rates,
                dense_layer_nodes = dense_layer_nodes,
                dense_layer_dropout_rate = dense_layer_dropout_rate,
                optimizer = optimizer_fn,
                opt_hparams = opt_hparams,
                lr_decay_epochs = lr_decay_epochs,
                lr_decay_rate = lr_decay_rate,
                epochs = epochs,
                batch_size = batch_size,
                verbose = verbose
            )
            if (!base::is.null(seed)) {
                base::set.seed(seed)
            }
            .openeocraft_sits_train_logged(
                training_set,
                model,
                "mlm_class_tempcnn::train"
            )
        }
    )
}

# TAE (Temporal Autoencoder) classification model spec (sits_tae).
#
# Args:
#   epochs, batch_size: training; optimizer and lr/weight_decay/epsilon: torch optim.
#   lr_decay_epochs, lr_decay_rate: step decay; seed: optional RNG.
#
# Returns:
#   Model spec list with train(training_set).
#
# Raises:
#   stop() on unsupported optimizer.
#
#* @openeo-process
mlm_class_tae <- function(epochs = 150,
                          batch_size = 64,
                          optimizer = "adam",
                          learning_rate = 0.001,
                          epsilon = 0.00000001,
                          weight_decay = 0.000001,
                          lr_decay_epochs = 1,
                          lr_decay_rate = 0.95,
                          seed = NULL) {
    base::message("[mlm_class_tae] START")
    base::on.exit(base::message("[mlm_class_tae] END"))
    # start preparing parameters
    optimizer_fn <- base::switch(optimizer,
        "adam" = torch::optim_adamw,
        "adabound" = torch::optim_adabound,
        "adabelief" = torch::optim_adabelief,
        "madagrad" = torch::optim_madagrad,
        "nadam" = torch::optim_nadam,
        "qhadam" = torch::optim_qhadam,
        "radam" = torch::optim_radam,
        "swats" = torch::optim_swats,
        "yogi" = torch::optim_yogi,
        stop(
            "Unsupported optimizer. currently only 'adam, ",
            "adabound, adabelief, madagrad, nadam, qhadam, ",
            "radam, swats, yogi' are supported.",
            call. = FALSE
        )
    )
    opt_hparams <- list(
        lr = learning_rate,
        eps = epsilon,
        weight_decay = weight_decay
    )
    # end preparing parameters
    list(
        ml_method = sits::sits_tae,
        ml_args = list(
            epochs = epochs,
            batch_size = batch_size,
            optimizer = optimizer_fn,
            opt_hparams = opt_hparams,
            lr_decay_epochs = lr_decay_epochs,
            lr_decay_rate = lr_decay_rate
        ),
        mlm_task = "classification",
        mlm_architecture = "TAE",
        mlm_framework = "Torch for R",
        train = function(training_set) {
            base::message("[mlm_class_tae::train] START")
            base::on.exit(base::message("[mlm_class_tae::train] END"))
            model <- sits::sits_tae(
                epochs = epochs,
                batch_size = batch_size,
                optimizer = optimizer_fn,
                opt_hparams = opt_hparams,
                lr_decay_epochs = lr_decay_epochs,
                lr_decay_rate = lr_decay_rate
            )
            if (!base::is.null(seed)) {
                base::set.seed(seed)
            }
            .openeocraft_sits_train_logged(
                training_set,
                model,
                "mlm_class_tae::train"
            )
        }
    )
}


# LightTAE classification model spec (sits_lighttae).
#
# Args:
#   epochs, batch_size: training; optimizer and lr/epsilon/weight_decay: torch optim.
#   lr_decay_epochs, lr_decay_rate: schedule; seed: optional RNG.
#
# Returns:
#   Model spec list with train(training_set).
#
# Raises:
#   stop() on unsupported optimizer.
#
#* @openeo-process
mlm_class_lighttae <- function(epochs = 150,
                               batch_size = 128,
                               optimizer = "adam",
                               learning_rate = 0.0005,
                               epsilon = 0.00000001,
                               weight_decay = 0.0007,
                               lr_decay_epochs = 50,
                               lr_decay_rate = 1,
                               seed = NULL) {
    base::message("[mlm_class_lighttae] START")
    base::on.exit(base::message("[mlm_class_lighttae] END"))
    # start preparing parameters
    optimizer_fn <- base::switch(optimizer,
        "adam" = torch::optim_adamw,
        "adabound" = torch::optim_adabound,
        "adabelief" = torch::optim_adabelief,
        "madagrad" = torch::optim_madagrad,
        "nadam" = torch::optim_nadam,
        "qhadam" = torch::optim_qhadam,
        "radam" = torch::optim_radam,
        "swats" = torch::optim_swats,
        "yogi" = torch::optim_yogi,
        stop(
            "Unsupported optimizer. currently only 'adam, ",
            "adabound, adabelief, madagrad, nadam, qhadam, ",
            "radam, swats, yogi' are supported.",
            call. = FALSE
        )
    )
    opt_hparams <- list(
        lr = learning_rate,
        eps = epsilon,
        weight_decay = weight_decay
    )
    # end preparing parameters
    list(
        ml_method = sits::sits_lighttae,
        ml_args = list(
            epochs = epochs,
            batch_size = batch_size,
            optimizer = optimizer_fn,
            opt_hparams = opt_hparams,
            lr_decay_epochs = lr_decay_epochs,
            lr_decay_rate = lr_decay_rate
        ),
        mlm_task = "classification",
        mlm_architecture = "LightTAE",
        mlm_framework = "Torch for R",
        train = function(training_set) {
            base::message("[mlm_class_lighttae::train] START")
            base::on.exit(base::message("[mlm_class_lighttae::train] END"))
            model <- sits::sits_lighttae(
                epochs = epochs,
                batch_size = batch_size,
                optimizer = optimizer_fn,
                opt_hparams = opt_hparams,
                lr_decay_epochs = lr_decay_epochs,
                lr_decay_rate = lr_decay_rate
            )
            if (!base::is.null(seed)) {
                base::set.seed(seed)
            }
            .openeocraft_sits_train_logged(
                training_set,
                model,
                "mlm_class_lighttae::train"
            )
        }
    )
}

# Attach MLM metadata to a trained sits model (shared by ml_fit and tuning final fit).
#
# Args:
#   trained_model: Object returned by sits_train / model$train.
#   model_spec: List with mlm_task, mlm_architecture, mlm_framework, ml_args.
#   training_obj: sits samples used for training (metadata extraction).
#   ml_args_for_hyperparameters: If non-NULL, used for mlm_hyperparameters (non-functions
#     only); otherwise model_spec$ml_args.
#
# Returns:
#   trained_model with attributes set (same object).
.openeocraft_attach_ml_fit_metadata <- function(trained_model,
                                                model_spec,
                                                training_obj,
                                                ml_args_for_hyperparameters = NULL) {
    if (!base::is.null(model_spec$mlm_task)) {
        base::attr(trained_model, "mlm_task") <- model_spec$mlm_task
    }
    if (!base::is.null(model_spec$mlm_architecture)) {
        base::attr(trained_model, "mlm_architecture") <- model_spec$mlm_architecture
    }
    if (!base::is.null(model_spec$mlm_framework)) {
        base::attr(trained_model, "mlm_framework") <- model_spec$mlm_framework
    }

    hparam_src <- if (!base::is.null(ml_args_for_hyperparameters)) {
        ml_args_for_hyperparameters
    } else {
        model_spec$ml_args
    }
    if (!base::is.null(hparam_src)) {
        hparams <- base::Filter(
            function(x) !base::is.function(x),
            hparam_src
        )
        if (base::length(hparams) > 0) {
            base::attr(trained_model, "mlm_hyperparameters") <- hparams
        }
    }

    base::tryCatch(
        {
            bands <- sits::sits_bands(training_obj)
            base::attr(trained_model, "mlm_bands") <- bands
        },
        error = function(e) NULL
    )

    base::tryCatch(
        {
            timeline <- sits::sits_timeline(training_obj)
            if (base::length(timeline) > 0) {
                base::attr(trained_model, "mlm_start_datetime") <- base::format(
                    base::min(timeline), "%Y-%m-%dT%H:%M:%SZ"
                )
                base::attr(trained_model, "mlm_end_datetime") <- base::format(
                    base::max(timeline), "%Y-%m-%dT%H:%M:%SZ"
                )
                base::attr(trained_model, "mlm_n_times") <- base::length(timeline)
            }
        },
        error = function(e) NULL
    )

    base::tryCatch(
        {
            labels <- base::unique(training_obj$label)
            if (base::length(labels) > 0) {
                base::attr(trained_model, "mlm_labels") <- labels
            }
        },
        error = function(e) NULL
    )

    base::tryCatch(
        {
            lons <- training_obj$longitude
            lats <- training_obj$latitude
            if (base::length(lons) > 0 && base::length(lats) > 0) {
                west <- base::min(lons, na.rm = TRUE)
                south <- base::min(lats, na.rm = TRUE)
                east <- base::max(lons, na.rm = TRUE)
                north <- base::max(lats, na.rm = TRUE)
                base::attr(trained_model, "mlm_bbox") <- base::c(
                    west, south, east, north
                )
            }
        },
        error = function(e) NULL
    )

    trained_model
}

# Train an openEO ML model spec on samples and attach MLM metadata attributes.
#
# Args:
#   model: List from mlm_class_* with $train function.
#   training_set: URL, RDS path, JSON string, or in-memory sits samples.
#   target: Label column name (reserved; samples use sits conventions).
#
# Returns:
#   Trained sits model with attributes mlm_task, mlm_architecture, bands, bbox, etc.
#
# Raises:
#   Errors from .openeocraft_load_samples (I/O) or model$train / sits.
#
#* @openeo-process
ml_fit <- function(model, training_set, target = "label") {
    base::message("[ml_fit] START")
    base::on.exit(base::message("[ml_fit] END"))
    # Allow training_set as:
    # 1) JSON-serialized object
    # 2) Local path to an RDS file
    # 3) URL to an RDS file
    # 4) Already-loaded object
    training_obj <- .openeocraft_load_samples(
        training_set,
        param_name = "training_set",
        wrap_io_errors = TRUE
    )

    # Log training data sample info
    base::message("[ml_fit] Training data loaded successfully")
    base::message("[ml_fit] Training data summary:")
    base::message("  - Number of samples: ", base::nrow(training_obj))
    base::message("  - Labels: ", base::paste(base::unique(training_obj$label), collapse = ", "))
    base::tryCatch(
        {
            bands <- sits::sits_bands(training_obj)
            base::message("  - Bands: ", base::paste(bands, collapse = ", "))
        },
        error = function(e) {
            NULL
        }
    )
    base::tryCatch(
        {
            timeline <- sits::sits_timeline(training_obj)
            base::message(
                "  - Timeline: ",
                base::length(timeline),
                " dates (",
                timeline[1],
                " to ",
                timeline[base::length(timeline)],
                ")"
            )
        },
        error = function(e) {
            NULL
        }
    )
    base::message("  - First 5 sample locations:")
    for (i in base::seq_len(base::min(5, base::nrow(training_obj)))) {
        base::message(
            "    [",
            i,
            "] label=",
            training_obj$label[i],
            ", lon=",
            base::round(training_obj$longitude[i], 4),
            ", lat=",
            base::round(training_obj$latitude[i], 4)
        )
    }
    base::message("[ml_fit] Training model...")
    trained_model <- model$train(training_obj)
    .openeocraft_attach_ml_fit_metadata(
        trained_model,
        model,
        training_obj,
        ml_args_for_hyperparameters = NULL
    )
}

# Classify a cube with a trained model and apply label layer (openEO).
#
# Args:
#   data: sits raster cube; model: object from ml_fit.
#
# Returns:
#   Cube after sits_classify and sits_label_classification.
#
# Raises:
#   Propagates sits errors; uses job temp dir from current_env().
#
#* @openeo-process
ml_predict <- function(data, model) {
    base::message("[ml_predict] START")
    base::on.exit(base::message("[ml_predict] END"))
    # Model should aware about the right `dimensions` parameter
    # Get current context of evaluation environment
    env <- openeocraft::current_env()
    # Preparing parameters
    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    # Create result directory
    result_dir <- base::file.path(job_dir, "temp")
    if (!base::dir.exists(result_dir)) {
        base::dir.create(result_dir)
    }
    # Get ROI from cube
    roi <- NULL
    if (!base::is.null(base::attr(data, "roi"))) {
        roi <- base::attr(data, "roi")
    }
    # Predict
    data <- sits::sits_classify(
        data = data,
        ml_model = model,
        roi = roi,
        memsize = .openeocraft_memsize(),
        multicores = .openeocraft_multicores(),
        output_dir = result_dir
    )
    # label the probability cube
    data <- sits::sits_label_classification(
        cube = data,
        memsize = .openeocraft_memsize(),
        multicores = .openeocraft_multicores(),
        output_dir = result_dir
    )
    data
}

# Validate a model spec on training data; write metrics.json STAC sidecar.
#
# Args:
#   model: mlm_class_* spec with ml_method and ml_args.
#   training_data, validation_data: Sample sources (see .openeocraft_load_samples).
#   target: Reserved label column name.
#   validation_split: Used when cv <= 1.
#   cv: Folds; if >1 runs sits_kfold_validate.
#   seed: Optional RNG.
#
# Returns:
#   list(metrics = list from metrics.json rows).
#
# Raises:
#   openeocraft::api_stop(400) if model not validatable; load_sample stops.
#
#* @openeo-process
ml_validate <- function(model,
                        training_data,
                        validation_data = NULL,
                        target = "label",
                        validation_split = 0.2,
                        cv = 5,
                        seed = NULL) {
    base::message("[ml_validate] START")
    base::on.exit(base::message("[ml_validate] END"))

    if (!base::is.null(seed)) {
        base::set.seed(seed)
    }

    if (base::is.null(model$ml_method) || base::is.null(model$ml_args)) {
        openeocraft::api_stop(400, "Model does not support validation.")
    }
    ml_method <- base::do.call(model$ml_method, model$ml_args)
    training_obj <- .openeocraft_load_samples(training_data, "training_data")
    base::message("[ml_validate] Training samples loaded: ", base::nrow(training_obj))

    validation_obj <- NULL
    if (!base::is.null(validation_data)) {
        validation_obj <- .openeocraft_load_samples(validation_data, "validation_data")
        base::message("[ml_validate] Validation samples loaded: ", base::nrow(validation_obj))
    }

    base::message("[ml_validate] Running validation...")
    if (base::is.null(cv) || cv <= 1) {
        acc_obj <- sits::sits_validate(
            samples = training_obj,
            samples_validation = validation_obj,
            validation_split = validation_split,
            ml_method = ml_method
        )
    } else {
        base::message("[ml_validate] Using ", cv, "-fold cross-validation")
        acc_obj <- sits::sits_kfold_validate(
            samples = training_obj,
            folds = cv,
            ml_method = ml_method,
            multicores = .openeocraft_multicores(),
            progress = FALSE
        )
    }

    summary_df <- .openeocraft_accuracy_summary_df(acc_obj)
    base::message(
        "[ml_validate] Validation complete. Accuracy: ",
        base::round(summary_df$Accuracy[1], 4)
    )

    metrics_json <- jsonlite::toJSON(
        summary_df,
        dataframe = "rows",
        auto_unbox = TRUE,
        digits = 16
    )

    env <- openeocraft::current_env()
    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    host <- openeocraft::get_host(env$api, env$req)
    metrics_file <- base::file.path(job_dir, "metrics.json")
    base::writeLines(metrics_json, metrics_file)

    asset_href <- openeocraft::make_job_files_url(
        host = host,
        user = env$user,
        job_id = env$job$id,
        file = "metrics.json"
    )
    collection <- openeocraft::job_empty_collection(env$api, env$user, env$job)
    collection$assets <- list(
        metrics = list(
            href = asset_href,
            type = "application/json",
            roles = list("data")
        )
    )
    jsonlite::write_json(
        x = collection,
        path = base::file.path(job_dir, "_collection.json"),
        auto_unbox = TRUE
    )

    list(
        metrics = jsonlite::fromJSON(metrics_json, simplifyVector = FALSE)
    )
}

# K-fold cross-validation with fixed fold count; writes metrics.json like ml_validate.
#
# Args:
#   model: mlm_class_* spec; training_data: sample source; folds: integer >= 2.
#   target, seed: same semantics as ml_validate.
#
# Returns:
#   list(metrics, folds).
#
# Raises:
#   openeocraft::api_stop(400) on bad folds or model; sample load errors.
#
#* @openeo-process
ml_validate_kfold <- function(model,
                              training_data,
                              folds,
                              target = "label",
                              seed = NULL) {
    base::message("[ml_validate_kfold] START")
    base::on.exit(base::message("[ml_validate_kfold] END"))

    if (!base::is.null(seed)) {
        base::set.seed(seed)
    }

    if (base::is.null(folds) || folds < 2) {
        openeocraft::api_stop(400, "folds must be at least 2 for k-fold cross-validation.")
    }

    if (base::is.null(model$ml_method) || base::is.null(model$ml_args)) {
        openeocraft::api_stop(400, "Model does not support validation.")
    }
    ml_method <- base::do.call(model$ml_method, model$ml_args)
    training_obj <- .openeocraft_load_samples(training_data, "training_data")
    base::message("[ml_validate_kfold] Training samples loaded: ", base::nrow(training_obj))
    base::message("[ml_validate_kfold] Running ", folds, "-fold cross-validation...")

    acc_obj <- sits::sits_kfold_validate(
        samples = training_obj,
        folds = folds,
        ml_method = ml_method,
        multicores = .openeocraft_multicores(),
        progress = FALSE
    )

    summary_df <- .openeocraft_accuracy_summary_df(acc_obj)
    base::message(
        "[ml_validate_kfold] Cross-validation complete. Accuracy: ",
        base::round(summary_df$Accuracy[1], 4)
    )

    metrics_json <- jsonlite::toJSON(
        summary_df,
        dataframe = "rows",
        auto_unbox = TRUE,
        digits = 16
    )

    env <- openeocraft::current_env()
    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    host <- openeocraft::get_host(env$api, env$req)
    metrics_file <- base::file.path(job_dir, "metrics.json")
    base::writeLines(metrics_json, metrics_file)

    asset_href <- openeocraft::make_job_files_url(
        host = host,
        user = env$user,
        job_id = env$job$id,
        file = "metrics.json"
    )
    collection <- openeocraft::job_empty_collection(env$api, env$user, env$job)
    collection$assets <- list(
        metrics = list(
            href = asset_href,
            type = "application/json",
            roles = list("data")
        )
    )
    jsonlite::write_json(
        x = collection,
        path = base::file.path(job_dir, "_collection.json"),
        auto_unbox = TRUE,
        digits = 16
    )

    list(
        metrics = jsonlite::fromJSON(metrics_json, simplifyVector = FALSE),
        folds = folds
    )
}

# Exhaustive grid search over discrete parameter combinations.
#
# Args:
#   model: mlm_class_* spec supporting ml_method/ml_args.
#   training_data: Sample source string or object.
#   parameters: Named list of grid dimensions (lists or arrays expanded in order).
#   target: Reserved; scoring: metric column name (e.g. "accuracy").
#   cv, validation_split, seed: validation controls.
#
# Returns:
#   Trained model on the full training set using the best hyperparameters;
#   `tuning_results.json` and job `_collection.json` assets are written as before.
#
# Raises:
#   openeocraft::api_stop(400) if model or parameters invalid.
#
#* @openeo-process
ml_tune_grid <- function(model,
                         training_data,
                         parameters,
                         target = "label",
                         scoring = "accuracy",
                         cv = 5,
                         validation_split = 0.2,
                         seed = NULL) {
    base::message("[ml_tune_grid] START")
    base::on.exit(base::message("[ml_tune_grid] END"))
    log_msg <- function(...) {
        base::message("[ml_tune_grid] ", ...)
    }
    if (!base::is.null(seed)) {
        base::set.seed(seed)
    }

    if (base::is.null(model$ml_method) || base::is.null(model$ml_args)) {
        openeocraft::api_stop(400, "Model does not support tuning.")
    }
    if (!base::is.list(parameters) || base::length(parameters) == 0) {
        openeocraft::api_stop(400, "parameters must be a non-empty object.")
    }

    normalize_grid_values <- function(values) {
        if (base::is.list(values)) {
            return(values)
        }
        if (base::is.array(values)) {
            if (base::length(base::dim(values)) >= 2) {
                return(base::lapply(base::seq_len(base::nrow(values)), function(i) {
                    values[i, , drop = TRUE]
                }))
            }
            return(base::as.list(values))
        }
        base::as.list(values)
    }

    expand_params <- function(params, keys = base::names(params)) {
        if (base::length(keys) == 0) {
            return(list(list()))
        }
        key <- keys[[1]]
        rest <- keys[-1]
        values <- normalize_grid_values(params[[key]])
        combos <- list()
        for (value in values) {
            for (suffix in expand_params(params, rest)) {
                combos <- c(combos, list(base::c(suffix, stats::setNames(list(value), key))))
            }
        }
        combos
    }

    training_obj <- .openeocraft_load_samples(training_data, "training_data")
    log_msg("Training samples loaded: ", base::nrow(training_obj))
    param_sets <- expand_params(parameters)
    results <- .openeocraft_tune_evaluate_param_sets(
        model = model,
        training_obj = training_obj,
        param_sets = param_sets,
        cv = cv,
        validation_split = validation_split,
        scoring = scoring,
        log_msg = log_msg,
        optimizer_context = "param_grid"
    )
    fin <- .openeocraft_tune_finalize_results(results)
    .openeocraft_tune_final_train(
        model,
        training_obj,
        fin$best_params,
        "param_grid",
        log_msg,
        "ml_tune_grid::final_train"
    )
}

# Random search: sample n_iter combinations from parameter distributions.
#
# Args:
#   model, training_data, target, scoring, cv, validation_split, seed: as ml_tune_grid.
#   parameters: Named list; values can be distributions (type uniform, log_uniform, etc.).
#   n_iter: Number of random draws.
#
# Returns:
#   Same as ml_tune_grid (trained model + tuning_results.json sidecar).
#
# Raises:
#   openeocraft::api_stop(400) on invalid model or parameters.
#
#* @openeo-process
ml_tune_random <- function(model,
                           training_data,
                           parameters,
                           n_iter = 10,
                           target = "label",
                           scoring = "accuracy",
                           cv = 5,
                           validation_split = 0.2,
                           seed = NULL) {
    base::message("[ml_tune_random] START")
    base::on.exit(base::message("[ml_tune_random] END"))
    log_msg <- function(...) {
        base::message("[ml_tune_random] ", ...)
    }

    if (!base::is.null(seed)) {
        base::set.seed(seed)
    }

    if (base::is.null(model$ml_method) || base::is.null(model$ml_args)) {
        openeocraft::api_stop(400, "Model does not support tuning.")
    }
    if (!base::is.list(parameters) || base::length(parameters) == 0) {
        openeocraft::api_stop(400, "parameters must be a non-empty object.")
    }

    sample_from_dist <- function(param_spec) {
        if (base::is.list(param_spec) && !base::is.null(param_spec$type)) {
            dist_type <- param_spec$type
            if (dist_type == "uniform") {
                return(stats::runif(1, min = param_spec$min, max = param_spec$max))
            } else if (dist_type == "log_uniform") {
                log_min <- base::log(param_spec$min)
                log_max <- base::log(param_spec$max)
                return(base::exp(stats::runif(1, min = log_min, max = log_max)))
            } else if (dist_type == "int_uniform") {
                return(base::sample(
                    base::seq(
                        from = base::as.integer(param_spec$min),
                        to = base::as.integer(param_spec$max)
                    ),
                    size = 1
                ))
            } else if (dist_type == "choice") {
                return(base::sample(param_spec$values, size = 1)[[1]])
            }
        }
        if (base::is.list(param_spec) || base::is.vector(param_spec)) {
            vals <- base::unlist(param_spec, recursive = FALSE)
            return(vals[[base::sample(base::length(vals), 1)]])
        }
        param_spec
    }

    sample_params <- function(params) {
        result <- list()
        for (key in base::names(params)) {
            result[[key]] <- sample_from_dist(params[[key]])
        }
        result
    }

    training_obj <- .openeocraft_load_samples(training_data, "training_data")
    log_msg("Training samples loaded: ", base::nrow(training_obj))
    log_msg("Generating ", n_iter, " random parameter combinations.")

    param_sets <- base::lapply(base::seq_len(n_iter), function(i) sample_params(parameters))
    results <- .openeocraft_tune_evaluate_param_sets(
        model = model,
        training_obj = training_obj,
        param_sets = param_sets,
        cv = cv,
        validation_split = validation_split,
        scoring = scoring,
        log_msg = log_msg,
        optimizer_context = "parameters"
    )
    fin <- .openeocraft_tune_finalize_results(results)
    .openeocraft_tune_final_train(
        model,
        training_obj,
        fin$best_params,
        "parameters",
        log_msg,
        "ml_tune_random::final_train"
    )
}

# Classify cube and return probability cube without final label layer (sits_classify only).
#
# Args:
#   data: sits cube; model: trained classifier.
#
# Returns:
#   Classified cube with per-class probabilities.
#
# Raises:
#   Propagates sits errors.
#
#* @openeo-process
ml_predict_probabilities <- function(data, model) {
    base::message("[ml_predict_probabilities] START")
    base::on.exit(base::message("[ml_predict_probabilities] END"))
    # Model should aware about the right `dimensions` parameter
    # Get current context of evaluation environment
    env <- openeocraft::current_env()
    # Preparing parameters
    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    # Create result directory
    result_dir <- base::file.path(job_dir, "temp")
    if (!base::dir.exists(result_dir)) {
        base::dir.create(result_dir)
    }
    # Get ROI from cube
    roi <- NULL
    if (!base::is.null(base::attr(data, "roi"))) {
        roi <- base::attr(data, "roi")
    }
    # Predict
    data <- sits::sits_classify(
        data = data,
        ml_model = model,
        roi = roi,
        memsize = .openeocraft_memsize(),
        multicores = .openeocraft_multicores(),
        output_dir = result_dir
    )
    data
}


# Uncertainty layer from a probability cube (sits_uncertainty).
#
# Args:
#   data: Probability or classification cube; approach: sits uncertainty type (e.g. "margin").
#
# Returns:
#   Cube with uncertainty band.
#
# Raises:
#   Propagates sits errors.
#
#* @openeo-process
ml_uncertainty_class <- function(data, approach = "margin") {
    base::message("[ml_uncertainty_class] START")
    base::on.exit(base::message("[ml_uncertainty_class] END"))
    # Get current context of evaluation environment
    env <- openeocraft::current_env()
    # Preparing parameters
    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    # Create result directory
    result_dir <- base::file.path(job_dir, "temp")
    if (!base::dir.exists(result_dir)) {
        base::dir.create(result_dir)
    }
    # label the probability cube
    data <- sits::sits_uncertainty(
        cube = data,
        type = approach,
        memsize = .openeocraft_memsize(),
        multicores = .openeocraft_multicores(),
        output_dir = result_dir
    )
    data
}

# Spatial/temporal smoothing of classification cube (sits_smooth).
#
# Args:
#   data: Cube; window_size, neighborhood_fraction, smoothness: sits_smooth args.
#
# Returns:
#   Smoothed cube.
#
# Raises:
#   Propagates sits errors.
#
#* @openeo-process
ml_smooth_class <- function(data,
                            window_size = 7L,
                            neighborhood_fraction = 0.5,
                            smoothness = 10L) {
    base::message("[ml_smooth_class] START")
    base::on.exit(base::message("[ml_smooth_class] END"))
    # Get current context of evaluation environment
    env <- openeocraft::current_env()
    # Preparing parameters
    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    # Create result directory
    result_dir <- base::file.path(job_dir, "temp")
    if (!base::dir.exists(result_dir)) {
        base::dir.create(result_dir)
    }
    # label the probability cube
    data <- sits::sits_smooth(
        cube = data,
        window_size = window_size,
        neigh_fraction = neighborhood_fraction,
        smoothness = smoothness,
        memsize = .openeocraft_memsize(),
        multicores = .openeocraft_multicores(),
        output_dir = result_dir
    )
    data
}

# Convert probability cube to hard class labels (sits_label_classification).
#
# Args:
#   data: Probability cube from classification.
#
# Returns:
#   Labelled cube.
#
# Raises:
#   Propagates sits errors.
#
#* @openeo-process
ml_label_class <- function(data) {
    base::message("[ml_label_class] START")
    base::on.exit(base::message("[ml_label_class] END"))
    # Get current context of evaluation environment
    env <- openeocraft::current_env()
    # Preparing parameters
    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    # Create result directory
    result_dir <- base::file.path(job_dir, "temp")
    if (!base::dir.exists(result_dir)) {
        base::dir.create(result_dir)
    }
    # label the probability cube
    data <- sits::sits_label_classification(
        cube = data,
        memsize = .openeocraft_memsize(),
        multicores = .openeocraft_multicores(),
        output_dir = result_dir
    )
    data
}

# Align cube to a regular time grid and resolution (sits_regularize).
#
# Args:
#   data: sits cube; resolution: spatial res; period: time step string for sits.
#
# Returns:
#   Regularized cube.
#
# Raises:
#   Propagates sits errors (I/O heavy).
#
#* @openeo-process
cube_regularize <- function(data, resolution, period) {
    base::message("[cube_regularize] START")
    base::on.exit(base::message("[cube_regularize] END"))
    # Get current context of evaluation environment
    env <- openeocraft::current_env()
    # Preparing parameters
    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    # Create result directory
    result_dir <- base::file.path(job_dir, "temp")
    if (!base::dir.exists(result_dir)) {
        base::dir.create(result_dir)
    }
    # Get ROI from cube
    roi <- NULL
    if (!base::is.null(base::attr(data, "roi"))) {
        roi <- base::attr(data, "roi")
    }

    # Regularize (often the slowest step: I/O + time-series alignment; progress
    # bars may stall for a long time between updates, especially over AWS/STAC.)
    mc <- .openeocraft_multicores()
    base::message(
        "[cube_regularize] sits_regularize(period=",
        period,
        ", res=",
        resolution,
        ", multicores=",
        mc,
        ") ..."
    )
    data <- sits::sits_regularize(
        cube = data,
        period = period,
        res = resolution,
        output_dir = result_dir,
        roi = roi,
        multicores = mc
    )
    base::message("[cube_regularize] sits_regularize() returned")
    data
}

# Compute NDVI from NIR and red bands via internal sits_apply raster pass.
#
# Args:
#   data: Regular sits raster cube; nir, red: source band names; target_band: output band name.
#
# Returns:
#   Cube with NDVI band added (or unchanged if band exists and sits warns).
#
# Raises:
#   Propagates sits internal check errors; depends on sits private APIs.
#
#* @openeo-process
ndvi <- function(data,
                 nir = "nir",
                 red = "red",
                 target_band = NULL) {
    base::message("[ndvi] START")
    base::on.exit(base::message("[ndvi] END"))
    # Get current context of evaluation environment
    env <- openeocraft::current_env()
    # Preparing parameters
    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    # Create result directory
    result_dir <- base::file.path(job_dir, "temp")
    if (!base::dir.exists(result_dir)) {
        base::dir.create(result_dir)
    }
    #
    # sits apply re-implementation
    sits:::sits_apply.raster_cube
    .sits_apply <- function(data, out_band, expr) {
        window_size <- 1L
        memsize <- .openeocraft_memsize()
        multicores <- .openeocraft_multicores()
        normalized <- TRUE
        progress <- FALSE
        sits:::.check_is_raster_cube(data)
        sits:::.check_that(sits:::.cube_is_regular(data))
        sits:::.check_int_parameter(window_size, min = 1, is_odd = TRUE)
        sits:::.check_lgl_parameter(normalized)
        sits:::.check_int_parameter(memsize, min = 1, max = 16384)
        sits:::.check_int_parameter(multicores, min = 1, max = 2048)
        sits:::.check_output_dir(result_dir)
        bands <- sits:::.cube_bands(data)
        # re-implementation of sits:::.apply_capture_expression()
        expr <- list(expr)
        out_band <- base::toupper(base::gsub("_", "-", out_band, fixed = TRUE))
        #
        base::names(expr) <- out_band
        if (out_band %in% bands) {
            if (sits:::.check_messages()) {
                warning(sits:::.conf("messages", "sits_apply_out_band"),
                    call. = FALSE
                )
            }
            return(data)
        }
        in_bands <- sits:::.apply_input_bands(
            cube = data,
            bands = bands,
            expr = expr
        )
        overlap <- base::ceiling(window_size / 2) - 1
        block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(sits:::.tile_path(data)))
        job_memsize <- sits:::.jobs_block_memsize(
            block_size = sits:::.block_size(block = block, overlap = overlap),
            npaths = base::length(in_bands) + 1,
            nbytes = 8,
            proc_bloat = sits:::.conf("processing_bloat_cpu")
        )
        block <- sits:::.jobs_optimal_block(
            job_block_memsize = job_memsize,
            block = block,
            image_size = sits:::.tile_size(sits:::.tile(data)),
            memsize = memsize,
            multicores = multicores
        )
        block <- sits:::.block_regulate_size(block)
        multicores <- sits:::.jobs_max_multicores(
            job_block_memsize = job_memsize,
            memsize = memsize,
            multicores = multicores
        )
        sits:::.parallel_start(workers = multicores)
        base::on.exit(sits:::.parallel_stop(), add = TRUE)
        features_cube <- sits:::.cube_split_features(data)
        features_band <- sits:::.jobs_map_parallel_dfr(features_cube, function(feature) {
            output_feature <- sits:::.apply_feature(
                feature = feature,
                block = block,
                expr = expr,
                window_size = window_size,
                out_band = out_band,
                in_bands = in_bands,
                overlap = overlap,
                normalized = normalized,
                output_dir = result_dir
            )
            output_feature
        }, progress = progress)
        sits:::.cube_merge_tiles(dplyr::bind_rows(list(features_cube, features_band)))
    }
    #
    data <- .sits_apply(
        data = data,
        out_band = target_band,
        expr = base::bquote((
            .(base::as.name(nir)) - .(base::as.name(red))
        ) /
            (
                .(base::as.name(nir)) + .(base::as.name(red))
            ))
    )
    data
}

# Merge two sits cubes along shared dimensions (sits_merge).
#
# Args:
#   cube1, cube2: sits cubes; overlap_resolver, context: reserved for openEO API parity.
#
# Returns:
#   Merged cube.
#
# Raises:
#   Propagates sits_merge errors.
#
#* @openeo-process
merge_cubes <- function(cube1,
                        cube2,
                        overlap_resolver = NULL,
                        context = NULL) {
    base::message("[merge_cubes] START")
    base::on.exit(base::message("[merge_cubes] END"))
    # Get current context of evaluation environment
    env <- openeocraft::current_env()
    # Merge cubes
    data <- sits::sits_merge(data1 = cube1, data2 = cube2)
    data
}


# Subset cube to requested band names (sits_select); wavelengths not supported.
#
# Args:
#   data: sits cube; bands: character vector or list of names; wavelengths: must be NULL.
#
# Returns:
#   Filtered cube, or original cube with warning if no intersection.
#
# Raises:
#   stop() if data missing, wavelengths set, bands invalid, or sits_select fails.
#
#* @openeo-process
filter_bands <- function(data,
                         bands = NULL,
                         wavelengths = NULL) {
    base::message("[filter_bands] START")
    base::on.exit(base::message("[filter_bands] END"))
    # Input validation
    if (base::missing(data)) {
        stop("Argument 'data' is required", call. = FALSE)
    }

    # Check if wavelengths parameter is used (not supported)
    if (!base::is.null(wavelengths)) {
        stop(
            "Filtering by wavelength is currently not supported. Please use the 'bands' parameter instead.",
            call. = FALSE
        )
    }

    # Validate bands parameter
    if (base::is.null(bands)) {
        stop(
            "The 'bands' parameter is required and must be a non-empty character vector or list",
            call. = FALSE
        )
    }

    # Convert bands to character vector if it's a list (e.g., from JSON/API input)
    if (base::is.list(bands)) {
        bands <- base::unlist(bands, use.names = FALSE)
    }

    if (!base::is.character(bands) || base::length(bands) == 0) {
        stop("'bands' must be a non-empty character vector or list of band names",
            call. = FALSE
        )
    }

    # Get band information from the data cube
    cube_bands <- sits::sits_bands(data)

    # If no bands found, stop with error
    if (base::length(cube_bands) == 0) {
        stop("No bands found in the input data cube", call. = FALSE)
    }

    # Find intersection of requested bands and available bands
    matched_bands <- base::intersect(bands, cube_bands)

    # If no bands matched, return the original cube with a warning
    if (base::length(matched_bands) == 0) {
        warning("No bands matched the filter criteria. Returning the original cube.",
            call. = FALSE
        )
        return(data)
    }

    # Use sits_select to filter the bands
    result <- tryCatch(
        {
            sits::sits_select(data, bands = matched_bands)
        },
        error = function(e) {
            stop(base::sprintf("Error filtering bands: %s", e$message),
                call. = FALSE
            )
        }
    )

    return(result)
}

# save_result: optional GeoTIFF mosaic (terra) --------------------------------

# merge_mode: "auto" (default) = mosaic GeoTIFF when multiple tiles exist;
# "yes" = always mosaic when GeoTIFF (e.g. single tile + target_crs); "no" = per-tile assets.

# Parse save_result options for GeoTIFF merge behavior and optional reprojection.
#
# Args:
#   options: List or NULL; may contain merge_tiles (logical or string) and target_crs.
#
# Returns:
#   list(merge_mode = "auto"|"yes"|"no", target_crs = character or NULL).
.openeocraft_save_result_merge_options <- function(options) {
    merge_mode <- "auto"
    target_crs <- NULL
    if (!base::is.null(options) && base::is.list(options)) {
        mt <- options[["merge_tiles"]]
        if (!base::is.null(mt)) {
            if (base::isTRUE(mt)) {
                merge_mode <- "yes"
            } else if (base::is.logical(mt) && base::length(mt) == 1L && !mt) {
                merge_mode <- "no"
            } else if (base::is.character(mt) && base::length(mt) == 1L) {
                tl <- base::tolower(mt)
                if (tl %in% base::c("true", "1", "yes")) {
                    merge_mode <- "yes"
                } else if (tl %in% base::c("false", "0", "no")) {
                    merge_mode <- "no"
                }
            }
        }
        tc <- options[["target_crs"]]
        if (!base::is.null(tc)) {
            tcs <- base::as.character(tc)
            if (base::length(tcs) >= 1L && base::nzchar(tcs[[1]])) {
                target_crs <- tcs[[1]]
            }
        }
    }
    base::list(merge_mode = merge_mode, target_crs = target_crs)
}

# Collect unique on-disk raster paths from cube file_info rows.
#
# Args:
#   data: sits cube with file_info paths.
#
# Returns:
#   Character vector of existing file paths (may be empty).
.openeocraft_collect_local_raster_paths <- function(data) {
    out <- base::character(0)
    n <- base::nrow(data)
    for (i in base::seq_len(n)) {
        fi <- data$file_info[[i]]
        if (base::is.null(fi) || base::is.null(fi$path)) {
            next
        }
        p <- fi$path
        if (base::is.character(p)) {
            out <- c(out, p)
        } else {
            u <- base::unlist(p, use.names = FALSE)
            if (base::is.character(u)) {
                out <- c(out, u)
            }
        }
    }
    out <- base::unique(out[base::nzchar(out) & base::file.exists(out)])
    out
}

# Whether format string denotes GeoTIFF output for merge logic.
#
# Args:
#   format: Character scalar or NULL.
#
# Returns:
#   TRUE if recognized as GTiff/GeoTIFF/tif.
.openeocraft_format_is_geotiff <- function(format) {
    if (base::is.null(format) ||
        !base::is.character(format) ||
        base::length(format) != 1L) {
        return(FALSE)
    }
    f <- base::tolower(format)
    f <- base::gsub("[^a-z0-9]", "", f)
    base::grepl("gtiff|geotiff", f) || base::identical(f, "tif")
}

# Merge multiple GeoTIFFs with terra and optionally reproject.
#
# Args:
#   paths: Character vector of existing .tif paths; out_path: output file;
#   target_crs: Optional WKT/EPSG string for terra::project.
#
# Returns:
#   TRUE invisibly on success.
#
# Raises:
#   stop() if terra missing, no paths, or write/project fails.
.openeocraft_merge_geotiff_files <- function(paths, out_path, target_crs) {
    if (!base::requireNamespace("terra", quietly = TRUE)) {
        stop(
            "save_result GeoTIFF merge requires the terra package on the worker.",
            call. = FALSE
        )
    }
    paths <- paths[base::file.exists(paths)]
    if (base::length(paths) < 1L) {
        stop("merge_tiles: no readable raster files on disk.", call. = FALSE)
    }
    rl <- base::lapply(paths, function(p) terra::rast(p))
    m <- rl[[1]]
    if (base::length(rl) > 1L) {
        for (k in base::seq_len(base::length(rl))[-1]) {
            m <- terra::merge(m, rl[[k]])
        }
    }
    if (!base::is.null(target_crs)) {
        m <- terra::project(m, target_crs, method = "near", gdal = TRUE)
    }
    terra::writeRaster(
        m,
        out_path,
        overwrite = TRUE,
        gdal = base::c("COMPRESS=LZW", "TILED=YES")
    )
    base::invisible(TRUE)
}

# Export cube to job directory, write STAC _collection.json, optional GeoTIFF mosaic.
#
# Args:
#   data: sits cube; format: output format string; options: list merge_tiles, target_crs.
#
# Returns:
#   TRUE on success.
#
# Raises:
#   openeocraft::api_stop on folder creation, invalid merge options, merge failures.
#
#* @openeo-process
save_result <- function(data, format, options = NULL) {
    base::message("[save_result] START")
    base::on.exit(base::message("[save_result] END"))
    env <- openeocraft::current_env()
    host <- openeocraft::get_host(env$api, env$req)
    result_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    obj_dir <- base::file.path(result_dir, ".obj")
    # Create result directory
    if (!base::dir.exists(obj_dir)) {
        base::dir.create(obj_dir, recursive = TRUE)
    }
    if (!base::dir.exists(obj_dir)) {
        openeocraft::api_stop(500, "Could not create the folder")
    }
    # Get ROI from cube
    roi <- NULL
    if (!base::is.null(base::attr(data, "roi"))) {
        roi <- base::attr(data, "roi")
    }
    # Copy files result
    data <- sits::sits_cube_copy(
        cube = data,
        roi = roi,
        multicores = .openeocraft_multicores(),
        output_dir = result_dir
    )
    # Save RDS object representation
    file <- base::file.path(result_dir, ".obj", "cube.rds")
    base::saveRDS(data, file)

    merge_opts <- .openeocraft_save_result_merge_options(options)
    is_geotiff <- .openeocraft_format_is_geotiff(format)

    if (merge_opts$merge_mode == "yes" && !is_geotiff) {
        openeocraft::api_stop(
            400,
            "save_result: merge_tiles is only supported for GeoTIFF output format."
        )
    }

    tile_paths <- if (is_geotiff) {
        .openeocraft_collect_local_raster_paths(data)
    } else {
        base::character(0)
    }

    n_paths <- base::length(tile_paths)
    multi_tile <- n_paths > 1L
    want_merge <- FALSE
    if (is_geotiff && merge_opts$merge_mode != "no") {
        if (merge_opts$merge_mode == "yes") {
            want_merge <- n_paths >= 1L
        } else {
            want_merge <- multi_tile
        }
    }

    terra_ok <- base::requireNamespace("terra", quietly = TRUE)
    if (want_merge && !terra_ok) {
        if (merge_opts$merge_mode == "yes") {
            openeocraft::api_stop(
                500,
                "save_result: merge_tiles requires the terra package on the worker."
            )
        }
        base::message(
            "[save_result] GeoTIFF multi-tile merge skipped (terra not installed); ",
            "using per-tile result assets."
        )
        want_merge <- FALSE
    }

    if (want_merge) {
        if (base::nrow(data) < 1L) {
            openeocraft::api_stop(400, "save_result GeoTIFF merge: empty cube.")
        }
        if (n_paths < 1L) {
            openeocraft::api_stop(
                400,
                "save_result GeoTIFF merge: no on-disk raster files found after export."
            )
        }
        merged_name <- "openeocraft_merged.tif"
        merged_abs <- base::file.path(result_dir, merged_name)
        base::message(
            "[save_result] GeoTIFF merge (",
            merge_opts$merge_mode,
            "): ",
            n_paths,
            " file(s) -> ",
            merged_name,
            if (!base::is.null(merge_opts$target_crs)) {
                base::paste0(" (target_crs=", merge_opts$target_crs, ")")
            } else {
                ""
            }
        )
        base::tryCatch(
            .openeocraft_merge_geotiff_files(
                tile_paths,
                merged_abs,
                merge_opts$target_crs
            ),
            error = function(e) {
                openeocraft::api_stop(
                    500,
                    base::paste("save_result GeoTIFF merge:", e$message)
                )
            }
        )
        merged_href <- openeocraft::make_job_files_url(
            host = host,
            user = env$user,
            job_id = env$job$id,
            file = merged_name
        )
        assets <- base::list()
        assets[[merged_name]] <- base::list(
            href = merged_href,
            type = openeocraft::format_content_type(format),
            roles = base::list("data")
        )
    } else {
        # Create assets list (one STAC entry per tile file)
        assets <- base::list()
        for (i in base::seq_len(base::nrow(data))) {
            # Change URLs to allow client access files
            filename <- base::basename(data$file_info[[i]]$path)
            data$file_info[[i]]$path <- openeocraft::make_job_files_url(
                host = host,
                user = env$user,
                job_id = env$job$id,
                file = filename
            )
            # Transform sits_cube into STAC assets
            tile_assets <- base::lapply(data$file_info[[i]]$path, \(path) {
                base::list(
                    href = path,
                    # TODO: implement format_content_type() function
                    type = openeocraft::format_content_type(format),
                    roles = base::list("data")
                )
            })
            base::names(tile_assets) <- filename
            assets <- c(assets, tile_assets)
        }
    }

    collection <- openeocraft::job_empty_collection(env$api, env$user, env$job)
    collection$assets <- assets
    jsonlite::write_json(
        x = collection,
        path = base::file.path(result_dir, "_collection.json"),
        auto_unbox = TRUE
    )
    return(TRUE)
}

# Load a cube previously saved by save_result from another job id.
#
# Args:
#   id: Job id whose result_dir contains .obj/cube.rds.
#
# Returns:
#   sits cube from RDS.
#
# Raises:
#   openeocraft::api_stop(500) if paths missing.
#
#* @openeo-process
load_result <- function(id) {
    base::message("[load_result] START")
    base::on.exit(base::message("[load_result] END"))
    # Get current context of evaluation environment
    env <- openeocraft::current_env()
    # Get result directory
    result_dir <- openeocraft::job_get_dir(env$api, env$user, id)
    obj_dir <- base::file.path(result_dir, ".obj")
    if (!base::dir.exists(obj_dir)) {
        openeocraft::api_stop(500, "Object's representation folder doesn't exist")
    }
    # Get RDS object representation
    file <- base::file.path(result_dir, ".obj", "cube.rds")
    if (!base::file.exists(file)) {
        openeocraft::api_stop(500, "Object's representation file doesn't exist")
    }
    data <- base::readRDS(file)
    data
}

# Copy cube to user workspace folder and write named RDS + STAC collection metadata.
#
# Args:
#   data: sits cube; name: stem for .rds under .obj; folder: workspace subfolder.
#
# Returns:
#   Modified cube with workspace URLs in file_info.
#
# Raises:
#   openeocraft::api_stop(500) if directories cannot be created.
#
#* @openeo-process
export_cube <- function(data, name, folder) {
    base::message("[export_cube] START")
    base::on.exit(base::message("[export_cube] END"))
    env <- openeocraft::current_env()
    host <- openeocraft::get_host(env$api, env$req)
    # Get workspace directory
    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    workspace_dir <- openeocraft::api_user_workspace(env$api, env$user)
    workspace_dir <- base::file.path(workspace_dir, "root")
    result_dir <- base::file.path(workspace_dir, folder)
    obj_dir <- base::file.path(result_dir, ".obj")
    # Create result directory
    if (!base::dir.exists(obj_dir)) {
        base::dir.create(obj_dir, recursive = TRUE)
    }
    if (!base::dir.exists(obj_dir)) {
        openeocraft::api_stop(500, "Could not create the folder")
    }
    # Get ROI from cube
    roi <- NULL
    if (!base::is.null(base::attr(data, "roi"))) {
        roi <- base::attr(data, "roi")
    }
    # Copy files result
    data <- sits::sits_cube_copy(
        cube = data,
        roi = roi,
        multicores = .openeocraft_multicores(),
        output_dir = result_dir
    )
    # Save RDS object representation
    file <- base::file.path(result_dir, ".obj", base::paste0(name, ".rds"))
    base::saveRDS(data, file)
    # Create assets list
    assets <- list()
    for (i in base::seq_len(base::nrow(data))) {
        # Change URLs to allow client access files
        filename <- base::basename(data$file_info[[i]]$path)
        data$file_info[[i]]$path <- openeocraft::make_workspace_files_url(
            host = host,
            user = env$user,
            folder = folder,
            file = filename
        )
        # Transform sits_cube into STAC assets
        tile_assets <- base::lapply(data$file_info[[i]]$path, \(path) {
            list(
                href = path,
                # TODO: implement format_content_type() function
                type = openeocraft::format_content_type("gtiff"),
                roles = list("data")
            )
        })
        base::names(tile_assets) <- filename
        assets <- c(assets, tile_assets)
    }
    collection <- openeocraft::job_empty_collection(env$api, env$user, env$job)
    collection$assets <- assets
    # Save collection in workspace directory
    jsonlite::write_json(
        x = collection,
        path = base::file.path(result_dir, "_collection.json"),
        auto_unbox = TRUE
    )
    # Save also in job directory
    jsonlite::write_json(
        x = collection,
        path = base::file.path(job_dir, "_collection.json"),
        auto_unbox = TRUE
    )
    data
}

# Load cube RDS from user workspace (inverse of export_cube).
#
# Args:
#   name: RDS stem; folder: workspace subfolder containing .obj/name.rds.
#
# Returns:
#   sits cube.
#
# Raises:
#   openeocraft::api_stop(500) if folder or file missing.
#
#* @openeo-process
import_cube <- function(name, folder) {
    base::message("[import_cube] START")
    base::on.exit(base::message("[import_cube] END"))
    # Get current context of evaluation environment
    env <- openeocraft::current_env()
    # Get workspace directory
    workspace_dir <- openeocraft::api_user_workspace(env$api, env$user)
    workspace_dir <- base::file.path(workspace_dir, "root")
    # Get result directory
    result_dir <- base::file.path(workspace_dir, folder)
    if (!base::dir.exists(result_dir)) {
        openeocraft::api_stop(500, "Folder does not exist")
    }
    # Get RDS object representation
    file <- base::file.path(result_dir, ".obj", base::paste0(name, ".rds"))
    if (!base::file.exists(file)) {
        openeocraft::api_stop(500, "File does not exist")
    }
    data <- base::readRDS(file)
    data
}

# Save trained model RDS under workspace folder and write minimal STAC collection.
#
# Args:
#   model: Trained sits model; name: stem for .rds; folder: workspace path.
#
# Returns:
#   model (unchanged).
#
# Raises:
#   openeocraft::api_stop(500) if folder creation fails.
#
#* @openeo-process
export_ml_model <- function(model, name, folder) {
    base::message("[export_ml_model] START")
    base::on.exit(base::message("[export_ml_model] END"))
    env <- openeocraft::current_env()
    # Get workspace directory
    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    workspace_dir <- openeocraft::api_user_workspace(env$api, env$user)
    workspace_dir <- base::file.path(workspace_dir, "root")
    result_dir <- base::file.path(workspace_dir, folder)
    obj_dir <- base::file.path(result_dir, ".obj")
    # Create result directory
    if (!base::dir.exists(obj_dir)) {
        base::dir.create(obj_dir, recursive = TRUE)
    }
    if (!base::dir.exists(obj_dir)) {
        openeocraft::api_stop(500, "Could not create the folder")
    }
    # Save RDS object representation
    file <- base::file.path(result_dir, ".obj", base::paste0(name, ".rds"))
    base::saveRDS(model, file)
    # Create assets list
    # TODO: point to the model
    assets <- list()
    collection <- openeocraft::job_empty_collection(env$api, env$user, env$job)
    collection$assets <- assets
    # Save collection in workspace directory
    jsonlite::write_json(
        x = collection,
        path = base::file.path(result_dir, "_collection.json"),
        auto_unbox = TRUE
    )
    # Save also in job directory
    jsonlite::write_json(
        x = collection,
        path = base::file.path(job_dir, "_collection.json"),
        auto_unbox = TRUE
    )
    model
}

# Load model RDS from workspace (inverse of export_ml_model).
#
# Args:
#   name, folder: Same as export_ml_model.
#
# Returns:
#   Model object.
#
# Raises:
#   openeocraft::api_stop(500) if missing.
#
#* @openeo-process
import_ml_model <- function(name, folder) {
    base::message("[import_ml_model] START")
    base::on.exit(base::message("[import_ml_model] END"))
    # Get current context of evaluation environment
    env <- openeocraft::current_env()
    # Get workspace directory
    workspace_dir <- openeocraft::api_user_workspace(env$api, env$user)
    workspace_dir <- base::file.path(workspace_dir, "root")
    # Get result directory
    result_dir <- base::file.path(workspace_dir, folder)
    if (!base::dir.exists(result_dir)) {
        openeocraft::api_stop(500, "Folder does not exist")
    }
    # Get RDS object representation
    file <- base::file.path(result_dir, ".obj", base::paste0(name, ".rds"))
    if (!base::file.exists(file)) {
        openeocraft::api_stop(500, "File does not exist")
    }
    data <- base::readRDS(file)
    data
}

# Persist model to job and public STAC MLM items; update models collection JSON.
#
# Args:
#   data: Trained model with optional mlm_* attributes from ml_fit.
#   name: Model id / filename stem; options: MLM overrides and optional code_asset.
#
# Returns:
#   TRUE on success; FALSE on non-HTTP runtime errors inside tryCatch.
#
# Raises:
#   openeocraft::api_stop on validation; rethrows api errors from inner tryCatch.
#
#* @openeo-process
save_ml_model <- function(data, name, options = list()) {
    base::message("[save_ml_model] START")
    base::on.exit(base::message("[save_ml_model] END"))
    # Input validation
    if (!base::is.character(name) ||
        base::length(name) != 1 || base::nchar(name) == 0) {
        openeocraft::api_stop(400, "Parameter 'name' must be a non-empty string")
    }

    # Validate data parameter - must be non-null to save
    if (base::is.null(data)) {
        openeocraft::api_stop(400, "Parameter 'data' must be a non-null model object")
    }

    # --- Infer MLM metadata from model attributes (set by ml_fit) ---
    # Task: inferred from the mlm_class_*/mlm_regr_* function that created the model
    inferred_task <- base::attr(data, "mlm_task")
    if (base::is.null(inferred_task)) {
        inferred_task <- "classification"
        base::message("[save_ml_model] No mlm_task attribute found; defaulting to 'classification'")
    }
    tasks <- base::c(inferred_task)

    # Architecture: inferred from the model function (e.g. Random Forest, TempCNN)
    inferred_architecture <- base::attr(data, "mlm_architecture")
    if (base::is.null(inferred_architecture)) {
        inferred_architecture <- "Unknown"
        base::message("[save_ml_model] No mlm_architecture attribute found; defaulting to 'Unknown'")
    }

    # Framework: inferred from the model function (e.g. R, PyTorch)
    inferred_framework <- base::attr(data, "mlm_framework")

    # Bands from training data
    inferred_bands <- base::attr(data, "mlm_bands")

    # Temporal extent from training data
    inferred_start_datetime <- base::attr(data, "mlm_start_datetime")
    inferred_end_datetime <- base::attr(data, "mlm_end_datetime")
    inferred_n_times <- base::attr(data, "mlm_n_times")

    # Labels from training data (for classification outputs)
    inferred_labels <- base::attr(data, "mlm_labels")

    # Bounding box from training sample locations
    inferred_bbox <- base::attr(data, "mlm_bbox")

    # Hyperparameters from ml_args
    inferred_hyperparameters <- base::attr(data, "mlm_hyperparameters")

    # Wrap operations in try-catch to return FALSE on runtime failures
    # Validation errors (api_stop) should propagate as HTTP errors
    result <- base::tryCatch(
        {
            env <- openeocraft::current_env()
            host <- openeocraft::get_host(env$api, env$req)
            job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)

            # Ensure job directory exists
            if (!base::dir.exists(job_dir)) {
                base::dir.create(job_dir, recursive = TRUE)
            }

            if (!base::dir.exists(job_dir)) {
                openeocraft::api_stop(500, "Could not create the job directory")
            }

            # Ensure models subdirectory exists
            models_dir <- base::file.path(job_dir, "models")
            if (!base::dir.exists(models_dir)) {
                base::dir.create(models_dir, recursive = TRUE)
            }

            if (!base::dir.exists(models_dir)) {
                openeocraft::api_stop(500, "Could not create the models folder")
            }

            model_filename <- base::paste0(name, ".rds")
            file <- base::file.path(models_dir, model_filename)
            base::saveRDS(data, file)

            model_href <- openeocraft::make_job_files_url(
                host = host,
                user = env$user,
                job_id = env$job$id,
                file = base::paste0("models/", model_filename)
            )

            public_models_dir <- base::file.path(
                openeocraft::api_user_workspace(env$api, "public"),
                "models"
            )
            if (!base::dir.exists(public_models_dir)) {
                base::dir.create(public_models_dir, recursive = TRUE)
            }
            if (!base::dir.exists(public_models_dir)) {
                openeocraft::api_stop(500, "Could not create public models folder")
            }
            public_model_path <- base::file.path(public_models_dir, model_filename)
            base::saveRDS(data, public_model_path)

            stac_item <- openeocraft::job_empty_collection(env$api, env$user, env$job)
            stac_item$type <- "Feature"
            stac_item$id <- name
            stac_item$stac_version <- "1.1.0"
            stac_item$stac_extensions <- list(
                "https://stac-extensions.github.io/mlm/v1.5.0/schema.json"
            )

            # --- Build geometry and bbox from inferred training data ---
            if (!base::is.null(inferred_bbox)) {
                west <- inferred_bbox[1]
                south <- inferred_bbox[2]
                east <- inferred_bbox[3]
                north <- inferred_bbox[4]
                stac_item$bbox <- list(west, south, east, north)
                stac_item$geometry <- list(
                    type = "Polygon",
                    coordinates = list(list(
                        list(west, south),
                        list(east, south),
                        list(east, north),
                        list(west, north),
                        list(west, south)
                    ))
                )
            } else {
                stac_item$bbox <- list(-180, -90, 180, 90)
                stac_item$geometry <- NULL
            }

            # --- Build MLM properties (all inferred internally) ---
            mlm_properties <- list(
                `mlm:name` = name,
                `mlm:tasks` = base::as.list(tasks),
                `mlm:architecture` = inferred_architecture
            )

            # Temporal extent
            if (!base::is.null(inferred_start_datetime)) {
                mlm_properties$datetime <- NULL
                mlm_properties$start_datetime <- inferred_start_datetime
                mlm_properties$end_datetime <- inferred_end_datetime
            } else {
                mlm_properties$datetime <- base::format(
                    base::Sys.time(), "%Y-%m-%dT%H:%M:%SZ"
                )
            }

            # Framework
            if (!base::is.null(inferred_framework)) {
                mlm_properties$`mlm:framework` <- inferred_framework
            }

            # Hyperparameters
            if (!base::is.null(inferred_hyperparameters)) {
                mlm_properties$`mlm:hyperparameters` <- inferred_hyperparameters
            }

            # Build mlm:input from inferred bands and timeline
            n_bands <- if (!base::is.null(inferred_bands)) {
                base::length(inferred_bands)
            } else {
                -1L
            }
            n_times <- if (!base::is.null(inferred_n_times)) {
                inferred_n_times
            } else {
                -1L
            }

            mlm_input <- list(list(
                name = "input",
                bands = if (!base::is.null(inferred_bands)) {
                    base::as.list(inferred_bands)
                } else {
                    list()
                },
                input = list(
                    shape = list(-1L, n_bands, n_times),
                    dim_order = list("batch", "bands", "time"),
                    data_type = "float32"
                )
            ))
            mlm_properties$`mlm:input` <- mlm_input

            # Build mlm:output from inferred labels/task
            n_classes <- if (!base::is.null(inferred_labels)) {
                base::length(inferred_labels)
            } else {
                -1L
            }

            mlm_output <- list(list(
                name = inferred_task,
                tasks = base::as.list(tasks),
                result = list(
                    shape = list(-1L, n_classes),
                    dim_order = list("batch", "class"),
                    data_type = "float32"
                ),
                `classification:classes` = if (!base::is.null(inferred_labels)) {
                    base::lapply(
                        base::seq_along(inferred_labels),
                        function(i) {
                            list(
                                value = i,
                                name = inferred_labels[i]
                            )
                        }
                    )
                } else {
                    list()
                }
            ))
            mlm_properties$`mlm:output` <- mlm_output

            # Allow options to override any inferred property
            for (key in base::names(options)) {
                mlm_key <- key
                if (!base::startsWith(key, "mlm:") &&
                    key %in% base::c(
                        "framework", "framework_version", "memory_size",
                        "total_parameters", "pretrained", "pretrained_source",
                        "batch_size_suggestion", "accelerator",
                        "accelerator_constrained", "accelerator_summary",
                        "accelerator_count"
                    )) {
                    mlm_key <- base::paste0("mlm:", key)
                }
                mlm_properties[[mlm_key]] <- options[[key]]
            }

            stac_item$properties <- mlm_properties

            # --- Build model asset ---
            artifact_type <- NULL
            if ("mlm:artifact_type" %in% base::names(options)) {
                artifact_type <- options$`mlm:artifact_type`
            } else if ("artifact_type" %in% base::names(options)) {
                artifact_type <- options$artifact_type
            }
            if (base::is.null(artifact_type)) {
                artifact_type <- "application/octet-stream"
            }

            model_asset <- list(
                href = model_href,
                type = "application/octet-stream",
                roles = list("mlm:model"),
                `mlm:artifact_type` = artifact_type
            )

            # Handle compile_method override
            compile_method <- NULL
            if ("mlm:compile_method" %in% base::names(options)) {
                compile_method <- options$`mlm:compile_method`
            } else if ("compile_method" %in% base::names(options)) {
                compile_method <- options$compile_method
            }
            if (!base::is.null(compile_method)) {
                model_asset$`mlm:compile_method` <- compile_method
            }

            assets <- list()
            assets[[model_filename]] <- model_asset

            if (!base::is.null(options$code_asset)) {
                code_asset_info <- options$code_asset

                # Validate required code asset fields
                if (base::is.null(code_asset_info$href) ||
                    base::nchar(code_asset_info$href) == 0) {
                    openeocraft::api_stop(400, "Code asset must have a valid 'href' field")
                }

                if (base::is.null(code_asset_info$type) ||
                    base::nchar(code_asset_info$type) == 0) {
                    openeocraft::api_stop(400, "Code asset must have a valid 'type' field")
                }

                if (base::is.null(code_asset_info$name) ||
                    base::nchar(code_asset_info$name) == 0) {
                    openeocraft::api_stop(400, "Code asset must have a valid 'name' field")
                }

                code_asset <- list(
                    href = code_asset_info$href,
                    type = code_asset_info$type,
                    roles = list("code")
                )

                if (!base::is.null(code_asset_info$entrypoint)) {
                    code_asset$`mlm:entrypoint` <- code_asset_info$entrypoint
                }

                assets[[code_asset_info$name]] <- code_asset
            }

            # Optional tuning trace from ml_tune_* (same job); not part of MLM spec.
            tuning_results_fn <- "tuning_results.json"
            tuning_src <- base::file.path(job_dir, tuning_results_fn)
            if (base::file.exists(tuning_src)) {
                tuning_public <- base::file.path(public_models_dir, tuning_results_fn)
                base::file.copy(tuning_src, tuning_public, overwrite = TRUE)
                tuning_href <- openeocraft::make_job_files_url(
                    host = host,
                    user = env$user,
                    job_id = env$job$id,
                    file = tuning_results_fn
                )
                assets[[tuning_results_fn]] <- list(
                    href = tuning_href,
                    type = "application/json",
                    roles = list("data"),
                    title = "Hyperparameter tuning results (all runs and scores)"
                )
            }

            stac_item$assets <- assets

            # Create item filename based on model name
            item_filename <- base::paste0(name, ".json")

            # Add link to parent collection
            stac_item$links <- list(
                list(href = "collection.json", rel = "collection"),
                list(href = item_filename, rel = "self")
            )

            # Save STAC Item (job-local, includes tokenized href)
            item_json_path <- base::file.path(job_dir, item_filename)
            jsonlite::write_json(
                x = stac_item,
                path = item_json_path,
                auto_unbox = TRUE
            )

            # Save public STAC Item (token-free, relative model href)
            public_stac_item <- stac_item
            public_stac_item$assets[[model_filename]]$href <- model_filename
            if (tuning_results_fn %in% base::names(public_stac_item$assets)) {
                public_stac_item$assets[[tuning_results_fn]]$href <- tuning_results_fn
            }
            public_item_json_path <- base::file.path(public_models_dir, item_filename)
            jsonlite::write_json(
                x = public_stac_item,
                path = public_item_json_path,
                auto_unbox = TRUE
            )

            # Create or update parent collection
            collection_json_path <- base::file.path(job_dir, "collection.json")

            # Use inferred bbox/temporal for collection extent
            coll_bbox <- if (!base::is.null(inferred_bbox)) {
                list(inferred_bbox)
            } else {
                list(list(-180, -90, 180, 90))
            }
            coll_interval <- if (!base::is.null(inferred_start_datetime)) {
                list(list(inferred_start_datetime, inferred_end_datetime))
            } else {
                list(list("1900-01-01T00:00:00Z", "9999-12-31T23:59:59Z"))
            }

            if (base::file.exists(collection_json_path)) {
                # Update existing collection
                collection <- jsonlite::read_json(
                    collection_json_path,
                    simplifyVector = FALSE
                )

                # Add item link if not already present
                item_link <- list(href = item_filename, rel = "item")

                link_exists <- base::any(base::sapply(
                    collection$links,
                    function(link) {
                        !base::is.null(link$href) && link$href == item_filename
                    }
                ))

                if (!link_exists) {
                    collection$links <- c(collection$links, list(item_link))
                }
            } else {
                # Create new parent collection
                collection <- list(
                    stac_version = "1.0.0",
                    stac_extensions = list(
                        "https://stac-extensions.github.io/item-assets/v1.0.0/schema.json"
                    ),
                    type = "Collection",
                    id = "ml-models",
                    title = "Machine Learning Models",
                    description = "Collection of machine learning models saved in this job.",
                    license = "Apache-2.0",
                    extent = list(
                        spatial = list(bbox = coll_bbox),
                        temporal = list(interval = coll_interval)
                    ),
                    item_assets = list(weights = list(
                        title = "model weights",
                        roles = list("mlm:model", "mlm:weights")
                    )),
                    summaries = list(
                        datetime = list(
                            minimum = if (!base::is.null(inferred_start_datetime)) {
                                inferred_start_datetime
                            } else {
                                "1900-01-01T00:00:00Z"
                            },
                            maximum = if (!base::is.null(inferred_end_datetime)) {
                                inferred_end_datetime
                            } else {
                                "9999-12-31T23:59:59Z"
                            }
                        )
                    ),
                    links = list(
                        list(href = "collection.json", rel = "self"),
                        list(href = item_filename, rel = "item")
                    )
                )
            }

            jsonlite::write_json(
                x = collection,
                path = collection_json_path,
                auto_unbox = TRUE
            )

            public_collection_json_path <- base::file.path(
                public_models_dir, "collection.json"
            )
            jsonlite::write_json(
                x = collection,
                path = public_collection_json_path,
                auto_unbox = TRUE
            )

            return(TRUE)
        },
        error = function(e) {
            # Re-throw api_stop errors (validation errors) as HTTP errors
            if (!base::is.null(e$status)) {
                stop(e)
            }
            # Return FALSE on runtime errors (file I/O, JSON writing, etc.)
            return(FALSE)
        }
    )

    return(result)
}

# Load ML model from STAC Item (URL or path) with mlm extension; resolve model asset href.
#
# Args:
#   uri: HTTPS URL or path relative to workspace/job; model_asset: optional asset key;
#   input_index, output_index: stored as attributes on returned model.
#
# Returns:
#   RDS model with attrs stac_item, input_index, output_index.
#
# Raises:
#   openeocraft::api_stop for missing files, invalid STAC, download failures.
#
#* @openeo-process
load_stac_ml <- function(uri,
                         model_asset = NULL,
                         input_index = 0,
                         output_index = 0) {
    base::message("[load_stac_ml] START")
    base::on.exit(base::message("[load_stac_ml] END"))
    base::tryCatch(
        {
            # Initialize environment
            env <- openeocraft::current_env()

            # Determine if uri is a URL or file path
            is_url <- base::grepl("^https?://", uri, perl = TRUE)

            # Load STAC Item
            stac_item <- NULL
            if (is_url) {
                # Load external STAC Item from URL
                stac_item <- base::tryCatch(
                    {
                        rstac::read_stac(uri)
                    },
                    error = function(e) {
                        openeocraft::api_stop(
                            404,
                            base::paste(
                                "Failed to load STAC Item from URL:",
                                uri,
                                "-",
                                e$message
                            )
                        )
                    }
                )
            } else {
                file_path <- NULL

                # Try workspace directory first
                workspace_dir <- openeocraft::api_user_workspace(env$api, env$user)
                workspace_root_dir <- base::file.path(workspace_dir, "root")
                workspace_path <- base::file.path(workspace_root_dir, uri)

                if (base::file.exists(workspace_path)) {
                    file_path <- workspace_path
                } else {
                    # Try current job directory
                    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
                    job_path <- base::file.path(job_dir, uri)

                    if (base::file.exists(job_path)) {
                        file_path <- job_path
                    } else {
                        # Search through all job directories (for STAC Items from previous jobs)
                        jobs_dir <- base::file.path(workspace_dir, "jobs")
                        if (base::dir.exists(jobs_dir)) {
                            job_dirs <- base::list.dirs(jobs_dir,
                                full.names = TRUE,
                                recursive = FALSE
                            )
                            for (other_job_dir in job_dirs) {
                                other_job_path <- base::file.path(other_job_dir, uri)
                                if (base::file.exists(other_job_path)) {
                                    file_path <- other_job_path
                                    break
                                }
                            }
                        }
                    }
                }

                if (base::is.null(file_path)) {
                    openeocraft::api_stop(
                        404,
                        base::paste("STAC Item file not found:", uri)
                    )
                }

                stac_item <- jsonlite::read_json(file_path, simplifyVector = FALSE)
            }

            if (base::is.null(stac_item)) {
                openeocraft::api_stop(400, "Failed to load STAC Item")
            }

            # Validate that STAC Item implements mlm extension
            mlm_extension <- "https://stac-extensions.github.io/mlm/v1.5.0/schema.json"
            if (base::is.null(stac_item$stac_extensions) ||
                !mlm_extension %in% stac_item$stac_extensions) {
                openeocraft::api_stop(400, "STAC Item must implement the mlm extension")
            }

            if (base::is.null(stac_item$assets)) {
                openeocraft::api_stop(400, "STAC Item does not contain any assets")
            }

            # Find model asset
            model_asset_obj <- NULL
            if (base::is.null(model_asset)) {
                mlm_assets <- base::Filter(function(asset) {
                    !base::is.null(asset$roles) &&
                        "mlm:model" %in% asset$roles
                }, stac_item$assets)

                if (base::length(mlm_assets) == 0) {
                    openeocraft::api_stop(
                        400,
                        "No asset with role 'mlm:model' found in STAC Item"
                    )
                } else if (base::length(mlm_assets) > 1) {
                    openeocraft::api_stop(
                        400,
                        base::paste(
                            "Multiple assets with role 'mlm:model' found.",
                            "Please specify 'model_asset' parameter."
                        )
                    )
                }

                model_asset_obj <- mlm_assets[[1]]
            } else {
                if (!model_asset %in% base::names(stac_item$assets)) {
                    openeocraft::api_stop(
                        400,
                        base::paste("Asset not found:", model_asset)
                    )
                }
                model_asset_obj <- stac_item$assets[[model_asset]]

                # Validate that specified asset has mlm:model role
                if (base::is.null(model_asset_obj$roles) ||
                    !"mlm:model" %in% model_asset_obj$roles) {
                    openeocraft::api_stop(
                        400,
                        base::paste(
                            "Specified asset '",
                            model_asset,
                            "' does not have role 'mlm:model'"
                        )
                    )
                }
            }

            if (base::is.null(model_asset_obj$href)) {
                openeocraft::api_stop(400, "Model asset does not contain 'href' field")
            }

            # Download or load the model from href
            model_href <- model_asset_obj$href

            # If STAC Item was loaded from URL and model_href is relative, resolve it
            if (is_url &&
                !base::grepl("^https?://", model_href, perl = TRUE)) {
                # Resolve relative URL against STAC Item base URL
                base_url <- base::sub("/[^/]*$", "", uri)
                if (!base::endsWith(base_url, "/")) {
                    base_url <- base::paste0(base_url, "/")
                }
                # Remove leading slash from model_href if present
                model_href <- base::sub("^/", "", model_href)
                model_href <- base::paste0(base_url, model_href)
            }

            model <- NULL

            if (base::grepl("^https?://", model_href, perl = TRUE)) {
                # Download external model file
                temp_file <- base::tempfile(fileext = ".rds")
                base::tryCatch(
                    {
                        utils::download.file(model_href,
                            temp_file,
                            mode = "wb",
                            quiet = TRUE
                        )
                    },
                    error = function(e) {
                        openeocraft::api_stop(
                            404,
                            base::paste(
                                "Failed to download model from:",
                                model_href,
                                "-",
                                e$message
                            )
                        )
                    }
                )
                model <- base::readRDS(temp_file)
                base::unlink(temp_file)
            } else {
                model_path <- NULL

                # Try current job directory first (for models saved with save_ml_model)
                job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
                job_model_path <- base::file.path(job_dir, model_href)

                if (base::file.exists(job_model_path)) {
                    model_path <- job_model_path
                } else {
                    # Search through all job directories (for models from previous jobs)
                    workspace_dir <- openeocraft::api_user_workspace(env$api, env$user)
                    jobs_dir <- base::file.path(workspace_dir, "jobs")

                    if (base::dir.exists(jobs_dir)) {
                        job_dirs <- base::list.dirs(jobs_dir,
                            full.names = TRUE,
                            recursive = FALSE
                        )
                        for (other_job_dir in job_dirs) {
                            other_job_model_path <- base::file.path(other_job_dir, model_href)
                            if (base::file.exists(other_job_model_path)) {
                                model_path <- other_job_model_path
                                break
                            }
                        }
                    }

                    # Try workspace directory
                    if (base::is.null(model_path)) {
                        workspace_root_dir <- base::file.path(workspace_dir, "root")
                        workspace_model_path <- base::file.path(workspace_root_dir, model_href)

                        if (base::file.exists(workspace_model_path)) {
                            model_path <- workspace_model_path
                        }
                    }
                }

                if (base::is.null(model_path)) {
                    openeocraft::api_stop(
                        404,
                        base::paste("Model file not found:", model_href)
                    )
                }

                model <- base::readRDS(model_path)
            }

            # Validate model format
            if (!base::is.list(model) &&
                !base::is.environment(model)) {
                openeocraft::api_stop(400, "Invalid model format")
            }

            # Attach metadata from STAC Item
            base::attr(model, "stac_item") <- stac_item
            base::attr(model, "input_index") <- input_index
            base::attr(model, "output_index") <- output_index

            model
        },
        error = function(e) {
            if (base::inherits(e, "openeocraft_error")) {
                stop(e)
            }
            openeocraft::api_stop(
                500,
                base::paste("Error loading STAC ML model:", e$message)
            )
        }
    )
}

# Resolve model id to an RDS file under job or workspace models/ directories.
#
# Args:
#   id: Model name or path-like id matching ^[\\w\\-\\.~/]+$.
#
# Returns:
#   Loaded model from readRDS.
#
# Raises:
#   openeocraft::api_stop(400/404) for invalid id or not found.
#
#* @openeo-process
load_ml_model <- function(id) {
    base::message("[load_ml_model] START")
    base::on.exit(base::message("[load_ml_model] END"))
    # Validate id parameter against pattern
    if (!base::grepl("^[\\w\\-\\.~/]+$", id, perl = TRUE)) {
        openeocraft::api_stop(
            400,
            "Invalid model ID. Must match pattern: ^[\\w\\-\\.~/]+$"
        )
    }

    # Initialize environment
    env <- openeocraft::current_env()

    # Build model filename (id corresponds to name used in save_ml_model)
    model_filename <- base::paste0(id, ".rds")

    # Try to find model in multiple locations
    model_file <- NULL

    # 1. Try current job directory (for models saved with save_ml_model in same job)
    job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
    job_model_file <- base::file.path(job_dir, "models", model_filename)

    if (base::file.exists(job_model_file)) {
        model_file <- job_model_file
    } else {
        # 2. Search through all job directories (for models saved in previous jobs)
        workspace_dir <- openeocraft::api_user_workspace(env$api, env$user)
        jobs_dir <- base::file.path(workspace_dir, "jobs")

        if (base::dir.exists(jobs_dir)) {
            # Get list of all job directories
            job_dirs <- base::list.dirs(jobs_dir,
                full.names = TRUE,
                recursive = FALSE
            )

            for (other_job_dir in job_dirs) {
                other_job_model_file <- base::file.path(other_job_dir, "models", model_filename)
                if (base::file.exists(other_job_model_file)) {
                    model_file <- other_job_model_file
                    break
                }
            }
        }

        # 3. Try workspace directory (for models imported or saved across jobs)
        if (base::is.null(model_file)) {
            workspace_root_dir <- base::file.path(workspace_dir, "root")
            workspace_model_file <- base::file.path(workspace_root_dir, "models", model_filename)

            if (base::file.exists(workspace_model_file)) {
                model_file <- workspace_model_file
            }
        }

        # 4. Handle path-based ids (e.g., "folder/model")
        if (base::is.null(model_file) && base::grepl("/", id)) {
            # Try as relative path in current job's models directory
            path_model_file <- base::file.path(job_dir, "models", base::paste0(id, ".rds"))
            if (base::file.exists(path_model_file)) {
                model_file <- path_model_file
            } else {
                # Try in workspace directory
                workspace_root_dir <- base::file.path(workspace_dir, "root")
                path_model_file <- base::file.path(
                    workspace_root_dir,
                    "models",
                    base::paste0(id, ".rds")
                )
                if (base::file.exists(path_model_file)) {
                    model_file <- path_model_file
                } else {
                    # Try in all job directories
                    if (base::dir.exists(jobs_dir)) {
                        job_dirs <- base::list.dirs(jobs_dir,
                            full.names = TRUE,
                            recursive = FALSE
                        )
                        for (other_job_dir in job_dirs) {
                            path_model_file <- base::file.path(
                                other_job_dir,
                                "models",
                                base::paste0(id, ".rds")
                            )
                            if (base::file.exists(path_model_file)) {
                                model_file <- path_model_file
                                break
                            }
                        }
                    }
                }
            }
        }
    }

    # Check if model file was found
    if (base::is.null(model_file)) {
        openeocraft::api_stop(404, base::paste0("Model '", id, "' not found"))
    }

    # Load the model
    model <- base::readRDS(model_file)
    model
}
