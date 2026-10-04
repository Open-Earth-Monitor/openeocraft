#' Machine learning runtimes (L3-ML API profile)
#'
#' Helpers to declare the machine learning (ML) runtimes a back-end offers.
#' The declaration is served by `GET /ml_runtimes`, an endpoint modelled on
#' `GET /udf_runtimes` of the openEO API and proposed by the L3-ML API
#' profile (requirements ML1 to ML8). It lets a client, or a federation
#' broker, check before submitting a job whether the back-end can train the
#' requested model type, load a given model format, and run a given workflow
#' type.
#'
#' \itemize{
#'
#' \item `new_ml_runtime_version()`: Describes one version of a runtime:
#'   training and inference support (ML4), the `mlm:artifact_type` values
#'   it can load and save (ML5), the workflow types (ML6), the training data
#'   formats (ML7) and the accelerators (ML8).
#'
#' \item `new_ml_runtime()`: Describes one runtime with its `versions` and
#'   `default` version (ML3).
#'
#' \item `set_ml_runtimes()`: Validates the runtimes and registers them on
#'   the API object. Names must be STAC MLM `mlm:framework` values, the same
#'   values the back-end writes into the metadata of the models it saves.
#'
#' \item `get_ml_runtimes()`: Returns the registered runtimes (an empty named
#'   list when none were registered).
#'
#' \item `sits_ml_runtimes()`: Detects the runtimes of the bundled `sits`
#'   processes (`inst/ml/processes.R`) from the installed packages.
#'
#' }
#'
#' @param api An openeocraft API object.
#'
#' @param runtimes A named list of runtimes created by `new_ml_runtime()`.
#'   Names are `mlm:framework` values.
#'
#' @param title Short human-readable title of the runtime.
#'
#' @param description Optional longer description (CommonMark allowed).
#'
#' @param versions A named list of versions created by
#'   `new_ml_runtime_version()`. Names are version numbers.
#'
#' @param default The default version. Must be one of `names(versions)`.
#'   Defaults to the first version.
#'
#' @param links Optional list of links, each a list with `rel` and `href`.
#'
#' @param training,inference Logical. Whether the version supports training
#'   and inference.
#'
#' @param load_artifact_types,save_artifact_types Character vectors of
#'   `mlm:artifact_type` values the version can load and save.
#'
#' @param workflow_types Character vector with at least one of `"feature"`,
#'   `"time_series"` and `"spatial_patch"`.
#'
#' @param training_data_formats Character vector of accepted training data
#'   formats. When `training` is `TRUE`, it must contain `"vector_cube"`.
#'   `NULL` (default) means `"vector_cube"` when training and none
#'   otherwise.
#'
#' @param accelerators Character vector of STAC MLM `mlm:accelerator` values.
#'
#' @param architectures Optional character vector of model architectures
#'   (`mlm:architecture` values) available in this version.
#'
#' @param libraries Optional named list of libraries, each a list with a
#'   `version` field, as in `GET /udf_runtimes`.
#'
#' @return `new_ml_runtime_version()` and `new_ml_runtime()` return a list
#'   ready to be serialized. `set_ml_runtimes()` returns the `api` object
#'   invisibly. `get_ml_runtimes()` and `sits_ml_runtimes()` return a named
#'   list of runtimes.
#'
#' @references
#' STAC Machine Learning Model (MLM) extension:
#' \url{https://github.com/stac-extensions/mlm}
#'
#' openEO profiles:
#' \url{https://openeo.org/documentation/1.0/developers/profiles/index.html}
#'
#' @name ml_runtimes
#'
#' @examples
#' api <- create_openeo_v1(
#'     id = "demo", title = "Demo", description = "Demo",
#'     backend_version = "0.4.1", stac_api = NULL,
#'     work_dir = tempdir(), production = FALSE
#' )
#' runtime <- new_ml_runtime(
#'     title = "Example framework",
#'     versions = list("1.0.0" = new_ml_runtime_version(
#'         load_artifact_types = "saveRDS",
#'         save_artifact_types = "saveRDS",
#'         workflow_types = "feature",
#'         accelerators = "amd64"
#'     ))
#' )
#' set_ml_runtimes(api, list("Example" = runtime))
#' names(get_ml_runtimes(api))
NULL

# Controlled vocabularies of the L3-ML API profile and of STAC MLM.
.ml_workflow_types <- c("feature", "time_series", "spatial_patch")
.ml_accelerators <- c(
    "amd64", "cuda", "xla", "amd-rocm", "intel-ipex-cpu", "intel-ipex-gpu",
    "macos-arm"
)
.ml_vector_cube_format <- "vector_cube"
# Same pattern as the free-form branch of `mlm:framework` in STAC MLM.
.ml_framework_pattern <- "^(?=[^\\s._\\-]).*[^\\s._\\-]$"

#' @rdname ml_runtimes
#' @export
new_ml_runtime_version <- function(training = TRUE,
                                   inference = TRUE,
                                   load_artifact_types = character(),
                                   save_artifact_types = character(),
                                   workflow_types = "feature",
                                   training_data_formats = NULL,
                                   accelerators = character(),
                                   architectures = character(),
                                   libraries = list()) {
    if (is.null(training_data_formats)) {
        training_data_formats <- if (isTRUE(training)) {
            .ml_vector_cube_format
        } else {
            character()
        }
    }
    version <- list(
        training = training,
        inference = inference,
        artifact_types = list(
            load = as.list(load_artifact_types),
            save = as.list(save_artifact_types)
        ),
        workflow_types = as.list(workflow_types),
        training_data_formats = as.list(training_data_formats),
        accelerators = as.list(accelerators)
    )
    if (length(architectures)) {
        version$architectures <- as.list(architectures)
    }
    if (length(libraries)) {
        version$libraries <- libraries
    }
    check_ml_runtime_version(version)
    version
}

#' @rdname ml_runtimes
#' @export
new_ml_runtime <- function(title,
                           versions,
                           default = names(versions)[[1]],
                           description = NULL,
                           links = list()) {
    runtime <- list(
        title = title,
        description = description,
        default = default,
        versions = versions,
        links = links
    )
    runtime <- runtime[!vapply(runtime, is.null, logical(1))]
    check_ml_runtime(runtime)
    runtime
}

#' @rdname ml_runtimes
#' @export
set_ml_runtimes <- function(api, runtimes) {
    if (!is.list(runtimes)) {
        stop("`runtimes` must be a named list of ML runtimes", call. = FALSE)
    }
    if (length(runtimes)) {
        frameworks <- names(runtimes)
        if (is.null(frameworks) || anyNA(frameworks) ||
            anyDuplicated(frameworks) ||
            !all(grepl(.ml_framework_pattern, frameworks, perl = TRUE))) {
            stop(
                "`runtimes` names must be unique `mlm:framework` values ",
                "without leading or trailing spaces, dots, dashes or ",
                "underscores",
                call. = FALSE
            )
        }
        for (framework in frameworks) {
            tryCatch(
                check_ml_runtime(runtimes[[framework]]),
                error = function(e) {
                    stop(
                        "Invalid ML runtime '", framework, "': ",
                        conditionMessage(e),
                        call. = FALSE
                    )
                }
            )
        }
    }
    api_attr(api, "ml_runtimes") <- runtimes
    invisible(api)
}

#' @rdname ml_runtimes
#' @export
get_ml_runtimes <- function(api) {
    runtimes <- api_attr(api, "ml_runtimes")
    if (!length(runtimes)) {
        # Empty named list serializes as `{}` rather than `[]`.
        return(structure(list(), names = character()))
    }
    runtimes
}

#' @rdname ml_runtimes
#' @export
sits_ml_runtimes <- function() {
    runtimes <- structure(list(), names = character())
    if (!ml_pkg_available("sits")) {
        return(runtimes)
    }
    cpu <- ml_cpu_accelerator()
    # Every runtime here reads and writes models with base::saveRDS(), and
    # trains on sits samples that may come in as an RDS file or URL.
    artifact_types <- "saveRDS"
    training_data_formats <- c(.ml_vector_cube_format, "sits_tibble_rds")

    caret_models <- c(
        "Random Forest" = "randomForest",
        "SVM" = "e1071",
        "XGBoost" = "xgboost"
    )
    caret_models <- caret_models[vapply(
        caret_models, ml_pkg_available, logical(1)
    )]
    if (ml_pkg_available("caret") && length(caret_models)) {
        version <- ml_pkg_version("caret")
        runtimes[["R CARET"]] <- new_ml_runtime(
            title = "caret (R)",
            description = paste(
                "Classical machine learning models trained through sits.",
                "Models work on per-pixel feature vectors."
            ),
            versions = stats::setNames(list(new_ml_runtime_version(
                training = TRUE,
                inference = TRUE,
                load_artifact_types = artifact_types,
                save_artifact_types = artifact_types,
                workflow_types = "feature",
                training_data_formats = training_data_formats,
                accelerators = cpu,
                architectures = names(caret_models),
                libraries = ml_pkg_libraries(c(
                    "sits", "caret", unname(caret_models)
                ))
            )), version),
            links = list(new_link(
                rel = "about",
                href = "https://topepo.github.io/caret/",
                type = "text/html",
                title = "caret documentation"
            ))
        )
    }

    if (ml_pkg_available("torch")) {
        version <- ml_pkg_version("torch")
        runtimes[["Torch for R"]] <- new_ml_runtime(
            title = "torch (R)",
            description = paste(
                "Deep learning models trained through sits on torch for R.",
                "MLP works on feature vectors; TempCNN, TAE and LightTAE",
                "work on time series."
            ),
            versions = stats::setNames(list(new_ml_runtime_version(
                training = TRUE,
                inference = TRUE,
                load_artifact_types = artifact_types,
                save_artifact_types = artifact_types,
                workflow_types = c("feature", "time_series"),
                training_data_formats = training_data_formats,
                accelerators = unique(c(cpu, ml_torch_accelerators())),
                architectures = c("MLP", "TempCNN", "TAE", "LightTAE"),
                libraries = ml_pkg_libraries(c("sits", "torch", "luz"))
            )), version),
            links = list(new_link(
                rel = "about",
                href = "https://torch.mlverse.org/",
                type = "text/html",
                title = "torch for R documentation"
            ))
        )
    }
    runtimes
}

#' @keywords internal
check_ml_runtime <- function(runtime) {
    if (!is.list(runtime)) {
        stop("runtime must be a list", call. = FALSE)
    }
    if (!is_string(runtime$title)) {
        stop("`title` must be a non-empty string", call. = FALSE)
    }
    if (!is.null(runtime$description) && !is_string(runtime$description)) {
        stop("`description` must be a non-empty string", call. = FALSE)
    }
    versions <- runtime$versions
    if (!is.list(versions) || !length(versions) || is.null(names(versions)) ||
        any(!nzchar(names(versions))) || anyDuplicated(names(versions))) {
        stop(
            "`versions` must be a non-empty list named by unique versions",
            call. = FALSE
        )
    }
    if (!is_string(runtime$default) ||
        !runtime$default %in% names(versions)) {
        stop("`default` must be one of the `versions`", call. = FALSE)
    }
    for (v in names(versions)) {
        tryCatch(
            check_ml_runtime_version(versions[[v]]),
            error = function(e) {
                stop(
                    "version '", v, "': ", conditionMessage(e),
                    call. = FALSE
                )
            }
        )
    }
    if (!is.null(runtime$links) && !is.list(runtime$links)) {
        stop("`links` must be a list", call. = FALSE)
    }
    invisible(TRUE)
}

#' @keywords internal
check_ml_runtime_version <- function(version) {
    if (!is.list(version)) {
        stop("version must be a list", call. = FALSE)
    }
    for (field in c("training", "inference")) {
        if (!is.logical(version[[field]]) || length(version[[field]]) != 1L ||
            is.na(version[[field]])) {
            stop("`", field, "` must be TRUE or FALSE", call. = FALSE)
        }
    }
    if (!version$training && !version$inference) {
        stop("a version must support training, inference or both",
            call. = FALSE
        )
    }
    load <- unlist(version$artifact_types$load)
    save <- unlist(version$artifact_types$save)
    if (!is.list(version$artifact_types) ||
        !all(vapply(c(load, save), is_string, logical(1)))) {
        stop("`artifact_types` must hold strings in `load` and `save`",
            call. = FALSE
        )
    }
    if (version$inference && !length(load)) {
        stop("inference requires at least one loadable artifact type",
            call. = FALSE
        )
    }
    if (version$training && !length(save)) {
        stop("training requires at least one savable artifact type",
            call. = FALSE
        )
    }
    workflow_types <- unlist(version$workflow_types)
    if (!length(workflow_types) ||
        !all(workflow_types %in% .ml_workflow_types)) {
        stop(
            "`workflow_types` must be one or more of: ",
            paste(.ml_workflow_types, collapse = ", "),
            call. = FALSE
        )
    }
    formats <- unlist(version$training_data_formats)
    if (version$training && !.ml_vector_cube_format %in% formats) {
        stop(
            "`training_data_formats` must include '", .ml_vector_cube_format,
            "' when training is supported",
            call. = FALSE
        )
    }
    if (!version$training && length(formats)) {
        stop(
            "`training_data_formats` must be empty when training is ",
            "not supported",
            call. = FALSE
        )
    }
    accelerators <- unlist(version$accelerators)
    if (!all(accelerators %in% .ml_accelerators)) {
        stop(
            "`accelerators` must be STAC MLM `mlm:accelerator` values: ",
            paste(.ml_accelerators, collapse = ", "),
            call. = FALSE
        )
    }
    invisible(TRUE)
}

is_string <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

ml_pkg_available <- function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
}

ml_pkg_version <- function(pkg) {
    as.character(utils::packageVersion(pkg))
}

ml_pkg_libraries <- function(pkgs) {
    pkgs <- unique(pkgs[vapply(pkgs, ml_pkg_available, logical(1))])
    stats::setNames(
        lapply(pkgs, function(pkg) list(version = ml_pkg_version(pkg))),
        pkgs
    )
}

# STAC MLM has no value for generic ARM CPUs other than Apple silicon, so
# other architectures report no CPU accelerator.
ml_cpu_accelerator <- function(arch = R.version$arch,
                               os = R.version$os) {
    if (arch %in% c("x86_64", "amd64")) {
        return("amd64")
    }
    if (arch %in% c("aarch64", "arm64") && grepl("^darwin", os)) {
        return("macos-arm")
    }
    character()
}

# Probing CUDA needs the Lantern binary, which may be missing or broken.
# torch is resolved at run time because it is not a declared dependency.
ml_torch_accelerators <- function() {
    cuda <- tryCatch(
        {
            torch_ns <- asNamespace("torch")
            isTRUE(torch_ns$torch_is_installed()) &&
                isTRUE(torch_ns$cuda_is_available())
        },
        error = function(e) FALSE
    )
    if (cuda) "cuda" else character()
}
