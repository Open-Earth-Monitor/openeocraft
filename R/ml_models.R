#' Machine learning models stored on the back-end (L3-ML API profile)
#'
#' Helpers behind `GET /ml_models` and `GET /ml_models/{model_id}`
#' (requirements ML10 and ML11 of the proposed L3-ML API profile). They list
#' the models saved by `save_ml_model` together with their STAC MLM Items.
#' The listed `id` is the identifier that `load_ml_model` accepts.
#'
#' Models are found in this order, and the first model with a given id wins:
#'
#' \enumerate{
#'
#' \item When `user` is given, the user's job folders
#'   (`<work_dir>/workspace/<user>/jobs/<job_id>/models/<id>.rds` with the
#'   Item at `<job_id>/<id>.json`), in alphabetical order of job id.
#'
#' \item When `user` is given, the user's workspace
#'   (`<work_dir>/workspace/<user>/root/models/<id>.rds` with the Item at
#'   `<id>.json` in the same folder).
#'
#' \item The public model folder
#'   (`<work_dir>/workspace/public/models/<id>.rds` and `<id>.json`).
#'
#' }
#'
#' This is the order that `load_ml_model` searches, so the Item returned for
#' an id describes the model that `load_ml_model` loads for the same user.
#' Only models whose Item declares the STAC MLM extension are listed.
#'
#' @param api An openeocraft API object.
#'
#' @param user Authenticated user, or `NULL` to list only public models.
#'
#' @param model_id Model identifier.
#'
#' @return `ml_models_list()` returns a named list of model records (`id`,
#'   `scope`, `item`, `model_file`). `ml_model_get()` returns one record, or
#'   `NULL` when the model does not exist.
#'
#' @name ml_models
#' @keywords internal
NULL

# Identifier pattern of load_ml_model, minus `/` (ids are single path
# segments on this endpoint).
.ml_model_id_pattern <- "^[\\w\\-\\.~]+$"
.ml_public_user <- "public"

#' @rdname ml_models
ml_models_list <- function(api, user = NULL) {
    sources <- list()
    if (!is.null(user)) {
        workspace_dir <- file.path(api_workdir(api), "workspace", user)
        jobs_dir <- file.path(workspace_dir, "jobs")
        job_dirs <- if (dir.exists(jobs_dir)) {
            sort(list.dirs(jobs_dir, full.names = TRUE, recursive = FALSE))
        } else {
            character()
        }
        for (job_dir in job_dirs) {
            sources <- c(sources, list(list(
                scope = "user",
                model_dir = file.path(job_dir, "models"),
                item_dir = job_dir
            )))
        }
        root_models <- file.path(workspace_dir, "root", "models")
        sources <- c(sources, list(list(
            scope = "user",
            model_dir = root_models,
            item_dir = root_models
        )))
    }
    public_models <- file.path(
        api_workdir(api), "workspace", .ml_public_user, "models"
    )
    sources <- c(sources, list(list(
        scope = "public",
        model_dir = public_models,
        item_dir = public_models
    )))

    models <- list()
    for (source in sources) {
        for (record in ml_models_scan_dir(source)) {
            if (!record$id %in% names(models)) {
                models[[record$id]] <- record
            }
        }
    }
    models
}

#' @rdname ml_models
ml_model_get <- function(api, user = NULL, model_id) {
    ml_models_list(api, user)[[model_id]]
}

# Read the models of one folder that have a valid STAC MLM Item.
ml_models_scan_dir <- function(source) {
    if (!dir.exists(source$model_dir)) {
        return(list())
    }
    model_files <- sort(list.files(
        source$model_dir,
        pattern = "\\.rds$",
        full.names = TRUE
    ))
    records <- lapply(model_files, function(model_file) {
        id <- sub("\\.rds$", "", basename(model_file))
        if (!grepl(.ml_model_id_pattern, id, perl = TRUE)) {
            return(NULL)
        }
        item <- ml_model_read_item(
            file.path(source$item_dir, paste0(id, ".json"))
        )
        if (is.null(item)) {
            return(NULL)
        }
        list(
            id = id,
            scope = source$scope,
            item = item,
            model_file = model_file
        )
    })
    records[!vapply(records, is.null, logical(1))]
}

# Returns NULL for missing, unreadable or non-MLM Items, so one broken file
# cannot take the whole listing down.
ml_model_read_item <- function(file) {
    if (!file.exists(file)) {
        return(NULL)
    }
    item <- tryCatch(
        jsonlite::read_json(file, simplifyVector = FALSE),
        error = function(e) NULL
    )
    if (!is.list(item) || !identical(item$type, "Feature")) {
        return(NULL)
    }
    if (!ml_item_has_mlm(item)) {
        return(NULL)
    }
    item
}

ml_item_has_mlm <- function(item) {
    extensions <- unlist(item$stac_extensions)
    any(grepl(
        "^https://stac-extensions\\.github\\.io/mlm/v[^/]+/schema\\.json$",
        extensions
    ))
}

#' Build the API representation of a stored model
#'
#' Sets the Item `id` to the model id, turns relative asset hrefs of public
#' models into absolute URLs under `/files/public/models/`, and replaces
#' relative links (which point into the storage folder) with API links.
#'
#' @param record A record from [ml_models_list()].
#' @param api An openeocraft API object.
#' @param req Plumber request.
#' @return A STAC Item as a list.
#' @keywords internal
ml_model_doc <- function(record, api, req) {
    host <- get_host(api, req)
    item <- record$item
    item$id <- record$id
    # save_ml_model stores a missing geometry as `{}`; STAC needs `null`,
    # which the unboxed JSON serializer writes for a logical NA.
    if (!length(item$geometry)) {
        item["geometry"] <- list(NA)
    }
    if (identical(record$scope, "public")) {
        item$assets <- lapply(item$assets, function(asset) {
            if (is_string(asset$href) && !is_absolute_url(asset$href)) {
                asset$href <- make_url(host, paste0(
                    "/files/public/models/", sub("^/+", "", asset$href)
                ))
            }
            asset
        })
    }
    links <- Filter(function(link) {
        is_string(link$rel) && is_string(link$href) &&
            is_absolute_url(link$href) &&
            !link$rel %in% c("self", "root", "parent", "collection")
    }, item$links)
    item$links <- unname(links)
    item <- update_link(
        item,
        rel = "self",
        href = make_url(host, paste0("/ml_models/", record$id)),
        type = "application/geo+json"
    )
    item <- update_link(
        item,
        rel = "parent",
        href = make_url(host, "/ml_models"),
        type = "application/json"
    )
    item <- update_link(
        item,
        rel = "root",
        href = make_url(host, "/"),
        type = "application/json"
    )
    item
}
