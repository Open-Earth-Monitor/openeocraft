# TODO allow many files processes definition
#* @openeo-import math.R
#* @openeo-import data.R

# Internal helpers -------------------------------------------------------
# Use one fewer than total physical cores (fallback to 2) but never more than 16.
.openeocraft_multicores <- function() {
    cores <- base::tryCatch(
        parallel::detectCores(logical = FALSE),
        error = function(...) {
            NA_integer_
        }
    )
    cores <- if (base::is.na(cores) || !base::is.numeric(cores)) {
        2L
    } else {
        base::max(1L, cores - 1L)
    }
    base::as.integer(base::min(cores, 16L))
}

# Default memory (GB) for sits processing; can be overridden via option
# options(openeocraft.memsize = 8L)
.openeocraft_memsize <- function() {
    mem <- base::getOption("openeocraft.memsize", 8L)
    if (!base::is.numeric(mem) ||
        base::length(mem) != 1 || base::is.na(mem)) {
        mem <- 8L
    }
    base::as.integer(base::max(1L, mem))
}

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

            # Create data cube with correct roi format
            data <- sits::sits_cube(
                source = source,
                collection = collection,
                bands = bands,
                roi = roi,
                # sf polygon object
                start_date = temporal_extent[[1]],
                end_date = temporal_extent[[2]]
            )

            base::attr(data, "roi") <- roi
            return(data)
        },
        error = function(e) {
            stop(base::paste("Error in load_collection():", e$message))
        }
    )
}


#* @openeo-process
mlm_class_random_forest <- function(num_trees = 100,
                                    max_variables = "sqrt",
                                    seed = NULL) {
    base::message("[mlm_class_random_forest] START")
    base::on.exit(base::message("[mlm_class_random_forest] END"))
    list(
        train = function(training_set) {
            base::message("[mlm_class_random_forest::train] START")
            base::on.exit(base::message("[mlm_class_random_forest::train] END"))
            # start preparing max_variables parameter
            n_bands <- base::length(sits::sits_bands(training_set))
            n_times <- base::length(sits::sits_timeline(training_set))
            n_features <- n_bands * n_times
            if (base::is.character(max_variables)) {
                if (max_variables == "sqrt") {
                    max_variables <- base::sqrt(n_features)
                } else if (max_variables == "log2") {
                    max_variables <- base::log2(n_features)
                }
            } else if (base::is.null(max_variables)) {
                max_variables <- n_features
            } else if (base::is.numeric(max_variables)) {
                max_variables <- max_variables
            } else {
                stop("Invalid max_variables parameter", call. = FALSE)
            }
            max_variables <- base::max(1, base::floor(max_variables))
            # end preparing max_variables parameter
            model <- sits::sits_rfor(num_trees = num_trees, mtry = max_variables)
            if (!base::is.null(seed)) {
                base::set.seed(seed)
            }
            sits::sits_train(training_set, model)
        }
    )
}

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
            sits::sits_train(training_set, model)
        }
    )
}

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
            sits::sits_train(training_set, model)
        }
    )
}


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
            sits::sits_train(training_set, model)
        }
    )
}

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
            sits::sits_train(training_set, model)
        }
    )
}

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
            sits::sits_train(training_set, model)
        }
    )
}


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
            sits::sits_train(training_set, model)
        }
    )
}

#* @openeo-process
ml_fit <- function(model, training_set, target = "label") {
    base::message("[ml_fit] START")
    base::on.exit(base::message("[ml_fit] END"))
    # Allow training_set as:
    # 1) JSON-serialized object
    # 2) Local path to an RDS file
    # 3) URL to an RDS file
    # 4) Already-loaded object
    load_training <- function(x) {
        if (!base::is.character(x) || base::length(x) != 1L) {
            return(x) # already an object
        }

        # URL to RDS
        if (base::grepl("^https?://", x)) {
            tmp <- base::tempfile(fileext = ".rds")
            base::on.exit(base::unlink(tmp), add = TRUE)
            return(base::tryCatch(
                {
                    utils::download.file(x, tmp, mode = "wb", quiet = TRUE)
                    base::readRDS(tmp)
                },
                error = function(e) {
                    stop(
                        base::sprintf(
                            "Failed to download/read training_set from URL: %s",
                            e$message
                        ),
                        call. = FALSE
                    )
                }
            ))
        }

        # JSON-serialized object (explicit option, not just fallback)
        json_obj <- base::tryCatch(
            jsonlite::unserializeJSON(x),
            error = function(...) {
                NULL
            }
        )
        if (!base::is.null(json_obj)) {
            return(json_obj)
        }

        # Local RDS file
        if (base::file.exists(x)) {
            return(base::tryCatch(
                base::readRDS(x),
                error = function(e) {
                    stop(
                        base::sprintf(
                            "Failed to read training_set from file: %s",
                            e$message
                        ),
                        call. = FALSE
                    )
                }
            ))
        }

        stop(
            "training_set must be a URL to RDS, a local RDS path, a JSON-serialized object, or an object",
            call. = FALSE
        )
    }

    training_obj <- load_training(training_set)

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
    model$train(training_obj)
}

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

    # Regularize
    data <- sits::sits_regularize(
        cube = data,
        period = period,
        res = resolution,
        output_dir = result_dir,
        roi = roi,
        multicores = .openeocraft_multicores()
    )
    data
}

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
    # data <- sits::sits_select(data, bands = target_band)
    data
}

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
    # Create assets list
    assets <- list()
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
            list(
                href = path,
                # TODO: implement format_content_type() function
                type = openeocraft::format_content_type(format),
                roles = base::list("data")
            )
        })
        base::names(tile_assets) <- filename
        assets <- c(assets, tile_assets)
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

#* @openeo-process
save_ml_model <- function(data, name, tasks, options = list()) {
    base::message("[save_ml_model] START")
    base::on.exit(base::message("[save_ml_model] END"))
    # Input validation
    if (!base::is.character(name) ||
        base::length(name) != 1 || base::nchar(name) == 0) {
        openeocraft::api_stop(400, "Parameter 'name' must be a non-empty string")
    }

    # Validate and convert tasks parameter
    if (!base::is.character(tasks) && !base::is.list(tasks)) {
        openeocraft::api_stop(400, "Parameter 'tasks' must be an array of strings")
    }

    # Convert tasks from list to character vector if needed
    if (base::is.list(tasks)) {
        tasks <- base::unlist(tasks, use.names = FALSE)
    }

    # Validate tasks enum values
    valid_tasks <- c(
        "regression",
        "classification",
        "object-detection",
        "detection",
        "scene-classification",
        "segmentation",
        "semantic-segmentation",
        "similarity-search",
        "generative",
        "image-captioning",
        "super-resolution"
    )

    invalid_tasks <- base::setdiff(tasks, valid_tasks)
    if (base::length(invalid_tasks) > 0) {
        openeocraft::api_stop(
            400,
            base::paste0(
                "Invalid task(s) in 'tasks' parameter: ",
                base::paste(invalid_tasks, collapse = ", "),
                ". Valid tasks are: ",
                base::paste(valid_tasks, collapse = ", ")
            )
        )
    }

    # Validate data parameter - must be non-null to save
    if (base::is.null(data)) {
        openeocraft::api_stop(400, "Parameter 'data' must be a non-null model object")
    }

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
            stac_item$stac_extensions <- list("https://stac-extensions.github.io/mlm/v1.5.0/schema.json")

            # Start with required MLM properties
            mlm_properties <- list(`mlm:name` = name, `mlm:tasks` = tasks)

            # Add architecture, input, and output if provided
            if (!base::is.null(options$architecture)) {
                mlm_properties$`mlm:architecture` <- options$architecture
            }
            if (!base::is.null(options$input)) {
                mlm_properties$`mlm:input` <- options$input
            }
            if (!base::is.null(options$output)) {
                mlm_properties$`mlm:output` <- options$output
            }

            # Process all other options dynamically, handling both 'mlm:' prefixed and unprefixed keys
            mlm_option_keys <- base::c(
                "framework",
                "framework_version",
                "memory_size",
                "total_parameters",
                "pretrained",
                "pretrained_source",
                "batch_size_suggestion",
                "accelerator",
                "accelerator_constrained",
                "accelerator_summary",
                "accelerator_count",
                "hyperparameters"
            )

            for (key in mlm_option_keys) {
                # Check for both 'mlm:key' and 'key' variants
                mlm_key <- base::paste0("mlm:", key)
                value <- NULL

                if (mlm_key %in% base::names(options)) {
                    value <- options[[mlm_key]]
                } else if (key %in% base::names(options)) {
                    value <- options[[key]]
                }

                if (!base::is.null(value)) {
                    mlm_properties[[mlm_key]] <- value
                }
            }

            # Handle any additional options that start with 'mlm:'
            for (key in base::names(options)) {
                if (base::startsWith(key, "mlm:") &&
                    !key %in% base::names(mlm_properties)) {
                    mlm_properties[[key]] <- options[[key]]
                }
            }

            stac_item$properties <- mlm_properties

            # Handle artifact_type (check both variants)
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

            # Handle compile_method (check both variants)
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
            public_item_json_path <- base::file.path(public_models_dir, item_filename)
            jsonlite::write_json(
                x = public_stac_item,
                path = public_item_json_path,
                auto_unbox = TRUE
            )

            # Create or update parent collection
            collection_json_path <- base::file.path(job_dir, "collection.json")

            if (base::file.exists(collection_json_path)) {
                # Update existing collection
                collection <- jsonlite::read_json(collection_json_path, simplifyVector = FALSE)

                # Add item link if not already present
                item_link <- list(href = item_filename, rel = "item")

                link_exists <- base::any(base::sapply(collection$links, function(link) {
                    !base::is.null(link$href) && link$href == item_filename
                }))

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
                        spatial = list(bbox = list(list(
                            -180, -90, 180, 90
                        ))),
                        temporal = list(interval = list(
                            list("1900-01-01T00:00:00Z", "9999-12-31T23:59:59Z")
                        ))
                    ),
                    item_assets = list(weights = list(
                        title = "model weights",
                        roles = list("mlm:model", "mlm:weights")
                    )),
                    summaries = list(
                        datetime = list(minimum = "1900-01-01T00:00:00Z", maximum = "9999-12-31T23:59:59Z")
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

            public_collection_json_path <- base::file.path(public_models_dir, "collection.json")
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
