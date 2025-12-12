# TODO allow many files processes definition
#* @openeo-import math.R
#* @openeo-import data.R

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
    list(
        train = function(training_set) {
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
    formula <- sits::sits_formula_linear()

    list(
        train = function(training_set) {
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
    list(
        train = function(training_set) {
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
                              seed = NULL) {
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
    # end preparing parameters
    list(
        train = function(training_set) {
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
mlm_class_tae <- function(epochs = 150,
                          batch_size = 64,
                          optimizer = "adam",
                          learning_rate = 0.001,
                          epsilon = 0.00000001,
                          weight_decay = 0.000001,
                          lr_decay_epochs = 1,
                          lr_decay_rate = 0.95,
                          seed = NULL) {
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
    # Check if training_set is a URL
    if (base::grepl("^https?://", training_set, ignore.case = TRUE)) {
        # Create a temporary file to store the downloaded data
        temp_file <- base::tempfile(fileext = ".rds")
        base::on.exit(base::unlink(temp_file)) # Clean up temp file on exit

        # Download the file and read the RDS data
        tryCatch(
            {
                utils::download.file(training_set,
                    temp_file,
                    mode = "wb",
                    quiet = TRUE
                )
                training_data <- base::readRDS(temp_file)
            },
            error = function(e) {
                stop(
                    "Failed to download or read the training data from URL: ",
                    e$message
                )
            }
        )
    } else {
        # Handle existing JSON serialized data
        training_data <- tryCatch(
            {
                jsonlite::unserializeJSON(training_set)
            },
            error = function(e) {
                stop(
                    "Failed to parse training data. Ensure it's either a valid JSON string or a URL to an RDS file."
                )
            }
        )
    }

    # Train the model with the loaded data
    model$train(training_data)
}

#* @openeo-process
ml_predict <- function(data, model) {
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
        memsize = 16L,
        multicores = 8L,
        output_dir = result_dir
    )
    # label the probability cube
    data <- sits::sits_label_classification(
        cube = data,
        memsize = 16L,
        multicores = 8L,
        output_dir = result_dir
    )
    data
}

#* @openeo-process
ml_predict_probabilities <- function(data, model) {
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
        memsize = 16L,
        multicores = 8L,
        output_dir = result_dir
    )
    data
}


#* @openeo-process
ml_uncertainty_class <- function(data, approach = "margin") {
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
        memsize = 16L,
        multicores = 8L,
        output_dir = result_dir
    )
    data
}

#* @openeo-process
ml_smooth_class <- function(data,
                            window_size = 7L,
                            neighborhood_fraction = 0.5,
                            smoothness = 10L) {
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
        memsize = 16L,
        multicores = 8L,
        output_dir = result_dir
    )
    data
}

#* @openeo-process
ml_label_class <- function(data) {
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
        memsize = 16L,
        multicores = 8L,
        output_dir = result_dir
    )
    data
}

#* @openeo-process
cube_regularize <- function(data, resolution, period) {
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
        multicores = 8L
    )
    data
}

#* @openeo-process
ndvi <- function(data,
                 nir = "nir",
                 red = "red",
                 target_band = NULL) {
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
        memsize <- 16L
        multicores <- 8L
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

#* @openeo-process
save_result <- function(data, format, options = NULL) {
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
        multicores = 8L,
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
        multicores = 8L,
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
save_ml_model <- function(data, name, tasks, options = NULL) {
    base::tryCatch(
        {
            # Initialize environment and host
            env <- openeocraft::current_env()
            host <- openeocraft::get_host(env$api, env$req)

            # Get directories
            job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
            result_dir <- job_dir # they are the same

            # Use visible "mlmodel" folder instead of hidden ".obj"
            mlmodel_dir <- base::file.path(result_dir, "mlmodel")

            # Create mlmodel directory if it doesn't exist
            if (!base::dir.exists(mlmodel_dir)) {
                base::dir.create(mlmodel_dir, recursive = TRUE)
            }

            if (!base::dir.exists(mlmodel_dir)) {
                openeocraft::api_stop(500, "Could not create the mlmodel folder")
            }

            # Save RDS object representation with the model name
            model_file <- base::file.path(mlmodel_dir, base::paste0(name, ".rds"))
            base::saveRDS(data, model_file)

            # Verify the file was saved correctly
            if (!base::file.exists(model_file)) {
                openeocraft::api_stop(500, "Failed to save model file")
            }

            # Create STAC Item (Feature) with mlm extension
            # Following the mlm extension specification and examples
            item <- base::list()

            # STAC core fields
            item$stac_version <- "1.0.0"
            item$type <- "Feature"
            item$id <- name
            item$stac_extensions <- base::list("https://stac-extensions.github.io/mlm/v1.0.0/schema.json")

            # Properties - start with mlm extension fields
            item$properties <- base::list()
            item$properties[["mlm:name"]] <- name
            item$properties[["mlm:tasks"]] <- tasks

            # Add mlm:input and mlm:output (basic structure)
            # These can be enhanced based on model metadata or options
            item$properties[["mlm:input"]] <- base::list(
                base::list(type = "data-cube", description = "Input data cube for model prediction")
            )
            item$properties[["mlm:output"]] <- base::list(
                base::list(type = "data-cube", description = "Output data cube from model prediction")
            )

            # If options is not NULL, merge into properties
            # This allows custom mlm properties like mlm:framework, mlm:name, etc.
            if (!base::is.null(options) && base::is.list(options)) {
                for (key in base::names(options)) {
                    item$properties[[key]] <- options[[key]]
                }
            }

            # Assets - add model asset with mlm:model role
            model_filename <- base::basename(model_file)
            model_url <- openeocraft::make_job_files_url(
                host = host,
                user = env$user,
                job_id = env$job$id,
                file = base::file.path("mlmodel", model_filename)
            )

            item$assets <- base::list()
            item$assets[["model"]] <- base::list(
                href = model_url,
                type = "application/x-rds",
                # RDS format
                title = name,
                description = base::paste("Machine learning model:", name),
                roles = base::list("mlm:model", "data")
            )

            # Links (optional but good practice)
            item$links <- base::list(
                base::list(
                    rel = "self",
                    href = base::file.path(job_dir, "ml_collection.json"),
                    type = "application/geo+json"
                )
            )

            # Save the STAC Item JSON in the job directory
            item_json_path <- base::file.path(job_dir, "ml_collection.json")
            jsonlite::write_json(
                x = item,
                path = item_json_path,
                auto_unbox = TRUE,
                pretty = TRUE
            )

            return(TRUE)
        },
        error = function(e) {
            if (base::inherits(e, "openeocraft_api_error")) {
                stop(e)
            }
            stop(base::paste("Error in save_ml_model():", e$message))
        }
    )
}

#* @openeo-process
load_stac_ml <- function(uri,
                         model_asset = NULL,
                         input_index = 0L,
                         output_index = 0L) {
    base::tryCatch(
        {
            # Check if rstac is available
            if (!base::requireNamespace("rstac", quietly = TRUE)) {
                stop("Package 'rstac' is required for loading STAC Items")
            }

            # Validate indices (0-based from spec, will convert to 1-based for R)
            if (input_index < 0L) {
                stop("input_index must be >= 0")
            }
            if (output_index < 0L) {
                stop("output_index must be >= 0")
            }

            # Load STAC Item from URI (URL or local file)
            item <- NULL
            if (base::grepl("^https?://", uri)) {
                # Remote URL - use rstac to fetch
                # Try to determine if it's an item, collection, or search endpoint
                response <- rstac::stac(uri) %>%
                    rstac::get_request()

                # Check if it's a FeatureCollection (search results)
                if (!base::is.null(response$features) &&
                    base::length(response$features) > 0) {
                    # It's a FeatureCollection, take the first item
                    item <- response$features[[1]]
                } else if (!base::is.null(response$type) &&
                    response$type == "Feature") {
                    # It's a direct STAC Item
                    item <- response
                } else if (!base::is.null(response$type) &&
                    response$type == "Collection") {
                    # It's a Collection, we need an item - check links
                    if (!base::is.null(response$links)) {
                        item_links <- base::Filter(
                            function(link) {
                                link$rel == "item"
                            },
                            response$links
                        )
                        if (base::length(item_links) > 0) {
                            # Try to fetch the first item
                            item_uri <- item_links[[1]]$href
                            if (base::grepl("^https?://", item_uri)) {
                                item <- rstac::stac(item_uri) %>%
                                    rstac::get_request()
                            } else {
                                # Relative URL - construct from base URI
                                base_uri <- base::sub("/[^/]*$", "", uri)
                                item_uri <- base::paste0(base_uri, "/", item_uri)
                                item <- rstac::stac(item_uri) %>%
                                    rstac::get_request()
                            }
                        } else {
                            stop("Collection has no item links")
                        }
                    } else {
                        stop("Cannot load model from Collection, please provide a STAC Item URI")
                    }
                } else {
                    stop("Unexpected STAC response type")
                }
            } else {
                # Local file path
                if (!base::file.exists(uri)) {
                    stop("STAC Item file not found: ", uri)
                }
                item <- jsonlite::fromJSON(uri, simplifyVector = FALSE)
            }

            # Validate that item is a Feature
            if (base::is.null(item$type) ||
                item$type != "Feature") {
                stop("STAC Item must be of type 'Feature'")
            }

            # Validate that item has mlm extension
            if (base::is.null(item$stac_extensions) ||
                !base::any(base::grepl("mlm", item$stac_extensions))) {
                stop("STAC Item must implement the 'mlm' extension")
            }

            # Convert 0-based indices to 1-based for R array access
            r_input_index <- input_index + 1L
            r_output_index <- output_index + 1L

            # Access mlm:input array from properties (if present)
            mlm_input <- item$properties[["mlm:input"]]
            if (!base::is.null(mlm_input)) {
                if (!base::is.list(mlm_input)) {
                    stop("mlm:input must be an array")
                }
                if (base::length(mlm_input) == 0) {
                    stop("mlm:input array is empty")
                }
                if (r_input_index > base::length(mlm_input)) {
                    stop(
                        "input_index ",
                        input_index,
                        " is out of bounds. ",
                        "mlm:input array has ",
                        base::length(mlm_input),
                        " element(s)."
                    )
                }
                # Store the selected input specification
                selected_input <- mlm_input[[r_input_index]]
            } else {
                selected_input <- NULL
            }

            # Access mlm:output array from properties (if present)
            mlm_output <- item$properties[["mlm:output"]]
            if (!base::is.null(mlm_output)) {
                if (!base::is.list(mlm_output)) {
                    stop("mlm:output must be an array")
                }
                if (base::length(mlm_output) == 0) {
                    stop("mlm:output array is empty")
                }
                if (r_output_index > base::length(mlm_output)) {
                    stop(
                        "output_index ",
                        output_index,
                        " is out of bounds. ",
                        "mlm:output array has ",
                        base::length(mlm_output),
                        " element(s)."
                    )
                }
                # Store the selected output specification
                selected_output <- mlm_output[[r_output_index]]
            } else {
                selected_output <- NULL
            }

            # Find assets with mlm:model role
            assets <- item$assets
            if (base::is.null(assets) ||
                base::length(assets) == 0) {
                stop("STAC Item has no assets")
            }

            model_assets <- base::list()
            for (asset_name in base::names(assets)) {
                asset <- assets[[asset_name]]
                roles <- asset$roles
                if (!base::is.null(roles) &&
                    "mlm:model" %in% roles) {
                    model_assets[[asset_name]] <- asset
                }
            }

            if (base::length(model_assets) == 0) {
                stop("No assets with 'mlm:model' role found in STAC Item")
            }

            # Determine which asset to use
            if (base::is.null(model_asset)) {
                if (base::length(model_assets) > 1) {
                    stop(
                        "Multiple assets with 'mlm:model' role found. ",
                        "Please specify 'model_asset' parameter."
                    )
                }
                model_asset <- base::names(model_assets)[[1]]
            } else {
                if (!model_asset %in% base::names(model_assets)) {
                    stop("Specified 'model_asset' not found or does not have 'mlm:model' role")
                }
            }

            # Get the selected asset
            selected_asset <- model_assets[[model_asset]]

            # Get current context for temporary storage
            env <- openeocraft::current_env()
            job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
            temp_dir <- base::file.path(job_dir, "temp")
            if (!base::dir.exists(temp_dir)) {
                base::dir.create(temp_dir, recursive = TRUE)
            }

            # Download or copy the model file
            asset_href <- selected_asset$href
            model_path <- NULL

            if (base::grepl("^https?://", asset_href)) {
                # Remote URL - download
                model_filename <- base::basename(asset_href)
                # Remove query parameters if any
                model_filename <- base::strsplit(model_filename, "?", fixed = TRUE)[[1]][[1]]
                model_path <- base::file.path(temp_dir, model_filename)
                utils::download.file(asset_href,
                    model_path,
                    mode = "wb",
                    quiet = TRUE
                )
            } else if (base::file.exists(asset_href)) {
                # Absolute local file path - copy
                model_filename <- base::basename(asset_href)
                model_path <- base::file.path(temp_dir, model_filename)
                base::file.copy(asset_href, model_path, overwrite = TRUE)
            } else {
                # Relative path - resolve relative to STAC Item location
                if (base::grepl("^https?://", uri)) {
                    # For remote STAC Items, resolve relative to base URL
                    base_url <- base::sub("/[^/]*$", "", uri)
                    if (!base::grepl("/$", base_url)) {
                        base_url <- base::paste0(base_url, "/")
                    }
                    # Handle both absolute and relative hrefs
                    if (base::grepl("^/", asset_href)) {
                        # Absolute path from root
                        parsed <- base::strsplit(uri, "/", fixed = TRUE)[[1]]
                        base_url <- base::paste0(parsed[[1]], "//", parsed[[3]], "/")
                        asset_url <- base::paste0(base_url, base::sub("^/", "", asset_href))
                    } else {
                        # Relative path
                        asset_url <- base::paste0(base_url, asset_href)
                    }
                    model_filename <- base::basename(asset_href)
                    model_filename <- base::strsplit(model_filename, "?", fixed = TRUE)[[1]][[1]]
                    model_path <- base::file.path(temp_dir, model_filename)
                    utils::download.file(asset_url,
                        model_path,
                        mode = "wb",
                        quiet = TRUE
                    )
                } else {
                    # Local STAC Item file - resolve relative to file location
                    stac_dir <- base::dirname(base::normalizePath(uri))
                    model_path <- base::file.path(stac_dir, asset_href)
                    if (!base::file.exists(model_path)) {
                        stop("Model file not found: ", model_path)
                    }
                    model_filename <- base::basename(model_path)
                    model_path <- base::file.path(temp_dir, model_filename)
                    base::file.copy(base::file.path(stac_dir, asset_href),
                        model_path,
                        overwrite = TRUE
                    )
                }
            }

            # Load the model based on file extension
            if (base::grepl("\\.rds$", model_path, ignore.case = TRUE)) {
                model <- base::readRDS(model_path)
            } else if (base::grepl("\\.json$", model_path, ignore.case = TRUE)) {
                model <- jsonlite::fromJSON(model_path, simplifyVector = FALSE)
            } else {
                # Try RDS first, then JSON, then raw binary
                tryCatch(
                    {
                        model <- base::readRDS(model_path)
                    },
                    error = function(e1) {
                        tryCatch(
                            {
                                model <- jsonlite::fromJSON(model_path, simplifyVector = FALSE)
                            },
                            error = function(e2) {
                                # For other formats (e.g., PyTorch .pth), store path reference
                                # The actual model loading would need format-specific handlers
                                model <- base::list(
                                    path = model_path,
                                    format = tools::file_ext(model_path),
                                    note = "Model file downloaded but format-specific loader not implemented"
                                )
                            }
                        )
                    }
                )
            }

            # Validate model structure (if it's a list/object)
            if (base::is.list(model)) {
                # Store mlm metadata in model attributes for reference
                # Store the original 0-based indices and the selected specifications
                base::attr(model, "mlm:input_index") <- input_index
                base::attr(model, "mlm:output_index") <- output_index
                base::attr(model, "mlm:input_spec") <- selected_input
                base::attr(model, "mlm:output_spec") <- selected_output
                base::attr(model, "mlm:stac_item") <- item
                base::attr(model, "mlm:asset_name") <- model_asset
            } else {
                stop("Invalid model format: model must be a list/object")
            }

            model
        },
        error = function(e) {
            stop(base::paste("Error in load_stac_ml():", e$message))
        }
    )
}

#* @openeo-process
load_ml_model <- function(id) {
    base::tryCatch(
        {
            # Initialize environment
            env <- openeocraft::current_env()

            # Try to find model in multiple locations:
            # 1. Current job directory mlmodel folder (for models saved in this job)
            # 2. Workspace root directories mlmodel folders (for exported models)
            # 3. Other job directories mlmodel folders (for models from previous jobs)

            job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
            workspace_dir <- openeocraft::api_user_workspace(env$api, env$user)
            workspace_root <- base::file.path(workspace_dir, "root")

            # Try current job mlmodel folder first
            model_file <- base::file.path(job_dir, "mlmodel", base::paste0(id, ".rds"))
            if (base::file.exists(model_file)) {
                model <- base::readRDS(model_file)
                if (base::is.list(model)) {
                    return(model)
                }
            }

            # Search in workspace root folders mlmodel subdirectories
            if (base::dir.exists(workspace_root)) {
                folders <- base::list.dirs(workspace_root,
                    full.names = TRUE,
                    recursive = FALSE
                )
                for (folder in folders) {
                    model_file <- base::file.path(folder, "mlmodel", base::paste0(id, ".rds"))
                    if (base::file.exists(model_file)) {
                        model <- base::readRDS(model_file)
                        if (base::is.list(model)) {
                            return(model)
                        }
                    }
                }
            }

            # If not found, check if it's a collection reference
            # (models saved with save_ml_model create a ml_collection.json)
            collection_file <- base::file.path(job_dir, "ml_collection.json")
            if (base::file.exists(collection_file)) {
                collection <- jsonlite::fromJSON(collection_file, simplifyVector = FALSE)
                if (!base::is.null(collection$id) &&
                    collection$id == id) {
                    # Model should be in mlmodel directory
                    model_file <- base::file.path(job_dir, "mlmodel", base::paste0(id, ".rds"))
                    if (base::file.exists(model_file)) {
                        model <- base::readRDS(model_file)
                        if (base::is.list(model)) {
                            return(model)
                        }
                    }
                }
            }

            # Model not found
            openeocraft::api_stop(
                404,
                base::paste("Model with id '", id, "' not found", sep = "")
            )
        },
        error = function(e) {
            if (base::inherits(e, "openeocraft_api_error")) {
                stop(e)
            }
            stop(base::paste("Error in load_ml_model():", e$message))
        }
    )
}
