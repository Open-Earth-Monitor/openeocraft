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
      base::print(">> load_collection() called")
      base::print(base::paste("  collection_id:", id))
      base::print(base::paste("  bands:", base::paste(bands, collapse = ", ")))
      base::print(base::paste("  temporal_extent:", base::paste(temporal_extent, collapse = " to ")))
      base::print("  spatial_extent:")
      base::print(spatial_extent)

      # Split ID
      parts <- base::strsplit(id, "-", fixed = TRUE)[[1]]
      if (base::length(parts) < 2) {
        stop("Invalid collection ID. Expected format: 'source-collection'.")
      }
      source <- parts[[1]]
      collection <- base::paste(parts[-1], collapse = "-")

      # Validate spatial_extent
      required_keys <- c("west", "east", "south", "north")
      if (!base::all(required_keys %in% base::names(spatial_extent))) {
        stop("Missing keys in spatial_extent. Expected: west, east, south, north")
      }

      # Create sf polygon for ROI
      bbox <- sf::st_bbox(
        c(xmin = spatial_extent$west,
          ymin = spatial_extent$south,
          xmax = spatial_extent$east,
          ymax = spatial_extent$north),
        crs = 4326
      )
      roi <- sf::st_as_sfc(bbox)

      # Validate temporal_extent
      if (!base::is.vector(temporal_extent) || base::length(temporal_extent) != 2) {
        stop("temporal_extent must be a vector of length 2: [start_date, end_date]")
      }

      # Create data cube with correct roi format
      data <- sits::sits_cube(
        source = source,
        collection = collection,
        bands = bands,
        roi = roi,  # sf polygon object
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
  base::print("mlm_class_random_forest()")

  model <- list(
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
  base::print("mlm_class_svm()")
  formula <- sits::sits_formula_linear()

  model <- list(
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
  base::print("mlm_class_xgboost()")
  model <- list(
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
  base::print("mlm_class_mlp()")

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
      "Unsupported optimizer. currently only 'adam, adabound, adabelief, madagrad, nadam, qhadam, radam, swats, yogi' are supported.  ",
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
  model <- list(
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
  base::print("mlm_class_tempcnn()")
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
      "Unsupported optimizer. currently only 'adam, adabound, adabelief, madagrad, nadam, qhadam, radam, swats, yogi' are supported.  ",
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
  model <- list(
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
  base::print("mlm_class_tae()")
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
      "Unsupported optimizer. currently only 'adam, adabound, adabelief, madagrad, nadam, qhadam, radam, swats, yogi' are supported.  ",
      call. = FALSE
    )
  )
  opt_hparams <- list(
    lr = learning_rate,
    eps = epsilon,
    weight_decay = weight_decay
  )
  # end preparing parameters
  model <- list(
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
  base::print("mlm_class_lighttae()")
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
      "Unsupported optimizer. currently only 'adam, adabound, adabelief, madagrad, nadam, qhadam, radam, swats, yogi' are supported.  ",
      call. = FALSE
    )
  )
  opt_hparams <- list(
    lr = learning_rate,
    eps = epsilon,
    weight_decay = weight_decay
  )
  # end preparing parameters
  model <- list(
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
  base::print("ml_fit()")
  training_set <- jsonlite::unserializeJSON(training_set)
  # base::saveRDS(training_set, "~/predictors.rds")
  model$train(training_set)
}

#* @openeo-process
ml_predict <- function(data, model) {
  base::print("ml_predict()")
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
    memsize = 2L,
    multicores = 2L,
    output_dir = result_dir
  )
  # label the probability cube
  data <- sits::sits_label_classification(
    cube = data,
    memsize = 2L,
    multicores = 2L,
    output_dir = result_dir
  )
  # data <- sits::sits_mosaic(
  #   cube = data,
  #   crs = "EPSG:3857",
  #   roi = roi,
  #   multicores = 2L,
  #   output_dir = output_dir
  # )
  data
}

#* @openeo-process
ml_predict_probabilities <- function(data, model) {
  base::print("ml_predict_probabilities()")
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
    memsize = 2L,
    multicores = 2L,
    output_dir = result_dir
  )
  data
}


#* @openeo-process
ml_uncertainty_class <- function(data, approach = "margin") {
  base::print("ml_uncertainty_class ()")
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
    memsize = 2L,
    multicores = 2L,
    output_dir = result_dir
  )
  data
}

#* @openeo-process
ml_smooth_class <- function(data,
                            window_size = 7L,
                            neighborhood_fraction = 0.5,
                            smoothness = 10L) {
  base::print("ml_smooth_class()")
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
    memsize = 2L,
    multicores = 2L,
    output_dir = result_dir
  )
  data
}

#* @openeo-process
ml_label_class <- function(data) {
  base::print("ml_label_class()")
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
    memsize = 2L,
    multicores = 2L,
    output_dir = result_dir
  )
  data
}

#* @openeo-process
cube_regularize <- function(data, resolution, period) {
  base::print("cube_regularize()")
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
    multicores = 2L
  )
  data
}

#* @openeo-process
ndvi <- function(data,
                 nir = "nir",
                 red = "red",
                 target_band = NULL) {
  base::print("ndvi()")
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
    memsize <- 2L
    multicores <- 2L
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
      return(output_feature)
    }, progress = progress)
    sits:::.cube_merge_tiles(dplyr::bind_rows(list(features_cube, features_band)))
  }
  #
  data <- .sits_apply(
    data = data,
    out_band = target_band,
    expr = base::bquote((.(
      base::as.name(nir)
    ) - .(
      base::as.name(red)
    )) /
      (.(
        base::as.name(nir)
      ) + .(
        base::as.name(red)
      )))
  )
  data
}

#* @openeo-process
save_result <- function(data, format, options = NULL) {
  base::print("save_result()")
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
    multicores = 2L,
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
  base::print("load_result()")
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
  base::print("export_cube()")
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
    multicores = 2L,
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
  base::print("import_cube()")
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
  base::print("export_ml_model()")
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
  base::print("import_ml_model()")
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
  base::print("save_ml_model()")

  # Initialize environment and host
  env <- openeocraft::current_env()
  host <- openeocraft::get_host(env$api, env$req)

  # Get directories
  job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
  result_dir <- job_dir # they are the same
  obj_dir <- base::file.path(result_dir, ".obj")

  # Create result directory if it doesn't exist
  if (!base::dir.exists(obj_dir)) {
    base::dir.create(obj_dir, recursive = TRUE)
  }

  if (!base::dir.exists(obj_dir)) {
    openeocraft::api_stop(500, "Could not create the folder")
  }

  # Save RDS object representation
  file <- base::file.path(obj_dir, base::paste0(name, ".rds"))
  base::saveRDS(data, file)

  # Prepare the collection dictionary
  assets <- list()
  collection <- openeocraft::job_empty_collection(env$api, env$user, env$job)

  # Add `name` as `id` and tasks to the collection
  collection$id <- name
  collection$tasks <- tasks

  # If options is not NULL, flatten its keys into the collection
  if (!is.null(options) && is.list(options)) {
    for (key in names(options)) {
      collection[[key]] <- options[[key]]
    }
  }


  collection$assets <- assets

  # Save the collection JSON in the job directory
  collection_json_path <- base::file.path(job_dir, "_collection.json")
  jsonlite::write_json(
    x = collection,
    path = collection_json_path,
    auto_unbox = TRUE
  )

  return(TRUE)
}

#* @openeo-process
load_ml_model <- function(name) {
  base::print("load_ml_model()")

  # TO DO : use rstac to load the model, can be in https or in s3 object storage
  # model metadata is in the collection.json file

  # Initialize environment
  env <- openeocraft::current_env()

  # Get job directory
  job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)

  # Get model file path
  model_file <- base::file.path(job_dir, ".obj", base::paste0(name, ".rds"))

  # Check if model file exists
  if (!base::file.exists(model_file)) {
    openeocraft::api_stop(404, "Model file not found")
  }

  # Load the model
  model <- base::readRDS(model_file)

  # Basic validation
  if (!base::is.list(model)) {
    openeocraft::api_stop(400, "Invalid model format")
  }

  model
}
