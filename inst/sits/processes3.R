# TODO allow many files processes definition
#* @openeo-import math.R
#* @openeo-import data.R

#* @openeo-process
load_collection <- function(id,
                            spatial_extent = NULL,
                            temporal_extent = NULL,
                            bands = NULL,
                            properties = NULL) {
  base::print("load_collection()")
  id <- base::strsplit(id, "/")[[1]]
  source <- id[[1]]
  collection <- id[[2]]
  spatial_extent <- spatial_extent[c("west", "east", "south", "north")]
  base::names(spatial_extent) <- c("lon_max", "lon_min", "lat_min", "lat_max")

  data <- sits::sits_cube(
    source = source,
    collection = collection,
    bands = bands,
    roi = spatial_extent,
    start_date = temporal_extent[[1]],
    end_date = temporal_extent[[2]]
  )
  # Save roi for later
  base::attr(data, "roi") <- spatial_extent
  data
}

#* @openeo-process
ml_random_forest <- function(num_trees = 100,
                             max_depth = NULL,
                             random_state = NULL,
                             classification = TRUE) {
  base::print("ml_random_forest()")
  if (!classification) {
    stop("Regression is not supported", call. = FALSE)
  }
  model <- sits::sits_rfor(
    num_trees = num_trees,
    mtry = max_depth
  )
  base::attr(model, "random_state") <- random_state
  model
}

#* @openeo-process
ml_fit <- function(model, training_set, target="label") {
  base::print("ml_fit()")
  random_state <- base::attr(model, "random_state")
  if (!base::is.null(random_state)) {
    base::set.seed(random_state)
  }
  training_set <- jsonlite::unserializeJSON(training_set)
  base::saveRDS(training_set, "~/predictors.rds")
  sits::sits_train(training_set, model)
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
ml_predict_probability <- function(data, model) {
  base::print("ml_predict_probability()")
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
ml_class_smooth <- function(data,
                            window_size,
                            neighborhood_fraction,
                            smoothness) {
  base::print("ml_class_smooth()")
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
ndvi <- function(data, nir = "nir", red = "red", target_band = NULL) {
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
    window_size = 1L
    memsize = 2L
    multicores = 2L
    normalized = TRUE
    progress = FALSE
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
        warning(sits:::.conf("messages", "sits_apply_out_band"), call. = FALSE)
      }
      return(data)
    }
    in_bands <- sits:::.apply_input_bands(
      cube = data,
      bands = bands,
      expr = expr
    )
    overlap <- base::ceiling(window_size / 2) - 1
    block <- sits:::.raster_file_blocksize(
      sits:::.raster_open_rast(sits:::.tile_path(data))
    )
    job_memsize <- sits:::.jobs_memsize(
      job_size = sits:::.block_size(block = block, overlap = overlap),
      npaths = base::length(in_bands) + 1,
      nbytes = 8,
      proc_bloat = sits:::.conf("processing_bloat_cpu")
    )
    block <- sits:::.jobs_optimal_block(
      job_memsize = job_memsize,
      block = block,
      image_size = sits:::.tile_size(sits:::.tile(data)),
      memsize = memsize,
      multicores = multicores
    )
    block <- sits:::.block_regulate_size(block)
    multicores <- sits:::.jobs_max_multicores(
      job_memsize = job_memsize,
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
    sits:::.cube_merge_tiles(
      dplyr::bind_rows(list(features_cube, features_band))
    )
  }
  #
  data <- .sits_apply(
    data = data,
    out_band = target_band,
    expr = base::bquote((.(base::as.name(nir)) - .(base::as.name(red))) /
                          (.(base::as.name(nir)) + .(base::as.name(red))))
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
    data$file_info[[i]]$path <- openeocraft::make_files_url(
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
export_model <- function(model, name, folder) {
  base::print("export_model()")
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
import_model <- function(name, folder) {
  base::print("import_model()")
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
