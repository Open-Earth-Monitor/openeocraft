# TODO allow many files processes definition
#* @openeo-import math.R
#* @openeo-import data.R

#* @openeo-process
load_collection <- function(id,
                            spatial_extent = NULL,
                            temporal_extent = NULL,
                            bands = NULL,
                            properties = NULL) {
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
ml_fit_class_random_forest <- function(training_set,
                                       target,
                                       max_variables,
                                       num_trees,
                                       train_test_split,
                                       random_state = NULL) {
  if (!base::is.null(random_state)) {
    base::set.seed(random_state)
  }
  model <- sits::sits_rfor(
    samples = training_set,
    num_trees = num_trees,
    mtry = max_variables
  )
  model
}

#* @openeo-process
ml_predict <- function(data,
                       model,
                       dimensions = NULL) {
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
  # # label the probability cube
  # data <- sits::sits_label_classification(
  #   cube = data,
  #   memsize = 2L,
  #   multicores = 2L,
  #   output_dir = result_dir
  # )
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
    multicores = 2L
  )
  data
}

#* @openeo-process
ndvi <- function(data, nir = "nir", red = "red", target_band = NULL) {
  # Get current context of evaluation environment
  env <- openeocraft::current_env()
  # Preparing parameters
  job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
  # Create result directory
  result_dir <- base::file.path(job_dir, "temp")
  if (!base::dir.exists(result_dir)) {
    base::dir.create(result_dir)
  }
  # Regularize
  nir <- base::as.name(nir)
  red <- base::as.name(red)
  # data <- sits::sits_apply(
  #   data = data,
  #   NDVI = call("/", call("-", nir, red), call("+", nir, red)),
  #   memsize = 2L,
  #   multicores = 2L,
  #   output_dir = result_dir
  # )
  data <- base::do.call(
    sits::sits_apply,
    args = list(
      data = data,
      NDVI = base::call("/", base::call("-", nir, red), base::call("+", nir, red)),
      memsize = 2L,
      multicores = 2L,
      output_dir = result_dir
    ),
    envir = base::baseenv()
  )
  data
}

#* @openeo-process
save_result <- function(data, format, options = NULL) {
  env <- openeocraft::current_env()
  host <- openeocraft::get_host(env$api, env$req)
  result_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
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
export_to_workspace <- function(data, name, folder) {
  # Get current context of evaluation environment
  env <- openeocraft::current_env()
  # Get workspace directory
  workspace_dir <- openeocraft::api_user_workspace(env$api, env$user)
  workspace_dir <- base::file.path(workspace_dir, "root")
  # Create result directory
  result_dir <- base::file.path(workspace_dir, folder)
  if (!base::dir.exists(result_dir)) {
    base::dir.create(result_dir, recursive = TRUE)
  }
  if (!base::dir.exists(result_dir)) {
    openeocraft::api_stop(500, "Could not create the folder")
  }
  # Save RDS object representation
  file <- base::file.path(result_dir, base::paste0(name, ".rds"))
  base::saveRDS(data, file)
  TRUE
}

#* @openeo-process
import_from_workspace <- function(name, folder) {
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
  file <- base::file.path(result_dir, base::paste0(name, ".rds"))
  if (!base::file.exists(file)) {
    openeocraft::api_stop(500, "File does not exist")
  }
  data <- base::readRDS(file)
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
    memsize = 2L,
    multicores = 2L,
    output_dir = result_dir
  )
  data
}
