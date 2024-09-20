# TODO allow many files processes definition
#* @openeo-import math.R
#* @openeo-import data.R

#* @openeo-process
load_collection <- function(id,
                            spatial_extent = NULL,
                            temporal_extent = NULL,
                            bands = NULL,
                            properties = NULL) {
  base::browser()
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
    start_date = spatial_extent[["t0"]],
    end_date = spatial_extent[["t1"]]
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
  api <- openeocraft::current_api()
  user <- openeocraft::current_user()
  job <- openeocraft::current_job()
  # Preparing parameters
  job_dir <- openeocraft::job_get_dir(api, user, job$id)
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
  data <- sits::sits_apply(
    data = data,
    NDVI = (nir - red) / (nir + red),
    memsize = 2L,
    multicores = 2L,
    output_dir = result_dir
  )
  data
}

#* @openeo-process
save_result <- function(data, format, options = NULL) {
  base::browser()
  env <- openeocraft::current_env()
  host <- openeocraft::get_host(env$api, env$req)
  result_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
  # Get ROI from cube
  roi <- NULL
  if (!base::is.null(base::attr(data, "roi"))) {
    roi <- base::attr(data, "roi")
  }
  # Copy files
  data <- sits::sits_cube_copy(
    cube = data,
    roi = roi,
    output_dir = result_dir
  )
  for (tile_id in base::seq_len(base::nrow(data))) {
    # TODO: test to see if this works
    data$file_info[[tile_id]]$path <- openeocraft::make_files_url(
      host = host,
      user = env$user,
      job_id = env$job$id,
      file = base::basename(data$file_info[[tile_id]]$path)
    )
  }

  # TODO implement generic function not relying on an specific data type
  data2 <- tidyr::unnest(data, file_info, names_sep = "_")
  tidyr::nest(data2, .by = c("source", "collection", "satellite", "sensor", "tile", "file_info_fid", "file_info_date"))
  # TODO: implement asset tokens
  assets <- base::lapply(assets_file, \(asset_file) {
    asset_path <- base::file.path("/files/jobs", env$job$id, asset_file)
    base::list(
      href = data[["file_info_path"]],
      # TODO: implement format_content_type() function
      type = openeocraft::format_content_type(format),
      roles = base::list("data")
    )
  })
  # TODO: where to out assets? links? assets?
  collection <- openeocraft::job_empty_collection(env$api, env$user, env$job)
  collection$assets <- results
  jsonlite::write_json(
    x = collection,
    path = base::file.path(job_dir, "_collection.json"),
    auto_unbox = TRUE
  )



  base::saveRDS(data, base::file.path(result_dir, "_data.rds"))
  return(TRUE)
}
