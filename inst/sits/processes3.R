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
    start_date = spatial_extent[["t0"]],
    end_date = spatial_extent[["t1"]]
  )
  # Save roi for later
  attr(data, "roi") <- spatial_extent
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
  roi <- NULL
  if (!is.null(attr(data, "roi"))) {
    roi <- attr(data, "roi")
  }
  # Predict
  data <- sits::sits_classify(
    data = data,
    ml_model = model,
    roi = roi,
    memsize = 2L,
    multicores = 2L,
    output_dir = job_dir
  )
  # label the probability cube
  data <- sits::sits_label_classification(
    cube = data,
    memsize = 2L,
    multicores = 2L,
    output_dir = job_dir
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
  roi <- NULL
  if (!is.null(attr(data, "roi"))) {
    roi <- attr(data, "roi")
  }
  # Regularize
  data <- sits::sits_regularize(
    cube = data,
    period = period,
    res = resolution,
    output_dir = job_dir,
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
  # Regularize
  nir <- as.name(nir)
  red <- as.name(red)
  data <- sits::sits_apply(
    data = data,
    NDVI = (nir - red) / (nir + red),
    memsize = 2L,
    multicores = 2L,
    output_dir = job_dir
  )
  data
}

#* @openeo-process
save_result <- function(data, format, options = NULL) {
  env <- openeocraft::current_env()
  job_dir <- openeocraft::job_get_dir(env$api, env$user, env$job$id)
  host <- openeocraft::get_host(env$api, env$req)

  for (i in seq_len(nrow(data))) {
    # TODO: move files to result subfolder
    path <- gsub("^.*/", "/result/", data$file_info[[i]]$path)
    data$file_info[[i]]$path <- make_files_url(host, env$user, env$job$id, path)
  }
  saveRDS(data, file.path(job_dir, ""))
  return(TRUE)
}
