# TODO allow many files processes definition
#* @openeo-import math.R
#* @openeo-import data.R

.data_timeline <- function(data) {
  stars::st_get_dimension_values(data, "t")
}

.data_bands <- function(data) {
  bands <- "default"
  if ("bands" %in% base::names(stars::st_dimensions(data)))
    bands <- stars::st_get_dimension_values(data, "bands")
  bands
}

.data_get_time <- function(data, time) {
  i <- base::match(time, .data_timeline(data))
  if (any(is.na(i))) {
    time <- paste0(time[is.na(i)], collapse = ",")
    openeocraft::api_stop(404, "Time(s) ", time, " not found")
  }
  if ("bands" %in% base::names(stars::st_dimensions(data)))
    return(data[,,,i])
  data[,,i]
}

.data_get_band <- function(data, band) {
  j <- match(band, .data_bands(data))
  if (any(is.na(j))) {
    band <- paste0(band[is.na(j)], collapse = ",")
    openeocraft::api_stop(404, "Band(s) ", band, " not found")
  }
  dplyr::select(data, dplyr::all_of(j))
}

#* @openeo-process
save_result <- function(data, format, options = NULL) {

  format <- base::tolower(format)
  supported_formats <- openeocraft::file_formats()
  outputFormats <- supported_formats$output
  if (!(format %in% base::tolower(base::names(outputFormats)))) {
    stop(base::paste("Format", format, "is not supported."))
  }
  # TODO: split data object into different files based on dates and
  #  bands dimensions.
  #  The number of output files will be: N(dates) * N(bands)
  #  The format parameter just defines the file type to be saved in
  #  this process.
  api <- openeocraft::current_api()
  user <- openeocraft::current_user()
  job <- openeocraft::current_job()
  req <- openeocraft::current_request()
  job_dir <- openeocraft::job_get_dir(api, user, job$id)
  host <- openeocraft::get_host(api, req)
  times <- .data_timeline(data)
  bands <- .data_bands(data)
  # TODO implement generic function not relying on an specific data type
  results <- base::list()
  for (i in base::seq_along(times)) {
    assets_file <- base::lapply(base::seq_along(bands), \(j) {
      asset_file <- base::paste0(base::paste(bands[[j]], times[[i]], sep = "_"),
                                 openeocraft::format_ext(format))
      stars::write_stars(.data_get_band(.data_get_time(data, i), j),
                         base::file.path(job_dir, asset_file))
      asset_file
    })
    # TODO: implement asset tokens
    assets <- base::lapply(assets_file, \(asset_file) {
      asset_path <- base::file.path("/files/jobs", job$id, asset_file)
      base::list(
        href = openeocraft::make_url(
          host = host,
          asset_path,
          token = base64enc::base64encode(base::charToRaw(user))
        ),
        # TODO: implement format_content_type() function
        type = openeocraft::format_content_type(format),
        roles = base::list("data")
      )
    })
    base::names(assets) <- base::paste0(bands, "_", times[[i]])
    results <- base::c(results, assets)
  }
  # TODO: where to out assets? links? assets?
  collection <- openeocraft::job_empty_collection(api, user, job)
  collection$assets <- results
  jsonlite::write_json(
    x = collection,
    path = base::file.path(job_dir, "_collection.json"),
    auto_unbox = TRUE
  )

  return(TRUE)
}

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

#' @openeo-process
ml_fit_class_random_forest <- function(training_set,
                                       target,
                                       max_variables,
                                       num_trees,
                                       train_test_splits,
                                       random_state = NULL) {
  if (!base::is.null(random_state)) {
    base::set.seed(random_state)
  }
  sits::sits_rfor(
    samples = training_set,
    num_trees = num_trees,
    mtry = max_variables
  )
}

#' @openeo-process
ml_predict <- function(data,
                       model,
                       dimensions = NULL) {
  # Get current context of evaluation environment
  api <- openeocraft::current_api()
  user <- openeocraft::current_user()
  job <- openeocraft::current_job()
  # Preparing parameters
  output_dir <- openeocraft::job_get_dir(api, user, job$id)
  roi <- NULL
  if (!is.null(attr(data, "roi")))
    roi <- attr(data, "roi")
  # Predict
  data <- sits::sits_classify(
    data = data,
    ml_model = model,
    roi = roi,
    memsize = 2L,
    multicores = 2L,
    output_dir = output_dir
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

#' @openeo-process
cube_regularize <- function(data, resolution, period) {
  # Get current context of evaluation environment
  api <- openeocraft::current_api()
  user <- openeocraft::current_user()
  job <- openeocraft::current_job()
  # Preparing parameters
  output_dir <- openeocraft::job_get_dir(api, user, job$id)
  roi <- NULL
  if (!is.null(attr(data, "roi")))
    roi <- attr(data, "roi")
  # Regularize
  data <- sits::sits_regularize(
    cube = data,
    period = period,
    res = resolution,
    output_dir = output_dir,
    roi = roi,
    multicores = 2L
  )
  data
}
