library(openeo)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  script_dir <- dirname(sub("^--file=", "", file_arg[[1L]]))
  source(file.path(script_dir, "uc2_job_client.R"), local = TRUE)
} else {
  source("inst/examples/uc2_job_client.R", local = TRUE)
}

host <- normalize_host(require_env("OPENEO_HOST"))
user <- require_env("OPENEO_USER")
password <- require_env("OPENEO_PASSWORD")
output_dir <- Sys.getenv("OUTPUT_DIR", unset = "/work/results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
init_poll_log(output_dir)

poll_seconds <- as.numeric(Sys.getenv("JOB_POLL_SECONDS", unset = "30"))
max_wait_seconds <- as.numeric(Sys.getenv("JOB_MAX_WAIT_SECONDS", unset = "86400"))
if (!is.finite(poll_seconds) || poll_seconds < 1) {
  poll_seconds <- 30
}
if (!is.finite(max_wait_seconds) || max_wait_seconds < poll_seconds) {
  max_wait_seconds <- 86400
}

message(sprintf("Connecting to openEO backend at: %s", host))
connection <- connect_with_retry(host = host, user = user, password = password)

p <- processes(con = connection)

deforestation_data <-
  "https://github.com/e-sensing/sitsdata/raw/main/data/samples_deforestation_rondonia.rds"

tempcnn_model_init <- p$mlm_class_tempcnn(
  optimizer = "adam",
  epochs = 20,
  batch_size = 64
)

parse_csv_nums <- function(raw, default) {
  if (!nzchar(raw)) {
    return(default)
  }
  vals <- suppressWarnings(as.numeric(trimws(strsplit(raw, ",", fixed = TRUE)[[1]])))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) default else vals
}

parse_csv_ints <- function(raw, default) {
  if (!nzchar(raw)) {
    return(default)
  }
  vals <- suppressWarnings(as.integer(trimws(strsplit(raw, ",", fixed = TRUE)[[1]])))
  vals <- vals[!is.na(vals)]
  if (!length(vals)) default else vals
}

param_grid <- list(
  learning_rate = parse_csv_nums(
    Sys.getenv("UC2_TUNE_LEARNING_RATES", unset = ""),
    c(0.0005, 0.0001)
  ),
  epochs = parse_csv_ints(
    Sys.getenv("UC2_TUNE_EPOCHS", unset = ""),
    c(20L, 40L)
  )
)

message("Running TempCNN grid search (ml_tune_grid)...")
tempcnn_tuned <- p$ml_tune_grid(
  model = tempcnn_model_init,
  training_data = deforestation_data,
  target = "label",
  parameters = param_grid,
  scoring = "accuracy",
  cv = 0,
  seed = 42
)

tempcnn_model <- p$save_ml_model(
  data = tempcnn_tuned,
  name = "tempcnn_rondonia_tuned_v1",
  return_model = TRUE
)

extent_full <- list(west = -63.9, east = -62.9, south = -9.14, north = -8.14)
area_frac_raw <- Sys.getenv("UC2_SPATIAL_AREA_FRACTION", unset = "")
area_frac <- if (!nzchar(area_frac_raw)) {
  1 / 7
} else {
  suppressWarnings(as.numeric(area_frac_raw))
}
if (!is.finite(area_frac) || area_frac <= 0) {
  area_frac <- 1 / 7
}
if (area_frac >= 1) {
  spatial_extent <- extent_full
} else {
  lon_c <- (extent_full$west + extent_full$east) / 2
  lat_c <- (extent_full$south + extent_full$north) / 2
  lon_span <- extent_full$east - extent_full$west
  lat_span <- extent_full$north - extent_full$south
  k <- sqrt(area_frac)
  spatial_extent <- list(
    west = lon_c - lon_span * k / 2,
    east = lon_c + lon_span * k / 2,
    south = lat_c - lat_span * k / 2,
    north = lat_c + lat_span * k / 2
  )
}

temporal_start <- Sys.getenv("UC2_TEMPORAL_START", unset = "2022-01-01")
temporal_end <- Sys.getenv("UC2_TEMPORAL_END", unset = "2022-12-31")
if (!nzchar(temporal_start)) {
  temporal_start <- "2022-01-01"
}
if (!nzchar(temporal_end)) {
  temporal_end <- "2022-12-31"
}
temporal_extent <- c(temporal_start, temporal_end)
cube_period <- Sys.getenv("UC2_REGULARIZE_PERIOD", unset = "P16D")
if (!nzchar(cube_period)) {
  cube_period <- "P16D"
}

res_raw <- Sys.getenv("UC2_GRID_RESOLUTION", unset = "300")
if (!nzchar(res_raw)) {
  res_raw <- "300"
}
resolution <- suppressWarnings(as.numeric(res_raw))
if (!is.finite(resolution) || resolution <= 0) {
  resolution <- 300
}

message(sprintf(
  "Inference cube: area fraction=%s, west=%.6f east=%.6f south=%.6f north=%.6f, temporal=%s..%s, period=%s, resolution=%gm",
  format(area_frac, scientific = FALSE, digits = 10),
  spatial_extent$west,
  spatial_extent$east,
  spatial_extent$south,
  spatial_extent$north,
  temporal_extent[[1L]],
  temporal_extent[[2L]],
  cube_period,
  resolution
))

band_spec <- trimws(Sys.getenv("UC2_COLLECTION_BANDS", unset = ""))
load_args <- list(
  id = "mpc-sentinel-2-l2a",
  spatial_extent = spatial_extent,
  temporal_extent = temporal_extent
)
if (nzchar(band_spec)) {
  band_list <- trimws(strsplit(band_spec, ",", fixed = TRUE)[[1]])
  band_list <- band_list[nzchar(band_list)]
  if (length(band_list)) {
    load_args$bands <- band_list
  }
}
if (is.null(load_args$bands)) {
  load_args$bands <- list(
    "B02", "B03", "B04", "B05", "B06", "B07", "B08",
    "B11", "B12", "B8A"
  )
}
datacube <- do.call(p$load_collection, load_args)

datacube <- p$cube_regularize(data = datacube, period = cube_period, resolution = resolution)
datacube <- p$ndvi(data = datacube, red = "B04", nir = "B08", target_band = "NDVI")

message("Running inference (ml_predict)...")
data <- p$ml_predict(data = datacube, model = tempcnn_model)

ml_job <- p$save_result(data = data, format = "GTiff")
job <- create_job(
  graph = ml_job,
  title = "TempCNN fine-tuning + inference",
  con = connection
)
job <- start_job(job, con = connection)

writeLines(as.character(job$id), file.path(output_dir, "last_job_id.txt"))
append_poll_log(sprintf("Submitted job id=%s log=%s", job$id, get(".poll_log_path", envir = .GlobalEnv)))

message("Submitted job successfully:")
print(job)

wait_until_job_terminal(
  job = job,
  con = connection,
  poll_sec = poll_seconds,
  max_sec = max_wait_seconds
)

dest <- download_job_results(job = job, con = connection, output_dir = output_dir)
append_poll_log(sprintf("Done. Results in %s", dest))
message("Done.")
