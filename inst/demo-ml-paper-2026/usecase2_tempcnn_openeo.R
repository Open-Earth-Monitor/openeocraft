library(openeo)

# Connect to the backend (defaults match docker/plumber.R demo credentials)
connection <- connect(
    host = "http://127.0.0.1:8000",
    user = "brian",
    password = "123456"
)

# Access processes
p <- processes()

# Initialize the TempCNN model
tempcnn_model_init <- p$mlm_class_tempcnn(
    optimizer = "adam",
    learning_rate = 0.0005,
    seed = 42
)

# Training data
deforestation_data <-
    "https://github.com/e-sensing/sitsdata/raw/main/data/samples_deforestation_rondonia.rds"

# Fit the model using the training dataset
tempcnn_model <- p$ml_fit(
    model = tempcnn_model_init,
    training_set = deforestation_data,
    target = "label"
)

# Save the fitted model
tempcnn_model_sv <- p$save_ml_model(
    data = tempcnn_model,
    name = "tempcnn_rondonia_v1"
)

# Load Sentinel-2 data cube
# Spatial extent: original UC2 ~1<U+00B0><U+00D7>1<U+00B0> box scaled to 2/7 area (see spatial_extent_area_fraction.R).
datacube <- p$load_collection(
    id = "mpc-sentinel-2-l2a",
    spatial_extent = list(
        west = -63.6672612,
        east = -63.1327388,
        south = -8.9072612,
        north = -8.3727388
    ),
    temporal_extent = c("2022-01-01", "2022-12-31"),
    bands = c(
        "b02", "b03", "b04", "b05", "b06",
        "b07", "b08", "b11", "b12", "b8a"
    )
)

# Regularize the data cube to 16-day intervals at 30 m resolution
datacube <- p$cube_regularize(
    data = datacube,
    period = "P16D",
    resolution = 30
)

# Compute NDVI and append as an additional feature band
datacube <- p$ndvi(
    data = datacube,
    red = "B04",
    nir = "B08",
    target_band = "NDVI"
)

# Apply the trained TempCNN to the preprocessed data cube
data <- p$ml_predict(data = datacube, model = tempcnn_model)

# Export the prediction as GeoTIFF and submit as a backend job
ml_job <- p$save_result(data = data, format = "GTiff")
job <- create_job(graph = ml_job, title = "Train TempCNN + Inference")
job <- start_job(job)

# Download job when done: folder = ./results/<results_name>/ ; optional single GeoTIFF -> <results_name>.tif
results_name <- "rondonia_2022"
out_dir <- file.path(getwd(), "results", results_name)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

while (TRUE) {
    j <- describe_job(job, con = connection)
    status <- j$status
    status <- if (is.null(status)) "" else tolower(as.character(status))
    if (status %in% c("finished", "completed", "done")) break
    if (status %in% c("error", "failed", "canceled", "cancelled")) {
        stop("Job failed: ", j$status, call. = FALSE)
    }
    Sys.sleep(15)
}
download_results(job = job, folder = out_dir, con = connection)

tifs <- list.files(out_dir, pattern = "\\.[tT][iI][fF]$", full.names = TRUE)
if (length(tifs) == 1L) {
    target_tif <- file.path(out_dir, paste0(results_name, ".tif"))
    file.rename(tifs[[1]], target_tif)
    message("Saved: ", target_tif)
} else {
    message("Saved under: ", normalizePath(out_dir))
}
