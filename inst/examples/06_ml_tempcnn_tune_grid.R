# TempCNN fine-tuning (grid search) + inference via the openEO R client.
#
# Prerequisites:
#   - OpenEOcraft backend running locally (e.g. source docker/plumber.R in R/RStudio,
#     or docker compose up)
#   - openeo R package installed
#
# Run from repo root:
#   Rscript inst/examples/06_ml_tempcnn_tune_grid.R
#
# Default connection below matches docker/plumber.R demo credentials (user brian).
# With a local plumber process, job artefacts are written under the backend
# workdir, e.g. <workdir>/workspace/<user>/jobs/<job_id>/ (tuning_results.json,
# saved model, GeoTIFF). Set the workdir when starting the API if needed.
#
# Cube bands must match the training RDS timeline and features (P16D; add NDVI
# when samples were built with an NDVI band). See inst/demo-lps-2025/00_ml_month.ipynb.
# Grid search uses a small param_grid for demo wall time; see inst/demo-lps-2025/.

library(openeo)

con <- connect(
    host = "http://127.0.0.1:8000",
    user = "brian",
    password = "123456"
)

p <- processes()

deforestation_data <-
    "https://github.com/e-sensing/sitsdata/raw/main/data/samples_deforestation_rondonia.rds"

tempcnn_model_init <- p$mlm_class_tempcnn(
    optimizer = "adam",
    learning_rate = 0.0005,
    seed = 42,
    epochs = 20,
    batch_size = 64,
    verbose = TRUE
)

param_grid <- list(
    learning_rate = c(0.0005, 0.0001),
    epochs = c(20, 40)
)

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

datacube <- p$load_collection(
    id = "mpc-sentinel-2-l2a",
    spatial_extent = list(
        west = -63.50,
        south = -8.92,
        east = -63.35,
        north = -8.78
    ),
    temporal_extent = c("2022-01-01", "2022-12-31"),
    bands = list(
        "b02", "b03", "b04", "b05", "b06", "b07", "b08",
        "b11", "b12", "b8a"
    )
)

datacube <- p$cube_regularize(
    data = datacube,
    period = "P16D",
    resolution = 30
)

datacube <- p$ndvi(
    data = datacube,
    red = "B04",
    nir = "B08",
    target_band = "NDVI"
)

data <- p$ml_predict(data = datacube, model = tempcnn_model)

ml_job <- p$save_result(data = data, format = "GTiff")

job <- create_job(
    graph = ml_job,
    title = "TempCNN fine-tuning + inference",
    description = "Grid-search TempCNN on Rondonia samples; predict GeoTIFF"
)
job <- start_job(job)

describe_job(job)
