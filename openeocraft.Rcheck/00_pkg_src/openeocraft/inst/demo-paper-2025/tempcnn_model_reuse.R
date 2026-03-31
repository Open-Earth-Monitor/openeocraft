library(openeo)
connection <- connect("http://127.0.0.1:8000")
login(user = "brian", password = "123456")

# Retrieve available processes from the backend
p <- processes()
# Load Sentinel-2 Data
datacube <- p$load_collection(
    id = "mpc-sentinel-2-l2a",
    spatial_extent = list(
        west = -63.33,
        south = -12.03,
        east = -62.43,
        north = -11.13,
        crs = 4326
    ),
    temporal_extent = c("2023-01-01", "2023-12-31")
)
# Regularize the Sentinel-2 data
datacube <-  p$cube_regularize(data = datacube,
                               period = "P16D",
                               resolution = 600)
# NDVI calculation
datacube <- p$ndvi(
    data = datacube,
    red = "B04",
    nir = "B08",
    target_band = "NDVI"
)


# Load an existing TempCNN model
tempcnn_model <- p$load_ml_model("tempcnn_model_2022_rondonia")

# Apply the loaded trained model to make a prediction
datacube <-  p$ml_predict(datacube, tempcnn_model)

# Save the prediction result
ml_job <- p$save_result(data = datacube, format = "GTiff")

# Run the job
job <- create_job(graph = ml_job,
                  title = "Deforestation Prediction using Loaded Model",
                  description =
                      "Using Pre-existing TempCNN model to predict deforestation in Rondonia")
job <- start_job(job)

# Display job information
describe_job(job)
