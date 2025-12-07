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
    temporal_extent = c("2022-01-01", "2022-12-31")
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
# Load training data
data_deforestation_rondonia <- readRDS("./data/samples_deforestation_rondonia.rds")
# Initialize TempCNN model
tempcnn_model_init <-  p$mlm_class_tempcnn(optimizer = "adam",
                                           learning_rate = 0.0005,
                                           seed = 42)
# Model Training
tempcnn_model <-  p$ml_fit(
    training_set = jsonlite::serializeJSON(data_deforestation_rondonia),
    target = "label"
)
# Apply the trained model to make a prediction
datacube <-  p$ml_predict(datacube, tempcnn_model)
# Save the Model
model <- p$save_ml_model(
    tempcnn_model,
    name = "tempcnn_model_2022_rondonia",
    tasks = list("classification"),
    options = list("mlm:accelerator" = "macos-arm", "mlm:framework" = "Torch for R")
)
# Save the prediction result
ml_job <- p$save_result(data = datacube, format = "GTiff")

# Run the job
job <- create_job(graph = ml_job,
                  title = "Deforestation Prediction in Rondonia",
                  description =
                      "Using TempCNN model to predict deforestation in Rondonia")
job <- start_job(job)

# Display job information
describe_job(job)
