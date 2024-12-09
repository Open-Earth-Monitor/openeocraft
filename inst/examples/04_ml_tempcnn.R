library(openeo)
library(sits)

# Connect to the backend
con <- connect("http://127.0.0.1:8000", user = "brian", password = "123456")

# Access processes
p <- processes()

# Load the collection
s2_data <- p$load_collection(
  id = "AWS/SENTINEL-2-L2A",
  spatial_extent = list(
    west = -55.24475,
    east = -55.09232,
    north = -11.68720,
    south = -11.81628
  ),
  temporal_extent = list(
    "2018-09-01",
    "2019-08-31"
  ),
  bands = list("B04", "B08")
)

# Regularize the cube
s2_regularized <- p$cube_regularize(
  data = s2_data,
  period = "P1M",
  resolution = 320
)

# Export the cube
s2_export <- p$export_cube(
  data = s2_regularized,
  name = "s2_cube",
  folder = "test"
)


tempcnn_model_def <- p$mlm_class_tempcnn(
  cnn_layers = list(64, 64, 64),
  cnn_kernels = list(5, 5, 5),
  cnn_dropout_rates = list(0.2, 0.2, 0.2),
  dense_layer_nodes = 256,
  dense_layer_dropout_rate = 0.5,
  optimizer = "adam",
  learning_rate = 0.0005,
  epsilon = 0.00000001,
  weight_decay = 0.000001,
  lr_decay_epochs = 1,
  lr_decay_rate = 0.95,
  epochs = 150,
  batch_size = 64,
  random_state = 42
)

# Fit the model using the training dataset
tempcnn_model_fitted <- p$ml_fit(
  model = tempcnn_model_def,
  training_set = jsonlite::serializeJSON(sits::samples_modis_ndvi),
  target = "label"
)


# Export the trained model
tempcnn_model <- p$export_model(
  model = tempcnn_model_fitted,
  name = "tempcnn_model_12_24",
  folder = "openeocraft-models"
)

# Run the job
job <- create_job(
  graph = tempcnn_model,
  title = "Train Temp CNN Model",
  description = "Training a Temp CNN model and exporting it"
)
job <- start_job(job)

# Display job information
job
describe_job(job)
