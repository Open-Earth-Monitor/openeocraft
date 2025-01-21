library(openeo)
library(sits)

# Connect to the backend
con <- connect(
  host = "http://127.0.0.1:8000",
  user = "rolf",
  password = "123456"
)

# Access processes
p <- processes()

# Load sits tibble Training data
data_deforestation_rondonia <- readRDS("./inst/examples/filtered_rondonia_data.rds")

# Define the Temporal CNN model
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
  training_set = jsonlite::serializeJSON(data_deforestation_rondonia),
  target = "label"
)

# Export the trained model
tempcnn_model <- p$export_model(
  model = tempcnn_model_fitted,
  name = "tempcnn_model_15_01_25",
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
describe_job(job)
