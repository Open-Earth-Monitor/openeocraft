library(openeo)
library(sits)

# Connect to the backend
con <- connect("http://127.0.0.1:8000", user = "brian", password = "123456")

# Access processes
p <- processes()

# Define a random forest model
rf_model_definition <- p$mlm_class_random_forest(
  num_trees = 100,
  seed = 42
)

# Load sits tibble Training data
data_deforestation_rondonia <- readRDS("./inst/examples/filtered_rondonia_data.rds")



# Fit the model using the training dataset
rf_model_fitted <- p$ml_fit(
  model = rf_model_definition,
  training_set = jsonlite::serializeJSON(data_deforestation_rondonia)
)

# Export the trained model
rf_model <- p$export_ml_model(
  model = rf_model_fitted,
  name = "rf_model_20_05_25",
  folder = "openeocraft-models"
)

# Run the job
job <- create_job(
  graph = rf_model,
  title = "Train Random Forest Model",
  description = "Training a Random Forest model and exporting it"
)
job <- start_job(job)

# Display job information
describe_job(job)
