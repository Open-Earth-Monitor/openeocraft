library(openeo)
library(sits)

# Connect to the backend
con <- connect("http://127.0.0.1:8000", user = "brian", password = "123456")

# Access processes
p <- processes()

# Define a random forest model
rf_model_definition <- p$mlm_class_random_forest(
  num_trees = 100,
  random_state = 42
)

# Fit the model using the training dataset
rf_model_fitted <- p$ml_fit(
  model = rf_model_definition,
  training_set = jsonlite::serializeJSON(sits::samples_modis_ndvi)
)

# Export the trained model
rf_model <- p$export_model(
  model = rf_model_fitted,
  name = "rf_model_12_12_24",
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
job
describe_job(job)
