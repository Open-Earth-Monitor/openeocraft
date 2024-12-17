library(openeo)
library(sits)

con <- connect("http://127.0.0.1:8000", user = "brian", password = "123456")
p <- processes()



s2_cube <- p$import_cube(
  name = "s2_cube",
  folder = "openeocraft-cubes"
)

rf_model <- p$import_model(
  name = "rf_model_12_12_24",
  folder = "openeocraft-models"
)

data <- p$ml_predict(
  data = s2_cube,
  model = rf_model
)

labels_cube <- p$export_cube(
  data = data,
  name = "labels_cube",
  folder = "openeocraft-cubes-pred"
)


# Run the job
job <- create_job(
  graph = labels_cube,
  title = "Labeling",
  description = "Labeling the cube classified with the Random Forest model."
)
job <- start_job(job)

# Display job information
job
describe_job(job)

