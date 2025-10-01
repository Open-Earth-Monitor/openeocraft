library(openeo)

con <- connect("http://127.0.0.1:8000", user = "brian", password = "123456")
p <- processes()


s2_cube <- p$import_cube(
    name = "s2_cube",
    folder = "openeocraft-cubes"
)

rf_model <- p$import_ml_model(
    name = "rf_model_20_05_25",
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
describe_job(job)
