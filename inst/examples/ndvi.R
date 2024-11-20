library(openeo)

con <- connect("http://127.0.0.1:8000", user = "rolf", password = "123456")
p <- processes()

run_job <- function(pg, title = "title", description = "description") {
  job <- create_job(
    graph = pg,
    title = title,
    description = description
  )
  job <- start_job(job)
  job
}

# Just run it once
aws_s2_reg <- p$export_cube(
  data = p$cube_regularize(
    data = p$load_collection(
      id = "AWS/SENTINEL-2-L2A",
      spatial_extent = list(
        west = 16.1,
        east = 16.6,
        north = 48.6,
        south = 47.2
      ),
      temporal_extent = list(
        "2018-07-01",
        "2018-10-01"
      ),
      bands = list(
        "B02",
        "B04",
        "B08"
      )
    ),
    period = "P1M",
    resolution = 320
  ),
  name = "s2_cube",
  folder = "/test"
)

job <- create_job(
  graph = aws_s2_reg,
  title = "Sentinel-2 AWS regularized",
  description = paste(
    "period: 2018-07-01/2018-10-01",
    "bands: B02, B04, B08",
    sep = "\n"
  )
)
job <- start_job(job)
res <- list_results(job)

# ---- example 2:  ----

library(openeo)

con <- connect("http://127.0.0.1:8000", user = "rolf", password = "123456")
p <- processes()

cube <- p$export_cube(
  p$load_result(
    id = "04df1eb88d43e6fe5ec355f7e17352c6"
  ),
  name = "s2_cube",
  folder = "recovered2"
)

job <- create_job(
  graph = cube,
  title = "Sentinel-2 AWS regularized",
  description = paste(
    "period: 2018-07-01/2018-10-01",
    "bands: B02, B04, B08",
    sep = "\n"
  )
)
job <- start_job(job)
res <- list_results(job)

# TODO not working -- get stuck!
openeo::compute_result(cube, output_file = "i_dont_know", format = "GTiff")


# ---- example 3 ----

library(openeo)
library(sits)


con <- connect("http://127.0.0.1:8000", user = "rolf", password = "123456")
p <- processes()

rf_model <- p$export_model(
  model = p$ml_fit(
    model = p$ml_random_forest(
      num_trees = 50,
      max_depth = NULL,
      random_state = 42
    ),
    training_set = sits::samples_modis_ndvi
  ),
  name = "rf_model",
  folder = "recovered"
)

job <- run_job(rf_model)

job

aws_s2_reg <- p$import_cube(
  name = "s2_cube",
  folder = "test"
)



# rf_model <- p$import_from_workspace(
#   name = "rf_model",
#   folder = "/models"
# )

cube <- p$export_cube(
  p$load_result(
    job_id = "04df1eb88d43e6fe5ec355f7e17352c6"
  ),
  name = "s2_cube",
  folder = "recovered"
)

probs_cube <- p$ml_predict(
  data = cube,
  model = rf_model
)

label_cube <- p$ml_label_class(
  data = probs_cube
)

lbl_job <- p$save_result(
  data = label_cube,
  format = "GTiff"
)

lbl_job <- create_job(
  graph = lbl_job,
  title = "This is my first job",
  description = "Computing NDVI using openeocraft"
)
lbl_job <- start_job(lbl_job)
res <- list_results(lbl_job)
res$assets
download_results(lbl_job, "~/Downloads/")
