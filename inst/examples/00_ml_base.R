# ---- helper functions ----

run_job <- function(pg, title = "title", description = "description") {
  job <- create_job(
    graph = pg,
    title = title,
    description = description
  )
  job <- start_job(job)
  job
}


# ---- example 1 - cube regularization  ----

library(openeo)

con <- connect("http://127.0.0.1:8000", user = "rolf", password = "123456")
p <- processes()

# Just run it once
s2_reg <- p$export_cube(
  data = p$cube_regularize(
    data = p$load_collection(
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
      bands = list(
        "B04",
        "B08"
      )
    ),
    period = "P1M",
    resolution = 320
  ),
  name = "s2_cube",
  folder = "test"
)

job <- run_job(s2_reg)

job
# list_results(job)

# ---- example 2 - compute NDVI ----

library(openeo)

con <- connect("http://127.0.0.1:8000", user = "rolf", password = "123456")
p <- processes()

s2_reg <- p$export_cube(
  data = p$ndvi(
    data = p$import_cube(
      name = "s2_cube",
      folder = "test"
    ),
    nir = "B08",
    red = "B04",
    target_band = "NDVI"
  ),
  name = "s2_cube",
  folder = "test"
)

job <- run_job(s2_reg)

job

# ---- example 3 - training RF ----

library(openeo)
library(sits)

con <- connect("http://127.0.0.1:8000", user = "rolf", password = "123456")
p <- processes()

rf_model <- p$export_model(
  model = p$ml_fit(
    model = p$mlm_class_random_forest(
      num_trees = 50,
      max_variables = "sqrt",
      random_state = 42
    ),
    training_set = jsonlite::serializeJSON(sits::samples_modis_ndvi)
  ),
  name = "rf_model",
  folder = "test"
)

job <- run_job(rf_model)

job

# ---- example 4 - classification ----

library(openeo)
library(sits)

con <- connect("http://127.0.0.1:8000", user = "rolf", password = "123456")
p <- processes()

labels_cube <- p$export_cube(
  data = p$ml_predict(
    data = p$import_cube(
      name = "s2_cube",
      folder = "test"
    ),
    model = p$import_model(
      name = "rf_model",
      folder = "test"
    )
  ),
  name = "labels_cube",
  folder = "test2"
)

job <- run_job(labels_cube)

job

x <- readRDS("/home/rolf/openeo-tests/workspace/rolf/root/test/.obj/labels_cube.rds")

library(sits)
sits_view(x)

# ---- other ----


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
# ---- hard problem to be fixed  ----

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
    "bands: B04, B08",
    sep = "\n"
  )
)
job <- start_job(job)
res <- list_results(job)

# TODO not working -- get stuck!
openeo::compute_result(cube, output_file = "i_dont_know", format = "GTiff")


