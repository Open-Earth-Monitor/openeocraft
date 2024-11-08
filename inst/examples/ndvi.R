library(openeo)

con <- connect("http://127.0.0.1:8000", user = "rolf", password = "123456")
p <- processes()

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

aws_s2_reg <- p$export_cube(
  p$import_cube(
    name = "s2_cube",
    folder = "test"
  ),
  name = "s2_cube",
  folder = "test2"
)

cube <- p$export_cube(
  p$load_result(
    job_id = "04df1eb88d43e6fe5ec355f7e17352c6"
  ),
  name = "s2_cube",
  folder = "recovered"
)

# rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)
#
# data_dir <- system.file("extdata/raster/mod13q1", package = "sits")
# cube <- sits_cube(
#   source = "BDC",
#   collection = "MOD13Q1-6",
#   data_dir = data_dir
# )
#
# # classify a data cube
# probs_cube <- sits_classify(
#   data = cube,
#   ml_model = rf_model,
#   output_dir = tempdir(),
#   version = "ex_classify"
# )
#
# # label the probability cube
# label_cube <- sits_label_classification(
#   probs_cube,
#   output_dir = tempdir(),
#   version = "ex_classify"
# )



# p$ml_cross_validate_class_random_forest(
#   training_set = samples_modis_ndvi,
#   target = "label",
#   num_trees = 50,
#   folds = 5
# )
#
# p$ml_validate_class_random_forest(
#   training_set = samples_modis_ndvi,
#   target = "label",
#   num_trees = 50,
#   train_test_split = 0.8
# )

# brian p : modified to
# p$ml_validate(
#   training_set = samples_modis_ndvi,
#   target = "label",
#   num_trees = 50,
#   train_test_split = 0.8
# )


# rf_model <- p$export_to_workspace(
#   data = p$ml_fit_class_random_forest(
#     training_set = samples_modis_ndvi,
#     target = "label",
#     num_trees = 50,
#     random_state = 42
#   ),
#   name = "rf_model",
#   folder = "/models"
# )
#
# job <- create_job(
#   graph = rf_model,
#   title = "Sentinel-2 AWS regularized",
#   description = paste(
#     "period: 2018-07-01/2018-10-01",
#     "bands: B02, B04, B08",
#     sep = "\n"
#   )
# )
# job <- start_job(job)


samples_modis_ndvi

rf_model <- p$ml_fit_class_random_forest(
  training_set = samples_modis_ndvi,
  target = "label",
  num_trees = 50,
  random_state = 42
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
