library(sits)
# Example of classification of a time series
# Retrieve the samples for Mato Grosso
# train a random forest model
sits_timeline(samples_modis_ndvi)
rf_model <- sits_train(samples_modis_ndvi, ml_method = sits_rfor)

# classify the point
point_ndvi <- sits_select(point_mt_6bands, bands = c("NDVI"))

# Example of classification of a data cube
# create a data cube from local files
s2_cube <- sits_cube(
  source = "MPC",
  collection = "SENTINEL-2-L2A",
  tiles = "20LKP",
  bands = c("B04", "B08", "CLOUD"),
  start_date = "2018-07-18",
  end_date = "2019-01-23"
)
s2_cube_reg <- sits_regularize(
  cube = s2_cube,
  period = "P16D",
  res = 320,
  output_dir = "~/sits-tests",
  multicores = 2L
)
s2_ndvi <- sits_apply(
  data = s2_cube_reg,
  NDVI = (B08 - B04) / (B08 + B04),
  memsize = 2L,
  multicores = 2L,
  output_dir = "~/sits-tests"
)
sits_bands(s2_ndvi)
sits_timeline(s2_ndvi)
plot(s2_ndvi, band = "NDVI")
# classify a data cube
probs_cube <- sits_classify(
  data = s2_ndvi,
  ml_model = rf_model,
  output_dir = "~/sits-tests",
  version = "ex_classify"
)
plot(probs_cube)
# label the probability cube
label_cube <- sits_label_classification(
  probs_cube,
  output_dir = "~/sits-tests",
  version = "ex_classify"
)
# plot the classified image
plot(label_cube)
