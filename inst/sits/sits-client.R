library(openeo)
con <- connect("http://127.0.0.1:8001")
p <- processes()
login("rolf", "123456")

# ---- example of save_result for a model ----
my_samples <- readRDS("my_samples.rds")
rf_model <- p$sits_rfor(my_samples, num_trees = 100, mtry = NULL)
file <- p$save_result(rf_model)
model <- readRDS(file)

# ---- example of save_result for samples (tibble) ----
my_cube <- p$load_collection(
  id = "Sentinel-2",
  spatial_extent = list(
    west = 16.1,
    east = 16.6,
    north = 48.6,
    south = 47.2
  ),
  temporal_extent = list(
    "2018-01-01",
    "2018-02-01"
  ),
  bands = list(
    "B02",
    "B04",
    "B08"
  )
)
my_samples <- readRDS("my_samples.rds")
time_series_samples <- sits_get_data(
  cube = my_cube,
  samples = my_samples
)
file <- p$save_result(time_series_samples)
samples <- readRDS(file)

# ---- cube save_rasult ----
file <- p$save_result(my_cube)
cube <- readRDS(file)
sits_view(cube)

# download each file from the third tile
tile <- 3
for (x in cube$file_info[[tile]]$url)
  download.file(x, destfile = basename(x))
