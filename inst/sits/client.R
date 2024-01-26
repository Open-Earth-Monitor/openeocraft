library(openeo)
con <- connect("http://127.0.0.1:8001")
p <- processes()
login("rolf", "123456")
list_collections()

x <- p$save_result(
  data = p$reduce_dimension(
    data = p$reduce_dimension(
      data = p$load_collection(
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
      ),
      dimension = "bands",
      reducer = function(data, context) {
        p$multiply(
          x = 2.5,
          y = p$divide(
            x = p$subtract(
              x = p$array_element(data = data, label = "B08"),
              y = p$array_element(data = data, label = "B04")
            ),
            y = p$sum(
              data = list(
                1L,
                p$array_element(data = data, label = "B08"),
                p$multiply(x = 6L, y = p$array_element(data = data, label = "B04")),
                p$multiply(x = -7.5, y = p$array_element(data = data, label = "B02"))
              )
            )
          )
        )
      }
    ),
    dimension = "t",
    reducer = function(data, context) {
      p$min(data = data)
    }
  ),
  format = "rds"
)

x <- p$save_result(
  data = p$reduce_dimension(
    data = p$load_collection(
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
    ),
    dimension = "bands",
    reducer = function(data, context) {
      p$divide(
        x = p$subtract(
          x = p$array_element(data = data, label = "B08"),
          y = p$array_element(data = data, label = "B04")
        ),
        y = p$add(
          x = p$array_element(data = data, label = "B08"),
          y = p$array_element(data = data, label = "B04")
        )
      )
    }
  ),
  format = "rds"
)

compute_result(x, output_file = "example.rds")
result <- readRDS("~/example.rds")

# ---- example of save_result for a model ----
# my_samples <- readRDS("my_samples.rds")
# rf_model <- p$sits_rfor(my_samples, num_trees = 100, mtry = NULL)
# file <- p$save_result(rf_model)
# model <- readRDS(file)

# ---- example of save_result for samples (tibble) ----
# my_cube <- p$load_collection(
#   id = "Sentinel-2",
#   spatial_extent = list(
#     west = 16.1,
#     east = 16.6,
#     north = 48.6,
#     south = 47.2
#   ),
#   temporal_extent = list(
#     "2018-01-01",
#     "2018-02-01"
#   ),
#   bands = list(
#     "B02",
#     "B04",
#     "B08"
#   )
# )
# my_samples <- readRDS("my_samples.rds")
# time_series_samples <- sits_get_data(
#   cube = my_cube,
#   samples = my_samples
# )
# file <- p$save_result(time_series_samples)
# samples <- readRDS(file)

# ---- cube save_rasult ----
# file <- p$save_result(my_cube)
# cube <- readRDS(file)
# sits_view(cube)

# ---- download each file from the third tile ----
# tile <- 3
# for (x in cube$file_info[[tile]]$url)
#   download.file(x, destfile = basename(x))
