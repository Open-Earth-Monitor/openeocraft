library(openeo)

con <- connect("http://127.0.0.1:8000")
p <- processes()
login("rolf", "123456")
list_collections()

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

compute_result(x, output_file = "~/ndvi.rds")

result <- readRDS("~/ndvi.rds")
result
plot(result)
