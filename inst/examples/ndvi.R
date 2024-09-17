library(openeo)

con <- connect("http://127.0.0.1:8000", user = "rolf", password = "123456")
login(user = "rolf", password = "123456")
p <- processes()

list_collections()

x <- p$save_result(
  data = p$load_collection(
    id = "AWS/SENTINEL-2-L2A",
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
  format = "GTiff"
)

list_file_formats()
job <- create_job(x, "This is my first job", "Computing NDVI using openeocraft")
job2 <- start_job(job)
res <- list_results(job2)
res$assets
download_results(job2, "~/Downloads/")

compute_result(x, output_file = "~/Downloads/ndvi.tar")

# library(terra)
# result <- terra::rast("~/ndvi.tif")
# plot(result)

library(stars)
result <- readRDS("~/ndvi.rds")
result
plot(result)
