# ---- example 1 - cube regularization  and export to workspace----

library(openeo)

con <- connect("http://127.0.0.1:8000", user = "brian", password = "123456")
p <- processes()

# Just run it once
s2_reg <- p$export_cube(
  data = p$cube_regularize(
    data = p$load_collection(
      id = "AWS/SENTINEL-2-L2A",
      spatial_extent = list(
        west = -63.63661, #  northern part of rondonia in Brazil
        east = -63.41824,
        north = -8.43097,
        south = -8.648347
      ),
      temporal_extent = list(
        "2022-01-01",
        "2022-12-31"
      ),
      bands = list(
        "B02",
        "B04",
        "B8A",
        "B08"
      )
    ),
    period = "P1M",
    resolution = 320
  ),
  name = "s2_cube",
  folder = "openeocraft-cubes"
)


# Run the job
job <- create_job(
  graph = s2_reg,
  title = "S2 regularize and export",
  description = "Regularize and export Sentinel 2 data"
)
job <- start_job(job)

describe_job(job)
