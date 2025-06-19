# ---- example 1 - cube regularization  and export to workspace----

library(openeo)

con <- connect("http://127.0.0.1:8000",
               user = "brian",
               password = "123456")
p <- processes()

# Just run it once
s2_reg <- p$export_cube(
  data = p$cube_regularize(
    data = p$load_collection(
      id = "mpc-sentinel-2-l2a",
      spatial_extent = list(
        west = -63.9,  # rondonia area
        east = 	-62.9,
        south = -9.14,
        north = -8.14
      ),
      temporal_extent = list("2022-01-01", "2022-12-31"),
      bands = list("B02","B03", "B04","B05","B06", "B07", "B08", "B11", "B12","B8A")
    ),
    period = "P1M",
    resolution = 320
  ),
  name = "s2_cube",
  folder = "openeocraft-cubes-1M"
)


# Run the job
job <- create_job(graph = s2_reg,
                  title = "S2 regularize and export",
                  description = "Regularize and export Sentinel 2 data")
job <- start_job(job)

describe_job(job)
