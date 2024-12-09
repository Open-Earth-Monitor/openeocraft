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
  folder = "openeocraft-test"
)

job <- run_job(s2_reg, "S2 regularize and export", "Regularize and export Sentinel 2 data")

describe_job(job)
