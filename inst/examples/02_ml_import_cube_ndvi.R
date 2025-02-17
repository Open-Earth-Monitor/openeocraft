library(openeo)

con <- connect("http://127.0.0.1:8000", user = "rolf", password = "123456")
p <- processes()

s2_cube <- p$import_cube(
  name = "s2_cube",
  folder = "openeocraft-cubes"
)

s2_cube_ndvi <- p$ndvi(
  data = s2_cube,
  nir = "B08",
  red = "B04",
  target_band = "NDVI"
)

s2_reg_export <- p$export_cube(
  data = s2_cube_ndvi,
  name = "s2_cube",
  folder = "openeocraft-cubes"
)



# Run the job
job <- create_job(
  graph = s2_reg_export,
  title = "NDVI Calculation",
  description = "Calculate NDVI from Sentinel-2 data."
)
job <- start_job(job)

# Display job information
job
describe_job(job)
