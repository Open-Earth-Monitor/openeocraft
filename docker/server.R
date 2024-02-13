print(installed.packages()[, 'Package'])

# build and install package locally (use for development)
#remotes::install_github("Open-Earth-Monitor/openeocraft", dependencies = TRUE, force = TRUE)

# check installed packages
#print(installed.packages()[, 'Package'])

library(openeocraft)

plumber_file <- system.file("sits/plumber.R", package = "openeocraft")
message(plumber_file)
plumber::plumb(plumber_file) |>
  plumber::pr_run(port = 8000, host = "0.0.0.0")
