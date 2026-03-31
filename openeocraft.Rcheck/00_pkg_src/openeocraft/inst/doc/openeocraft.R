## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# #* @openeo-process
# ndvi <- function(data, nir = "nir", red = "red", target_band = NULL) {
#   ## implementation using your cube library (sits, stars, terra, gdalcubes, …)
# }

## ----minimal------------------------------------------------------------------
library(openeocraft)

work_dir <- tempfile("oc_vignette")
dir.create(work_dir)

api <- create_openeo_v1(
  id = "vignette-mock",
  title = "Vignette mock backend",
  description = "Minimal processes for documentation",
  backend_version = as.character(utils::packageVersion("openeocraft")),
  stac_api = "1.0.0",
  work_dir = work_dir,
  production = FALSE
)

mock_processes <- system.file("mock/mock-processes.R", package = "openeocraft")
stopifnot(nzchar(mock_processes))

api <- load_processes(api, mock_processes)

## ----cleanup, include = FALSE-------------------------------------------------
unlink(work_dir, recursive = TRUE)

