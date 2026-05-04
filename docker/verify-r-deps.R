#!/usr/bin/env Rscript
# Fail the Docker build if expected R packages are not loadable.
# Keep this list aligned with docker/Dockerfile install steps.

pkgs <- c(
  "gdalcubes", "plumber", "useful", "s2", "sf", "rstac", "geojsonsf",
  "jsonlite", "base64enc", "ids", "callr", "sits",
  "torch",
  "openstac", "openeocraft",
  "swagger"
)

ok <- vapply(pkgs, function(p) requireNamespace(p, quietly = TRUE), logical(1))
if (!all(ok)) {
  missing <- pkgs[!ok]
  stop(
    "Docker R deps incomplete. Missing: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

message("Docker R dependency check OK (", length(pkgs), " packages).")
