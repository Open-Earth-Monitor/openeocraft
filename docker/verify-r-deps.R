#!/usr/bin/env Rscript
# Fail the Docker build if expected R packages are not loadable.
# Keep this list aligned with docker/Dockerfile install steps.

pkgs <- c(
  "gdalcubes", "plumber", "useful", "s2", "sf", "rstac", "geojsonsf",
  "jsonlite", "base64enc", "ids", "callr", "sits", "caret",
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

# requireNamespace("torch") succeeds even when the Lantern C binary is absent —
# it only loads the R namespace. Verify Lantern is actually present by calling
# a real tensor operation, which triggers the binary load.
message("Verifying torch Lantern binary loads...")
tryCatch({
  t <- torch::torch_tensor(1L)
  stopifnot(as.numeric(t) == 1L)
  message("torch Lantern OK.")
}, error = function(e) {
  stop(
    "torch Lantern binary failed to load: ", conditionMessage(e),
    "\nRun install_torch() to install the missing binary.",
    call. = FALSE
  )
})
