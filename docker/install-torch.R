#!/usr/bin/env Rscript
# Install torch Lantern during Docker build with retries for flaky CI downloads.
#
# Each install attempt and each verification runs in a fresh R process: once
# torch's namespace has been loaded without Lantern, the session can neither
# load a freshly installed Lantern nor retry install_torch() (locked bindings).
# So this parent process must never load the torch namespace itself.

Sys.setenv(TORCH_INSTALL = "1", CUDA = "cpu")

if (!nzchar(system.file(package = "torch"))) {
  install.packages("torch", repos = "https://cloud.r-project.org")
}

rscript <- file.path(R.home("bin"), "Rscript")

run_r <- function(expr) {
  status <- system2(rscript, c("-e", shQuote(expr)))
  identical(status, 0L)
}

# Loading torch with TORCH_INSTALL=1 already attempts the download; only call
# install_torch() explicitly if that auto-install did not succeed.
install_expr <- paste(
  "options(timeout = 1800);",
  "if (!torch::torch_is_installed()) torch::install_torch(timeout = 1800)"
)
verify_expr <- paste(
  "stopifnot(torch::torch_is_installed());",
  "stopifnot(as.numeric(torch::torch_tensor(1L)) == 1)"
)

max_attempts <- 3L
ok <- FALSE
for (attempt in seq_len(max_attempts)) {
  run_r(install_expr)
  ok <- run_r(verify_expr)
  if (ok) break
  message("install_torch attempt ", attempt, " failed")
  if (attempt < max_attempts) Sys.sleep(60)
}

if (!ok) {
  stop("install_torch failed after ", max_attempts, " attempts", call. = FALSE)
}

message("Lantern OK")
