#!/usr/bin/env Rscript
# Install torch Lantern during Docker build with retries for flaky CI downloads.

Sys.setenv(TORCH_INSTALL = "1", CUDA = "cpu")

if (!requireNamespace("torch", quietly = TRUE)) {
  install.packages("torch", repos = "https://cloud.r-project.org")
}

max_attempts <- 3L
ok <- FALSE
for (attempt in seq_len(max_attempts)) {
  ok <- tryCatch(
    {
      torch::install_torch(timeout = 1800)
      t <- torch::torch_tensor(1L)
      stopifnot(as.numeric(t) == 1L)
      TRUE
    },
    error = function(e) {
      message("install_torch attempt ", attempt, " failed: ", conditionMessage(e))
      FALSE
    }
  )
  if (ok) break
  if (attempt < max_attempts) Sys.sleep(60)
}

if (!ok) {
  stop("install_torch failed after ", max_attempts, " attempts", call. = FALSE)
}

message("Lantern OK")
