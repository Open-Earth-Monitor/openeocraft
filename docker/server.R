
plumber_file <- system.file("sits/plumber.R", package = "openeocraft")
plumber::plumb(plumber_file) |>
  plumber::pr_run(port = 8000)
