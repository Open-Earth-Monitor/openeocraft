plumber_file <- "./inst/sits/plumber.R"
plumber::plumb(plumber_file) |>
  plumber::pr_run(port = 8000, host = "0.0.0.0")
