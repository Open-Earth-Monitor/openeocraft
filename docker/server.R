# TO-DO: show R errors into terminal
plumber_file <- "docker/plumber.R"
plumber::plumb(plumber_file) |>
  plumber::pr_set_debug(TRUE) |>
  plumber::pr_run(port = 8000, host = "0.0.0.0")
