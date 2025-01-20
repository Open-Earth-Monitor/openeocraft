# Functionality Description
# GET /file_formats:
# - File format names aligned with GDAL
# - Each format has a description describing their structure and relation to data cubes

# GDAL Supported Formats (Example Subset for Test)
gdal_formats <- c("GeoTIFF", "GeoJSON", "netCDF", "JSON")

test_that("GET /file_formats: File format names aligned with GDAL", {
  req <- mock_req("/file_formats", method = "GET")
  res <- mock_res()
  result <- mock_api_file_formats(api, req, res)

  format_names <- c(names(result$input), names(result$output))
  for (format in format_names) {
    expect_true(format %in% gdal_formats, info = paste("Format not aligned with GDAL:", format))
  }
})

test_that("GET /file_formats: Each format has a description describing their structure and relation to data cubes", {
  req <- mock_req("/file_formats", method = "GET")
  res <- mock_res()
  result <- mock_api_file_formats(api, req, res)

  has_valid_description <- function(description) {
    grepl("data", description, ignore.case = TRUE)
  }

  for (format_name in names(result$input)) {
    description <- result$input[[format_name]]$description
    expect_true(!is.null(description) && has_valid_description(description),
                info = paste("Invalid or missing description for input format:", format_name))
  }

  for (format_name in names(result$output)) {
    description <- result$output[[format_name]]$description
    expect_true(!is.null(description) && has_valid_description(description),
                info = paste("Invalid or missing description for output format:", format_name))
  }
})
