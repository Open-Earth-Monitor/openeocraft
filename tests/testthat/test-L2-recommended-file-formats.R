# Mock API object
api <- mock_create_openeo_v1()

# GDAL Supported Formats (Example Subset for Test)
gdal_formats <- c("GeoTIFF", "GeoJSON", "netCDF", "JSON")

test_that("GET /file_formats: File format names aligned with GDAL", {
  req <- mock_req("/file_formats", method = "GET")
  res <- mock_res()
  result <- api_file_formats(api, req, res)

  format_names <- unique(c(names(result$input), names(result$output)))
  for (format in format_names) {
    expect_true(tolower(format) %in% tolower(gdal_formats),
                info = paste("Format not aligned with GDAL:", format))
  }
})

test_that("GET /file_formats: Each format has a description", {
  req <- mock_req("/file_formats", method = "GET")
  res <- mock_res()
  result <- api_file_formats(api, req, res)

  has_valid_description <- function(description) {
    # TODO: improve this -- what is a valid description according to spec?
    # grepl("data", description, ignore.case = TRUE)
    nchar(description) > 10L
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
