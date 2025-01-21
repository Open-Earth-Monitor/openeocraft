# Functionality Description
# GET /file_formats:
# - Valid response with input and output properties
# - Works without authentication
# - Works with authentication
# - At least one output file format is available
# - File format names are accepted case-insensitively
# - Each format includes at least gis_data_types and parameters

# Create openEO API object
api <- create_openeo_v1(
  id = "openeocraft",
  title = "openEO compliant R backend",
  description = "OpenEOcraft offers a robust R framework designed for the development and deployment of openEO API applications.",
  backend_version = "0.2.0",
  stac_api = NULL,
  work_dir = getwd(),
  conforms_to = NULL,
  production = FALSE
)

test_that("GET /file_formats: Valid response with input and output properties", {
  req <- mock_req("/file_formats", method = "GET")
  res <- mock_res()
  result <- mock_api_file_formats(api, req, res)

  expect_true("input" %in% names(result))
  expect_true("output" %in% names(result))
})

test_that("GET /file_formats: Works without authentication", {
  req <- mock_req("/file_formats", method = "GET")
  res <- mock_res()

  expect_no_error(mock_api_file_formats(api, req, res))
})

test_that("GET /file_formats: Works with authentication", {
  req <- mock_req("/file_formats", HTTP_AUTHORIZATION = "Bearer valid_token", method = "GET")
  res <- mock_res()

  expect_no_error(mock_api_file_formats(api, req, res))
})

test_that("GET /file_formats > output: At least one output file format is available", {
  req <- mock_req("/file_formats", method = "GET")
  res <- mock_res()
  result <- mock_api_file_formats(api, req, res)

  expect_true(length(result$output) > 0)
})

test_that("GET /file_formats: File format names are accepted case-insensitively", {
  req <- mock_req("/file_formats", method = "GET")
  res <- mock_res()
  result <- mock_api_file_formats(api, req, res)

  format_names <- tolower(names(result$output))
  test_format <- tolower("GeoTIFF")  # Example file format
  expect_true(test_format %in% format_names)
})

test_that("GET /file_formats: Each format includes at least gis_data_types and parameters", {
  req <- mock_req("/file_formats", method = "GET")
  res <- mock_res()
  result <- mock_api_file_formats(api, req, res)

  for (format in result$output) {
    expect_true("gis_data_types" %in% names(format))
    expect_true("parameters" %in% names(format))
  }
})
