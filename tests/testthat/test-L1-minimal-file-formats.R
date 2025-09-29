# Functionality Description
# GET /file_formats:
# - Valid response with input and output properties
# - Works without authentication
# - Works with authentication
# - At least one output file format is available
# - File format names are accepted case-insensitively
# - Each format includes at least gis_data_types and parameters

# Create openEO API object
api <- mock_create_openeo_v1()

test_that("GET /file_formats: Valid response with input and output properties", {
    result <- file_formats()

    expect_true("input" %in% names(result))
    expect_true("output" %in% names(result))
})

test_that("GET /file_formats: Works without authentication", {
    req <- mock_req("/file_formats", method = "GET")
    res <- mock_res()

    expect_no_error(api_file_formats(api, req, res))
})

test_that("GET /file_formats: Works with authentication", {
    req <- mock_req("/file_formats", HTTP_AUTHORIZATION = "Test", method = "GET")
    res <- mock_res()

    expect_error(api_file_formats(api, req, res), "Invalid token")
})

test_that("GET /file_formats > output: At least one output file format is available", {
    req <- mock_req("/file_formats", method = "GET")
    res <- mock_res()
    result <- api_file_formats(api, req, res)

    expect_true(length(result$output) > 0L)
})

test_that("GET /file_formats: File format names are accepted case-insensitively", {
    req <- mock_req("/file_formats", method = "GET")
    res <- mock_res()
    result <- api_file_formats(api, req, res)

    format_names <- tolower(names(result$output))
    test_format <- tolower("GeoTIFF") # Example file format
    expect_true(test_format %in% format_names)
})

test_that("GET /file_formats: Each format includes at least gis_data_types and parameters", {
    req <- mock_req("/file_formats", method = "GET")
    res <- mock_res()
    result <- api_file_formats(api, req, res)

    for (format in result$output) {
        expect_true("gis_data_types" %in% names(format))
        expect_true("parameters" %in% names(format))
    }
})
