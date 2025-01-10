# Functionality Description
# POST /result	Is supported (with authentication)
# POST /result > process	Only accepts valid process submissions
# POST /result	Returns HTTP status 200 on success with the data in the response body, HTTP status codes 4XX or 5XX on failure with valid response body
# POST /result > Content-Type header	Returns suitable media type for the requested file format (not always application/octet-stream) in the Content-Type header
# POST /result	Rejects processing if a payment is required.

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

test_that("POST /result	Is supported (with authentication)", {
  # Call without token
  req <- mock_req("/result", method = "POST")
  res <- mock_res()
  expect_error(api_result(api, req, res), "Token is missing")

  # Call with invalid token
  req <- mock_req("/result", HTTP_AUTHORIZATION = "Test", method = "POST")
  res <- mock_res()
  expect_error(api_result(api, req, res), "Invalid token")
})
