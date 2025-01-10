# Functionality Description
# POST /result	Is supported (with authentication)
# POST /result > process	Only accepts valid process submissions
# POST /result	Returns HTTP status 200 on success with the data in the response body, HTTP status codes 4XX or 5XX on failure with valid response body
# POST /result > Content-Type header	Returns suitable media type for the requested file format (not always application/octet-stream) in the Content-Type header
# POST /result	Rejects processing if a payment is required.

# Create an STAC server API object
stac_api <- openstac::create_stac(
  id = "openlandmap",
  title = "OpenLandMap STAC API",
  description = paste(
    "Spatio-Temporal Asset Catalog for global layers provided ",
    "by OpenLandMap and maintaned by OpenGeoHub Foundation"
  ),
  conforms_to = NULL
)

# Set API database
file <- system.file("ml/db.rds", package = "openeocraft")
stac_api <- openstac::set_db(stac_api, driver = "local", file = file)

# Create openEO API object
api <- create_openeo_v1(
  id = "openeocraft",
  title = "openEO compliant R backend",
  description = "OpenEOcraft offers a robust R framework designed for the development and deployment of openEO API applications.",
  backend_version = "0.2.0",
  stac_api = stac_api,
  work_dir = "~/openeo-tests",
  conforms_to = NULL,
  production = FALSE
)

# Set the credentials
set_credentials(api, file = "~/openeo-credentials.rds")

# Load processes
processes_file <- system.file("ml/processes.R", package = "openeocraft")
load_processes(api, processes_file)

test_that("POST /result	Is supported (with authentication)", {
  req <- mock_req("/result", method = "POST")
  res <- mock_res()
  testthat::expect_error(api_result(api, req, res), "Token is missing")
  req <- mock_req("/result", HTTP_AUTHORIZATION = "Test", method = "POST")
  res <- mock_res()
  testthat::expect_error(api_result(api, req, res), "Invalid token")
})
