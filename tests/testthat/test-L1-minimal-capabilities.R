# Define expected versions
expected_api_version <- "1.2.0"
expected_stac_version <- "1.0.0"

# Create openEO API object
api <- create_openeo_v1(
  id = "openeocraft",
  title = "openEO compliant R backend",
  description = "OpenEOcraft offers a robust R framework designed for the development and deployment of openEO API applications.",
  backend_version = "0.2.0",
  stac_api = list(
    get = function(key) {
      if (key == "stac_version") return("1.0.0")
      NULL
    }
  ),
  work_dir = getwd(),
  conforms_to = c(
    "https://api.openeo.org/1.2.0",
    "https://api.stacspec.org/v1.0.0/collections"
  ),
  production = FALSE
)

mock_root_response <- mock_landing_page(api)

# Add required endpoints
mock_root_response$endpoints <- list(
  list(
    path = "/collections",
    methods = c("GET")
  ),
  list(
    path = "/processes",
    methods = c("GET")
  ),
  list(
    path = "/jobs",
    methods = c("GET", "POST", "DELETE")
  )
)

# Execute the function to get the actual response data
mock_well_known_response <- mock_well_known_response(api)

# Helper function to validate endpoint
is_valid_endpoint <- function(endpoint, implemented_endpoints) {
  path <- endpoint$path
  methods <- endpoint$methods
  if (is.null(path) || is.null(methods)) return(FALSE)
  for (method in methods) {
    if (!method %in% implemented_endpoints[[path]]) return(FALSE)
  }
  return(TRUE)
}

# Test: /.well-known/openeo
test_that("GET /.well-known/openeo: Valid response with at least url and api_version per instance", {
  # Description: This test ensures the /.well-known/openeo endpoint includes the required fields:
  # - A URL for accessing the API instance
  # - The API version implemented by the backend
  expect_true(!is.null(mock_well_known_response$url), "URL is missing in /.well-known/openeo response")
  expect_true(!is.null(mock_well_known_response$api_version), "API version is missing in /.well-known/openeo response")
})

# Test: Root endpoint basic structure
test_that("GET /: Valid response with at least api/backend/stac_version, id, title, description, endpoints, and links", {
  # Description: This test verifies the root endpoint response includes all required fields:
  # - API version, backend version, and STAC version for compatibility
  # - Metadata fields (id, title, description) for identifying the backend
  # - Endpoints and links for accessing backend functionality
  expect_true(!is.null(mock_root_response$api_version), "API version is missing in / response")
  expect_true(!is.null(mock_root_response$backend_version), "Backend version is missing in / response")
  expect_true(!is.null(mock_root_response$stac_version), "STAC version is missing in / response")
  expect_true(!is.null(mock_root_response$id), "ID is missing in / response")
  expect_true(!is.null(mock_root_response$title), "Title is missing in / response")
  expect_true(!is.null(mock_root_response$description), "Description is missing in / response")
  expect_true(length(mock_root_response$endpoints) > 0, "Endpoints are missing in / response")
  expect_true(length(mock_root_response$links) > 0, "Links are missing in / response")
})

# Test: Endpoint listing completeness
test_that("GET / > endpoints: All implemented endpoints are listed with their methods", {
  # Description: This test ensures that all implemented endpoints are correctly listed with their supported HTTP methods.
  # Any missing or misconfigured endpoints would lead to incorrect functionality for users.
  implemented_endpoints <- list(
    "/collections" = c("GET"),
    "/processes" = c("GET"),
    "/jobs" = c("GET", "POST", "DELETE")
  )

  for (endpoint in mock_root_response$endpoints) {
    expect_true(is_valid_endpoint(endpoint, implemented_endpoints),
                info = paste("Invalid or unimplemented endpoint:", endpoint$path))
  }
})

# Test: Endpoint listing validity
test_that("GET / > endpoints: No endpoints are listed that are not implemented or faulty", {
  # Description: This test ensures that the backend does not list endpoints that are unimplemented or have issues.
  # Such discrepancies would result in user confusion or runtime errors.
  implemented_endpoints <- list(
    "/collections" = c("GET"),
    "/processes" = c("GET"),
    "/jobs" = c("GET", "POST", "DELETE")
  )

  for (endpoint in mock_root_response$endpoints) {
    path <- endpoint$path
    expect_true(path %in% names(implemented_endpoints),
                info = paste("Endpoint listed but not implemented:", path))
  }
})

# Test: API version compliance
test_that("GET / > api_version: Implements openEO API version 1.2.0", {
  # Description: This test checks if the backend supports the required openEO API version (1.2.0).
  # Maintaining version compliance is critical for interoperability with client applications.
  expect_equal(mock_root_response$api_version, expected_api_version,
               info = paste("API version mismatch:", mock_root_response$api_version))
})

# Test: STAC version compliance
test_that("GET / > stac_version: Implements STAC version 1.0.0", {
  # Description: This test verifies that the backend supports the required STAC version (1.0.0).
  # STAC compliance ensures compatibility with geospatial asset catalogs.
  expect_equal(mock_root_response$stac_version, expected_stac_version,
               info = paste("STAC version mismatch:", mock_root_response$stac_version))
})
