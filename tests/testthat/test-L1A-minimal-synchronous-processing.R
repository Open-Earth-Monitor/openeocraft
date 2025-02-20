# Mock API object
api <- mock_create_openeo_v1()
token <- readLines(file(system.file("mock/token", package = "openeocraft")))
job_id <- NULL
mock_job <- jsonlite::read_json(system.file("mock/mock-job.json", package = "openeocraft"))
mock_badjob <- jsonlite::read_json(system.file("mock/mock-badjob.json", package = "openeocraft"))

test_that("POST /result	Is supported (with authentication)", {
  # Call without token
  req <- mock_req("/result", method = "POST")
  res <- mock_res()
  expect_error(api_result(api, req, res), "Token is missing")

  # Call with invalid token
  req <- mock_req("/result", HTTP_AUTHORIZATION = "Test", method = "POST")
  res <- mock_res()
  expect_error(api_result(api, req, res), "Invalid token")

  # Call with valid token
  req <- mock_req("/result", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22", method = "POST")
  res <- mock_res()
  expect_true(is.environment(api_result(api, req, res)))
})

test_that("POST /result > process	Only accepts valid process submissions", {
  req <- mock_req("/result", method = "POST", HTTP_AUTHORIZATION = token)
  req$body <- mock_badjob
  res <- mock_res()
  expect_error(api_job_create(api, req, res), "Invalid job information")
})

# POST /result	Returns HTTP status 200 on success with the data in the response body, HTTP status codes 4XX or 5XX on failure with valid response body

# POST /result > Content-Type header	Returns suitable media type for the requested file format (not always application/octet-stream) in the Content-Type header

# POST /result	Rejects processing if a payment is required.
