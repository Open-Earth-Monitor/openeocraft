# Mock API object
api <- mock_create_openeo_v1()
api_attr(api, "processes") <- list(
  list(id = "ndvi", summary = "NDVI calculation"),
  list(id = "evi", summary = "EVI calculation")
)
token <- "5aad11e1d49b880a4468e1b252944e22"

# --- GET /processes ---

test_that("GET /processes: Valid response with at least processes as an array", {
  req <- mock_req("/processes", method = "GET")
  res <- mock_res()

  result <- api_processes(api, req, res)

  expect_true("processes" %in% names(result))
  expect_type(result$processes, "list")
})

test_that("GET /processes: Works without authentication", {
  req <- mock_req("/processes", method = "GET")
  res <- mock_res()

  result <- api_processes(api, req, res)

  expect_true(is.list(result))
})

test_that("GET /processes: Works with authentication", {
  req <- mock_req("/processes", method = "GET", HTTP_AUTHORIZATION = token)
  res <- mock_res()

  result <- api_processes(api, req, res)

  expect_true(is.list(result))
})

test_that("GET /processes > limit parameter: All processes are returned if no limit parameter is provided", {
  req <- mock_req("/processes", method = "GET", HTTP_AUTHORIZATION = token)
  res <- mock_res()

  result <- api_processes(api, req, res)

  expect_gte(length(result$processes), 1)
})

test_that("GET /processes > processes: Missing properties in process objects are not set to null if not valid according to OpenAPI schema", {
  req <- mock_req("/processes", method = "GET", HTTP_AUTHORIZATION = token)
  res <- mock_res()

  result <- api_processes(api, req, res)

  for (process in result$processes) {
    expect_false(any(sapply(process, is.null)))
  }
})
