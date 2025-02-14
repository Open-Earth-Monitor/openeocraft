# Mock API object
api <- mock_create_openeo_v1()
token <- "5aad11e1d49b880a4468e1b252944e22"
job_id <- NULL

# --- Job Status Tests ---

test_that("Job follows the correct status changes", {
  req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = token)
  req$body <- jsonlite::fromJSON('{"title": "Test Job", "process": {}}', simplifyDataFrame = FALSE)
  res <- mock_res()

  result <- api_job_create(api, req, res)
  job_id <<- res$getHeader("OpenEO-Identifier")

  job <- job_get(api, user = "mock_user", job_id = job_id)
  expect_equal(job$status, "created")

  job_upd_status(api, user = "mock_user", job_id = job_id, status = "running")
  job <- job_get(api, user = "mock_user", job_id = job_id)
  expect_equal(job$status, "running")

  job_upd_status(api, user = "mock_user", job_id = job_id, status = "finished")
  job <- job_get(api, user = "mock_user", job_id = job_id)
  expect_equal(job$status, "finished")
})

# --- Processing Implementation Tests ---

test_that("Batch job processing is implemented", {
  req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = token)
  req$body <- jsonlite::fromJSON('{"title": "Batch Processing Test", "process": {}}', simplifyDataFrame = FALSE)
  res <- mock_res()

  result <- api_job_create(api, req, res)
  job_id <<- res$getHeader("OpenEO-Identifier")

  job <- job_get(api, user = "mock_user", job_id = job_id)
  expect_true(!is.null(job))
})

test_that("Synchronous processing is implemented", {
  req <- mock_req("/process", method = "POST", HTTP_AUTHORIZATION = token)
  req$body <- jsonlite::fromJSON('{"process": {}}', simplifyDataFrame = FALSE)

  res <- mock_res()
  job_id <<- res$getHeader("OpenEO-Identifier")

  result <- openeocraft:::run_pgraph(api, req, user = "mock_user", job = job_id, pg = NULL)
  expect_true(is.null(result))
})

