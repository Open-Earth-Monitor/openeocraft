# Mock API object
api <- mock_create_openeo_v1()

mock_job <- '{
  "title": "NDVI based on Sentinel 2",
  "description": "Deriving minimum NDVI measurements over pixel time series of Sentinel 2",
  "process": {
    "id": "ndvi",
    "summary": "string",
    "description": "string",
    "parameters": [],
    "returns": {},
    "categories": [
      "string"
    ],
    "deprecated": false,
    "process_graph": {}
  },
  "log_level": "warning"
}'

# --- GET /jobs ---
test_that("GET /jobs: Is supported (with authentication)", {
  req <- mock_req("/jobs", method = "GET", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  res <- mock_res()

  result <- api_jobs_list(api, req, res)
  expect_true(is.list(result))
})

test_that("GET /jobs: Returns a valid response with at least jobs and links as arrays", {
  req <- mock_req("/jobs", method = "GET", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  res <- mock_res()

  result <- api_jobs_list(api, req, res)

  expect_true("jobs" %in% names(result))
  expect_true("links" %in% names(result))
  expect_type(result$jobs, "list")
  expect_type(result$links, "list")
})

test_that("GET /jobs > limit parameter: Returns all jobs if no limit parameter is provided", {
  req <- mock_req("/jobs", method = "GET", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  res <- mock_res()

  result <- api_jobs_list(api, req, res)
  expect_gte(length(result$jobs), 1)
})

test_that("GET /jobs > jobs: Serves valid job metadata (id, status, created required)", {
  req <- mock_req("/jobs", method = "GET", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  res <- mock_res()

  result <- api_jobs_list(api, req, res)

  for (job in result$jobs) {
    expect_true(all(c("id", "status", "created") %in% names(job)))
  }
})

test_that("GET /jobs > jobs: Includes title for all jobs (if provided by the user)", {
  req <- mock_req("/jobs", method = "GET", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  res <- mock_res()

  result <- api_jobs_list(api, req, res)

  for (job in result$jobs) {
    if (!is.null(job$title)) {
      expect_type(job$title, "character")
    }
  }
})

# --- POST /jobs ---
test_that("POST /jobs: Supports creating batch jobs (with authentication)", {
  req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  req$body <- jsonlite::fromJSON(mock_job, simplifyDataFrame = FALSE)
  res <- mock_res()

  result <- api_job_create(api, req, res)
  expect_true(is.list(result))
})

test_that("POST /jobs > process: Only accepts valid process submissions", {
  req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  req$body <- jsonlite::fromJSON(mock_job, simplifyDataFrame = FALSE)
  res <- mock_res()

  result <- api_job_create(api, req, res)
  expect_true(is.list(result))
})

test_that("POST /jobs: Supports storing title and description", {
  req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  req$body <- jsonlite::fromJSON(mock_job, simplifyDataFrame = FALSE)
  res <- mock_res()

  result <- api_job_create(api, req, res) # this function returns an empty list, i need to get stuff from res e.g. res has job id.
  job_id = res$getHeader("OpenEO-Identifier")
  job = job_get(api, user = "mock_user", job_id = job_id)
  expect_equal(job$title, "NDVI based on Sentinel 2")
  expect_equal(job$description, "Deriving minimum NDVI measurements over pixel time series of Sentinel 2")
})

test_that("POST /jobs: Returns HTTP status 201 and OpenEO-Identifier + Location header if successful", {
  req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  req$body <- jsonlite::fromJSON(mock_job, simplifyDataFrame = FALSE)
  res <- mock_res()

  result <- api_job_create(api, req, res)
  expect_equal(res$status, 201)
  expect_true(!is.null(res$getHeader("OpenEO-Identifier")))
  expect_true(!is.null(res$getHeader("Location")))
})

test_that("POST /jobs > status: Sets job status to 'created' after successful creation", {
  req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")

  req$body <- jsonlite::fromJSON(mock_job, simplifyDataFrame = FALSE)
  res <- mock_res()

  result <- api_job_create(api, req, res)
  job_id = res$getHeader("OpenEO-Identifier")
  job = job_get(api, user = "mock_user", job_id = job_id)
  expect_equal(job$status, "created")
})

# --- GET /jobs/{id} ---
test_that("GET /jobs/{id}: Is supported (with authentication)", {
  job_id = "job-20250207"
  req <- mock_req(paste("/jobs", job_id, sep = "/"), method = "GET", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  res <- mock_res()

  result <- api_job_info(api, req, res, job_id)
  expect_equal(res$status, 200)
})

test_that("GET /jobs/{id}: Returns at least id, process, status, and created", {
  req <- mock_req("/jobs/job-20250207", method = "GET", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  res <- mock_res()

  result <- api_job_info(api, req, res, "job-20250207")
  expect_true(all(c("id", "title","process" , "status", "created") %in% names(result)))
})

test_that("GET /jobs/{id}: Includes title and description if provided by the user", {
  req <- mock_req("/jobs/job-20250207", method = "GET", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  res <- mock_res()

  result <- api_job_info(api, req, res, "job-20250207")

  if (!is.null(result$title)) {
    expect_type(result$title, "character")
  }
  if (!is.null(result$description)) {
    expect_type(result$description, "character")
  }
})

test_that("GET /jobs/{id} > status: Correctly reports status (and progress if supported)", {

  req <- mock_req("/jobs/job-20250207", method = "GET", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  res <- mock_res()

  result <- api_job_info(api, req, res, "job-20250207")

  expect_true(result$status %in% c("created", "queued", "running", "finished"))
})

# --- DELETE /jobs/{id} ---
test_that("DELETE /jobs/{id}: Is supported (with authentication)", {
  # req <- mock_req("/jobs/job-20250207", method = "DELETE", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  # res <- mock_res()

  # result <- job_delete(api, user = "mock_user", job_id = "job-20250207")
  #expect_equal(result$message, "Job deleted")
})

test_that("DELETE /jobs/{id}: Deletes the job and returns HTTP 204 on success, or 4XX otherwise", {
  # req <- mock_req("/jobs/job-20250207", method = "DELETE", HTTP_AUTHORIZATION = "5aad11e1d49b880a4468e1b252944e22")
  # res <- mock_res()
  # result <- job_delete(api, user = "mock_user", job_id = "job-20250207")
  # res_not_found <- mock_res()
  # result_not_found <- job_delete(api, user = "mock_user", job_id = "nonexistent-job")
  # expect_equal(result$message, "Job deleted")
  # expect_equal(result_not_found$status, 404)
})
