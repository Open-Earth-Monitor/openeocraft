# Mock API object
api <- mock_create_openeo_v1()
token <- readLines(file(system.file("mock/token", package = "openeocraft")))
job_id <- NULL
mock_job <- jsonlite::read_json(
    system.file("mock/mock-job.json", package = "openeocraft")
)
mock_badjob <- jsonlite::read_json(
    system.file("mock/mock-badjob.json", package = "openeocraft")
)

test_that("POST /jobs: Supports creating batch jobs (with authentication)", {
    req <- mock_req(
        "/jobs",
        method = "POST", HTTP_AUTHORIZATION = "invalid_token"
    )
    req$body <- mock_job
    res <- mock_res()
    expect_error(api_job_create(api, req, res))

    req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = token)
    req$body <- mock_job
    res <- mock_res()
    result <- api_job_create(api, req, res)
    expect_true(is.character(res$getHeader("Location")))
    expect_true(is.character(res$getHeader("OpenEO-Identifier")))
    expect_true(is.list(result))

    job_id <<- res$getHeader("OpenEO-Identifier")
})

test_that("POST /jobs > process: Only accepts valid process submissions", {
    req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = token)
    req$body <- mock_badjob
    res <- mock_res()
    expect_error(api_job_create(api, req, res), "Invalid job information")
})

test_that("POST /jobs: Supports storing title and description", {
    req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = token)
    req$body <- mock_job
    res <- mock_res()

    result <- api_job_create(api, req, res)
    job_id <- res$getHeader("OpenEO-Identifier")
    job <- job_get(api, user = "mock_user", job_id = job_id)
    expect_equal(job$title, "NDVI based on Sentinel 2")
    expect_equal(
        job$description,
        paste0(
            "Deriving minimum NDVI measurements over ",
            "pixel time series of Sentinel 2"
        )
    )
})

test_that(paste0(
    "POST /jobs: Returns HTTP status 201 and ",
    "OpenEO-Identifier + Location header if successful"
), {
    req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = token)
    req$body <- mock_job
    res <- mock_res()

    result <- api_job_create(api, req, res)
    expect_equal(res$status, 201L)
    expect_true(!is.null(res$getHeader("OpenEO-Identifier")))
    expect_true(!is.null(res$getHeader("Location")))
})

test_that(paste0(
    "POST /jobs > status: Sets job status to 'created'",
    " after successful creation"
), {
    req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = token)

    req$body <- mock_job
    res <- mock_res()

    result <- api_job_create(api, req, res)
    job_id <- res$getHeader("OpenEO-Identifier")
    job <- job_get(api, user = "mock_user", job_id = job_id)
    expect_equal(job$status, "created")
})

test_that("GET /jobs: Is supported (with authentication)", {
    req <- mock_req("/jobs", method = "GET", HTTP_AUTHORIZATION = token)
    res <- mock_res()

    result <- api_jobs_list(api, req, res)
    expect_true(is.list(result))
})

test_that(paste0(
    "GET /jobs: Returns a valid response with at ",
    "least jobs and links as arrays"
), {
    req <- mock_req("/jobs", method = "GET", HTTP_AUTHORIZATION = token)
    res <- mock_res()

    result <- api_jobs_list(api, req, res)

    expect_true("jobs" %in% names(result))
    expect_true("links" %in% names(result))
    expect_type(result$jobs, "list")
    expect_type(result$links, "list")
})

test_that(paste0(
    "GET /jobs > limit parameter: Returns all jobs if ",
    "no limit parameter is provided"
), {
    req <- mock_req("/jobs", method = "GET", HTTP_AUTHORIZATION = token)
    res <- mock_res()

    result <- api_jobs_list(api, req, res)
    expect_gte(length(result$jobs), 1)

    # TODO: implement limit test
})

test_that(paste0(
    "GET /jobs > jobs: Serves valid job metadata ",
    "(id, status, created required)"
), {
    req <- mock_req("/jobs", method = "GET", HTTP_AUTHORIZATION = token)
    res <- mock_res()

    result <- api_jobs_list(api, req, res)

    for (job in result$jobs) {
        expect_true(all(c("id", "status", "created") %in% names(job)))
    }
})

test_that(paste0(
    "GET /jobs > jobs: Includes title for all jobs (if provided by the user)"
), {
    req <- mock_req("/jobs", method = "GET", HTTP_AUTHORIZATION = token)
    res <- mock_res()

    result <- api_jobs_list(api, req, res)

    for (job in result$jobs) {
        if (!is.null(job$title)) {
            expect_type(job$title, "character")
        }
    }
})

test_that("GET /jobs/{id}: Is supported (with authentication)", {
    req <- mock_req(
        paste("/jobs", job_id, sep = "/"),
        method = "GET", HTTP_AUTHORIZATION = token
    )
    res <- mock_res()
    result <- api_job_info(api, req, res, job_id)
    expect_equal(res$status, 200L)

    req <- mock_req(
        paste("/jobs", job_id, sep = "/"),
        method = "GET", HTTP_AUTHORIZATION = "invalid_token"
    )
    res <- mock_res()
    expect_error(api_job_info(api, req, res, job_id), "Invalid token")
})

test_that(paste0(
    "GET /jobs/{id}: Returns at least id, process, status, and created"
), {
    req <- mock_req(
        paste("/jobs", job_id, sep = "/"),
        method = "GET", HTTP_AUTHORIZATION = token
    )
    res <- mock_res()
    result <- api_job_info(api, req, res, job_id)
    expect_true(
        all(c("id", "title", "process", "status", "created") %in% names(result))
    )
})

test_that(paste0(
    "GET /jobs/{id}: Includes title and description if provided by the user"
), {
    req <- mock_req(
        paste("/jobs", job_id, sep = "/"),
        method = "GET", HTTP_AUTHORIZATION = token
    )
    res <- mock_res()
    result <- api_job_info(api, req, res, job_id)
    expect_true(result$title == "NDVI based on Sentinel 2")
    expect_true(
        result$description == paste0(
            "Deriving minimum NDVI measurements over ",
            "pixel time series of Sentinel 2"
        )
    )
})

test_that(paste0(
    "GET /jobs/{id} > status: Correctly reports ",
    "status (and progress if supported)"
), {
    req <- mock_req(paste("/jobs", job_id, sep = "/"), method = "GET", HTTP_AUTHORIZATION = token)
    res <- mock_res()
    result <- api_job_info(api, req, res, job_id)
    expect_true(
        result$status %in% c("created", "queued", "running", "finished")
    )
})

test_that("DELETE /jobs/{id}: Is supported (with authentication)", {
    req <- mock_req(
        paste("/jobs", job_id, sep = "/"),
        method = "DELETE", HTTP_AUTHORIZATION = "invalid_token"
    )
    res <- mock_res()
    expect_error(api_job_delete(api, req, res, job_id))
})

test_that(paste0(
    "DELETE /jobs/{id}: Deletes the job and returns ",
    "HTTP 204 on success, or 4XX otherwise"
), {
    req <- mock_req(
        paste("/jobs", job_id, sep = "/"),
        method = "DELETE", HTTP_AUTHORIZATION = token
    )
    res <- mock_res()
    api_job_delete(api, req, res, job_id)
    expect_true(res$status == 204L)
})
