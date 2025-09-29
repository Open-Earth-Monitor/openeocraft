# Mock API object
api <- mock_create_openeo_v1()
token <- readLines(file(system.file("mock/token", package = "openeocraft")))
job_id <- NULL
mock_job <- jsonlite::read_json(system.file("mock/mock-job.json", package = "openeocraft"))
mock_badjob <- jsonlite::read_json(system.file("mock/mock-badjob.json", package = "openeocraft"))

test_that("Synchronous processing is implemented", {
    req <- mock_req("/process", method = "POST", HTTP_AUTHORIZATION = token)
    req$body <- mock_job

    res <- mock_res()

    result <- api_result(api, req, res)
    expect_true(is.environment(result))
})

test_that("Batch job processing is implemented", {
    req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = token)
    req$body <- mock_job
    res <- mock_res()

    result <- api_job_create(api, req, res)
    job_id <<- res$getHeader("OpenEO-Identifier")

    api_job_start(api, req, res, job_id)
    expect_true(res$status == 202L)
})

test_that("Job follows the correct status changes", {
    req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = token)
    req$body <- mock_job
    res <- mock_res()

    api_job_create(api, req, res)
    job_id <<- res$getHeader("OpenEO-Identifier")

    job <- api_job_info(api, req, res, job_id)
    expect_true(is.list(job))
    expect_equal(job$status, "created")

    api_job_start(api, req, res, job_id)
    job <- api_job_info(api, req, res, job_id)
    expect_true(job$status %in% c("running", "finished"))
})
