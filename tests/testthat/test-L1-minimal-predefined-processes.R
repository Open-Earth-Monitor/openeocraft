# Mock API object
api <- mock_create_openeo_v1()
token <- readLines(file(system.file("mock/token", package = "openeocraft")))

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

    # TODO: implement limit parameter
})

test_that("GET /processes > processes: Missing properties in process objects are not set to null if not valid according to OpenAPI schema", {
    req <- mock_req("/processes", method = "GET", HTTP_AUTHORIZATION = token)
    res <- mock_res()

    result <- api_processes(api, req, res)

    for (process in result$processes) {
        expect_false(any(sapply(process, is.null)))
    }
})
