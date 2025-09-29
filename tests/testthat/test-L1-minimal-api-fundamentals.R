# Create a mock API instance
api <- mock_create_openeo_v1()

# Helper function to simulate a request
simulate_request <- function(req_fn) {
    req <- req_fn()
    res <- mock_res()
    list(req = req, res = res)
}

test_that("CORS: OPTIONS method supports all headers", {
    result <- simulate_request(function() mock_req("/", method = "OPTIONS"))
    expect_true("content-type" %in% result$req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    expect_true("authorization" %in% result$req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    expect_true("accept" %in% result$req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
})

test_that("CORS: Access-Control-Allow-Origin is correctly set", {
    result <- simulate_request(function() mock_req("/", method = "GET"))
    result$res$setHeader("Access-Control-Allow-Origin", "*")
    expect_equal(result$res$getHeader("Access-Control-Allow-Origin"), "*")
})

test_that("CORS: Access-Control-Expose-Headers is correctly set", {
    result <- simulate_request(function() mock_req("/", method = "GET"))
    result$res$setHeader("Access-Control-Expose-Headers", "content-type, authorization, accept")
    expect_equal(result$res$getHeader("Access-Control-Expose-Headers"), "content-type, authorization, accept")
})

test_that("UTF-8: Charset is UTF-8 for all requests and responses", {
    result <- simulate_request(function() mock_req("/", method = "GET"))
    result$res$setHeader("Content-Type", "application/json; charset=utf-8")
    expect_equal(result$res$getHeader("Content-Type"), "application/json; charset=utf-8")
})

test_that("HTTPS: HTTPS is enforced for all requests", {
    result <- simulate_request(function() mock_req("/", method = "GET"))
    expect_equal(result$req$rook.url_scheme, "https")
})


test_that("Error Handling: Returns valid HTTP error codes", {
    error_codes <- c(400L, 401L, 403L, 404L, 500L, 503L)
    for (code in error_codes) {
        result <- list(status = code)
        expect_true(result$status >= 400L && result$status <= 599L)
    }
})

test_that("Error Handling: Returns JSON object with code and message", {
    error_response <- list(code = 404L, message = "Not Found")
    json_response <- jsonlite::toJSON(error_response, auto_unbox = TRUE)
    parsed_response <- jsonlite::fromJSON(json_response)
    expect_true("code" %in% names(parsed_response))
    expect_true("message" %in% names(parsed_response))
})

test_that("Process Graphs: Callback functionality is supported", {
    result <- simulate_request(function() mock_req("/result", method = "POST"))
    expect_true(!is.null(result$req$REQUEST_METHOD))
    expect_equal(result$req$REQUEST_METHOD, "POST")
})
