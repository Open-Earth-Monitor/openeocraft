# Mock API object
api <- mock_create_openeo_v1()
token <- "5aad11e1d49b880a4468e1b252944e22"
job_id <- NULL

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
    "process_graph": {
      "dc": {
        "process_id": "load_collection",
        "arguments": {
          "id": "Sentinel-2",
          "spatial_extent": {
            "west": 16.1,
            "east": 16.6,
            "north": 48.6,
            "south": 47.2
          },
          "temporal_extent": [
            "2018-01-01",
            "2018-02-01"
          ]
        }
      },
      "save": {
        "process_id": "save_result",
        "arguments": {
          "data": {
            "from_node": "dc"
          },
          "format": "GTiff"
        },
        "result": true
      }
    }
  },
  "log_level": "warning"
}'

# --- Processing Implementation Tests ---

test_that("Synchronous processing is implemented", {
  req <- mock_req("/process", method = "POST", HTTP_AUTHORIZATION = token)
  req$body <- jsonlite::fromJSON(mock_job, simplifyDataFrame = FALSE)

  res <- mock_res()

  result <- api_result(api, req, res)
  expect_true(is.environment(result))
})

test_that("Batch job processing is implemented", {
  req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = token)
  req$body <- jsonlite::fromJSON(mock_job, simplifyDataFrame = FALSE)
  res <- mock_res()

  result <- api_job_create(api, req, res)
  job_id <<- res$getHeader("OpenEO-Identifier")

  api_job_start(api, req, res, job_id)
  expect_true(res$status == 202L)
})

# --- Job Status Tests ---

test_that("Job follows the correct status changes", {
  req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = token)
  req$body <- jsonlite::fromJSON(mock_job, simplifyDataFrame = FALSE)
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
