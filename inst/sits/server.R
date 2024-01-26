library(openeocraft)

# Create API object
api <- create_api(
  id = "openeo-sits",
  title = "openEO backend for sits",
  description = "This is an openEO compliant R backend for sits package.",
  backend_version = "0.1.0"
)

# Load processes
processes_file <- system.file("sits/processes.R", package = "openeocraft")
load_processes(api, processes_file)

# Load collections
collections <- jsonlite::read_json(
  path = system.file("sits/collections.json", package = "openeocraft"),
  auto_unbox = TRUE
)
load_collections(api, collections = collections)

# Run server
run_api(api, host = "http://127.0.0.1", port = 8001)

