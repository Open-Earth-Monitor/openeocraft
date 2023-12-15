library(openeocraft)

api <- create_api(
  id = "openeo-sits",
  title = "openEO backend for sits",
  description = "This is an openEO compliant R backend for sits package.",
  backend_version = "0.1.0"
)

processes_file <- system.file("sits/processes.R", package = "openeocraft")
load_processes(api, processes_file)

catalog_file <- system.file("sits/catalog.json", package = "openeocraft")
stac_endpoint <- "https://example.com/stac"
load_collections(api, stac_catalog = catalog_file)
load_collections(api, stac_api = stac_endpoint)
load_collections(api, data = list(list(...)))

run_api(api, host = "127.0.0.1", port = 8001)



