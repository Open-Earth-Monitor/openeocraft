library(openeocraft)

api <- openeocraft::create_api(
  id = "openeo-sits",
  title = "openEO backend for sits",
  description = "This is an openEO compliant R backend for sits package.",
  backend_version = "0.1.0"
)

processes_file <- system.file("sits/processes.R", package = "openeocraft")
openeocraft::load_processes(api, processes_file)

catalog_file <- system.file("sits/catalog.json", package = "openeocraft")
openeocraft::load_collections(api, catalog_file)

openeocraft::run_api(host = "127.0.0.1", port = 8001)
