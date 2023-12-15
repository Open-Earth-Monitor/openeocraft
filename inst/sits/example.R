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
load_collections(api)

run_api(api, host = "127.0.0.1", port = 8001)



proc <- jsonlite::read_json(system.file("extdata/example2.json", package = "openeocraft"))
pgraph_expr(proc)
eval(pgraph_expr(proc), envir = get_attr(api, "processes"))



