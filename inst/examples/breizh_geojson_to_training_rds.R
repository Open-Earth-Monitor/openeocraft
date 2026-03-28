# Build sits training samples (RDS) from labelled GeoJSON for openeocraft / ml_fit
# ---------------------------------------------------------------------------
# Mirrors the intent of openeo: load_collection → regularize (monthly) →
# optional NDVI → aggregate_spatial. Here, temporal aggregation is done by
# sits_regularize; extraction is sits_get_data(cube, sf).
#
# Aligned Python (prediction graph + ml_fit on this RDS):
#   inst/examples/breizh_openeo_training_predict_aligned.py
#
# Cross-walk (keep both files in sync when you change settings):
#   | This R variable          | Python constant / usage              |
#   |--------------------------|--------------------------------------|
#   | roi_ll (training bbox)   | TRAIN_EXTENT (optional check-cube)   |
#   | start_date, end_date     | TEMPORAL_EXTENT                      |
#   | cube_source "CDSE" +     | COLLECTION_ID "cdse-sentinel-2-l2a"  |
#   |   cube_collection        |   (first "-" splits source/coll.)  |
#   | bands                    | BANDS (lowercase STAC ids)           |
#   | sits_regularize P1M, 10  | REGULARIZE_PERIOD, RESOLUTION        |
#   | sits_apply NDVI B08/B04  | ndvi(B04,B08) → NDVI_TARGET          |
#   | output_rds               | TRAINING_RDS_URL in .py              |
#
# Prerequisites: sf, sits; Copernicus CDSE or MPC credentials in
# sits environment (see sits::sits_configure_* and package docs).
#
# GeoJSON: POINT or POLYGON features in WGS84 (EPSG:4326). You must have a
# column named `label` (class names). Rename before calling if your file uses
# e.g. "class" or "LC".
#
# Bands and timeline must match the prediction cube you build in openeo
# (same source/collection, extent, dates, regularization, band set).

suppressPackageStartupMessages({
    library(sf)
    library(sits)
})

# --- Training area (Brittany-style bbox; adjust as needed) -------------------
roi_ll <- c(
    lon_min = -4.02,
    lon_max = -3.74,
    lat_min = 48.20,
    lat_max = 48.30
)

start_date <- "2017-05-01"
end_date <- "2017-09-30"

# sits source/collection: must match what openeocraft exposes, e.g.
# collection id "cdse-sentinel-2-l2a" → source "cdse", collection "sentinel-2-l2a"
# (see sits::sits_config()$sources and openeo list_collection_ids()).
cube_source <- "CDSE"
cube_collection <- "SENTINEL-2-L2A"

# Use the same spectral bands as your prediction pipeline (names as in sits YAML).
bands <- c(
    "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08",
    "B8A", "B09", "B11", "B12", "SCL"
)

# Paths
geojson_path <- "./breizh_data.geojson"
output_rds <- "./breizh_training_sits.rds"
work_dir <- file.path(tempdir(), "sits_breizh_cube")
dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)

# --- Labelled geometries -----------------------------------------------------
samples_sf <- st_read(geojson_path, quiet = TRUE)
if (is.na(st_crs(samples_sf)$epsg) || st_crs(samples_sf)$epsg != 4326) {
    samples_sf <- st_transform(samples_sf, 4326)
}

if (!"label" %in% names(samples_sf)) {
    stop(
        "GeoJSON must include a column `label` (class per feature). ",
        "e.g. samples_sf$label <- samples_sf$class"
    )
}

geom_types <- unique(as.character(st_geometry_type(samples_sf)))
if (!all(geom_types %in% c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON"))) {
    stop("sits_get_data.sf expects POINT/POLYGON (or MULTI*) geometries.")
}

# --- Cube: same logic as openeo load_collection + temporal composite ----------
cube <- sits_cube(
    source = cube_source,
    collection = cube_collection,
    bands = bands,
    roi = roi_ll,
    start_date = start_date,
    end_date = end_date
)

cube_reg <- sits_regularize(
    cube = cube,
    period = "P1M",
    res = 10,
    output_dir = work_dir,
    multicores = parallel::detectCores(logical = FALSE) - 1L,
    progress = TRUE
)

# Optional: NDVI as extra band (align names with openeo ndvi / ml_predict cube)
ndvi_dir <- file.path(work_dir, "ndvi")
dir.create(ndvi_dir, showWarnings = FALSE, recursive = TRUE)
cube_reg <- sits_apply(
    cube_reg,
    NDVI = (B08 - B04) / (B08 + B04),
    output_dir = ndvi_dir,
    memsize = 4L,
    multicores = 2L,
    progress = TRUE
)

# --- Extract time series → sits tibble (replaces aggregate_spatial reducer) ---
samples <- sits_get_data(
    cube = cube_reg,
    samples = samples_sf,
    multicores = 4L,
    progress = TRUE,
    impute_fn = impute_linear()
)

saveRDS(samples, output_rds)
message("Wrote ", normalizePath(output_rds, winslash = "/", mustWork = FALSE))
message("Use in openeo: training_set = <URL or path to this RDS> in ml_fit.")
