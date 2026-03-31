"""
Breizh-style workflow aligned with `breizh_geojson_to_training_rds.R`.

1. Run the R script to build `breizh_training_sits.rds` (sits_get_data from GeoJSON).
2. Host the RDS (HTTPS URL) or use a path visible to the openeocraft worker.
3. Keep COLLECTION_ID, BANDS, TEMPORAL_EXTENT, REGULARIZE_* in sync with the R file.

Training uses the RDS only (`ml_fit`); the prediction cube must use the same
spectral + temporal preprocessing as in R (same collection, bands, P1M, res, NDVI).
"""

from __future__ import annotations

import openeo  # type: ignore

# ---------------------------------------------------------------------------
# Keep in sync with inst/examples/breizh_geojson_to_training_rds.R
# ---------------------------------------------------------------------------

# openeocraft id = "<sits-source>-<collection>" (first hyphen splits source)
COLLECTION_ID = "cdse-sentinel-2-l2a"
# If your backend has no CDSE cube, try: "mpc-sentinel-2-l2a"

TRAIN_EXTENT = {"west": -4.02, "south": 48.20, "east": -3.74, "north": 48.30}
PRED_EXTENT = {"west": -2.96, "south": 47.91, "east": -2.70, "north": 48.06}

TEMPORAL_EXTENT = ["2017-05-01", "2017-09-30"]

# STAC-style names; load_collection maps them to sits YAML names (often B01, SCL, …)
BANDS = [
    "b01",
    "b02",
    "b03",
    "b04",
    "b05",
    "b06",
    "b07",
    "b08",
    "b8a",
    "b09",
    "b11",
    "b12",
    "scl",
]

REGULARIZE_PERIOD = "P1M"
REGULARIZE_RESOLUTION = 10

# After load_collection + cube_regularize, sits band labels are usually B04/B08-style.
# If ndvi fails with unknown band, inspect a dry-run error or backend logs and adjust.
NDVI_RED = "B04"
NDVI_NIR = "B08"
NDVI_TARGET = "NDVI"

TRAINING_RDS_URL = "https://example.org/breizh_training_sits.rds"

BACKEND_URL = "http://127.0.0.1:8000"
BASIC_USER = "brian"
BASIC_PASSWORD = "123456"


def build_regularized_cube_with_ndvi(connection, spatial_extent: dict):
    """load_collection → cube_regularize (same as sits_regularize in R) → ndvi."""
    cube = connection.load_collection(
        collection_id=COLLECTION_ID,
        spatial_extent=spatial_extent,
        temporal_extent=TEMPORAL_EXTENT,
        bands=BANDS,
    )
    cube = cube.process(
        process_id="cube_regularize",
        arguments={
            "data": cube,
            "period": REGULARIZE_PERIOD,
            "resolution": REGULARIZE_RESOLUTION,
        },
    )
    cube = cube.ndvi(red=NDVI_RED, nir=NDVI_NIR, target_band=NDVI_TARGET)
    return cube


def main() -> None:
    con = openeo.connect(url=BACKEND_URL)
    con.authenticate_basic(BASIC_USER, BASIC_PASSWORD)

    # Prediction cube (must match R: same collection, bands, period, res, NDVI)
    dc_pred = build_regularized_cube_with_ndvi(con, PRED_EXTENT)

    # Optional: build training-area cube only to validate the graph (not passed to ml_fit)
    # dc_train = build_regularized_cube_with_ndvi(con, TRAIN_EXTENT)

    rf_init = con.mlm_class_random_forest(num_trees=150, seed=42)
    rf_model = rf_init.fit(training_set=TRAINING_RDS_URL, target="label")

    classified = rf_model.predict(dc_pred)
    result = classified.save_result("GTiff")
    job = result.create_job(title="Breizh RF (RDS training + aligned cube)")
    job.start_and_wait()
    job.get_results().download_files("output")


if __name__ == "__main__":
    main()
