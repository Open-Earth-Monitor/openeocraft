# NEWS.md

## openeocraft (development version)

* Proposed L3-ML API profile: new `GET /ml_runtimes` endpoint (ML1-ML8) to
  declare ML runtimes keyed by STAC MLM `mlm:framework`, with training /
  inference support, `mlm:artifact_type` values, workflow types, training
  data formats and `mlm:accelerator` values. Declare runtimes with
  `set_ml_runtimes()`, `new_ml_runtime()` and `new_ml_runtime_version()`;
  `sits_ml_runtimes()` detects the bundled sits runtimes.
* New `GET /ml_models` and `GET /ml_models/{model_id}` endpoints (ML10, ML11)
  list stored models as STAC MLM Items; public models without auth, plus the
  user's own models with a bearer token. Paginated with `limit` / `page`.
* `load_ml_model` falls back to the public model folder, so every id listed
  by `GET /ml_models` can be loaded. Ids that resolved before resolve the
  same way.
* OpenAPI 3.0 description of the L3-ML endpoints in `inst/openapi/l3-ml.yaml`.
* Example notebooks and scripts (`inst/demo-lps-2025/`, `inst/demo-sw-paper-2025/`,
  `inst/demo-ml-paper-2026/`, `inst/examples/`) moved to
  <https://github.com/Open-Earth-Monitor/openeocraft-examples>.

## openeocraft 0.4.1

* openEO L1/L2 compliance hardening: auth status codes, sync `/result`,
  job logs/results (`partial`, `DELETE`), `/me`, billing stub, OIDC discovery
  stub, pagination on `/jobs` and `/processes`.
* User-defined processes: `/process_graphs` CRUD, `namespace` / UDP-by-`process_id`,
  and `from_parameter` evaluation.
* Infer finished-job STAC extents from raster outputs when metadata is missing.
* Bug fixes: reject empty UDP ids / nameless parameters; fix `ext_format()` for
  single-file sync results; apply runtime thread-limit env vars correctly;
  skip sits validation tests when `caret` is missing.
* CRAN preparation: drop non-CRAN `Remotes`/`openstac` Suggests (soft dependency),
  package topic, NEWS, contribution docs, quieter `.onLoad`, and R-CMD-check CI.
* Raise R/ unit-test coverage past 80%; add EMS 2026 paper citation to README.

## openeocraft 0.4.0

* Multi-cloud Terraform: AWS EC2 (GPU defaults), Azure, GCP, and OpenStack,
  with shared cloud-init and deployment docs.
* ML: enriched `ml_validate` / `ml_validate_kfold` metrics export;
  `save_ml_model` `return_model`; TempCNN `ml_tune_grid` example;
  Torch Lantern install/verify and `cube_regularize` pre-copy fixes.
* Docker/CI: Node 24 actions, image verify workflow, Torch build hardening,
  Compose CPU-cap removal, and ML batch-job / result-download URL fixes.

## openeocraft 0.3.1

* Workspace file storage API and uploaded-file loading process.

## openeocraft 0.3.0

* Initial public openEO craft backend with Plumber routes, process graphs,
  batch jobs, and sits-backed ML processes.
