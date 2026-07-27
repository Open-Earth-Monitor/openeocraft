# NEWS.md

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
