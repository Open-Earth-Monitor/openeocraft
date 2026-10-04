# Development Workflow for openEOcraft

## Overview

The openEOcraft server can be run in two modes:
1. **Docker mode** (production): Requires rebuilding the container for changes
2. **Local mode** (development): Loads processes directly from source files

## Process Loading Mechanism

The server loads processes from `inst/ml/processes.R`. The loading mechanism now automatically detects the environment:

- **In Docker**: Loads from installed package location
- **Locally**: Loads directly from `inst/ml/processes.R` (no reinstall needed!)

## Workflow 1: Docker Mode (Production)

Use this for testing the production environment.

### Steps:
```bash
# 1. Make changes to inst/ml/processes.R or other files

# 2. Rebuild and restart the container
docker-compose down
docker-compose up --build -d

# 3. Check logs if needed
docker-compose logs -f
```

### When to rebuild:
- After modifying `inst/ml/processes.R`
- After changing any R code in the package
- After updating dependencies in DESCRIPTION

## Workflow 2: Local Mode (Development) ⚡ FASTER

Use this for rapid development and testing.

### Steps:
```bash
# 1. Stop any running servers (Docker or local)
docker-compose down
pkill -9 -f "Rscript.*server.R"  # Kill any local R servers
lsof -ti:8000 | xargs kill -9    # Free port 8000 if needed

# 2. Make changes to inst/ml/processes.R

# 3. Start server directly (from repo root - IMPORTANT!)
cd /path/to/openeocraft  # Make sure you're in repo root
Rscript docker/server.R

# 4. Server automatically loads from inst/ml/processes.R
# Look for: "Loading processes from source: /path/to/inst/ml/processes.R"
# No package reinstall needed! Just restart the server.
```

### Important:
- **Must run from repo root**: The server looks for `inst/ml/processes.R` relative to the current directory
- **Check startup logs**: Verify it says "Loading processes from source" not "from package"
- **Port conflicts**: If port 8000 is busy, kill the process using it first

### Advantages:
- ✅ No Docker rebuild needed
- ✅ Instant process changes (just restart server)
- ✅ Easier debugging with direct R output
- ✅ Faster iteration cycle

### How it works:
The `plumber.R` file checks if `inst/ml/processes.R` exists:
- **Found**: Loads from source (local development)
- **Not found**: Loads from package (Docker mode)

## Testing Changes

### Verify processes are loaded:
```bash
# List all processes
curl http://127.0.0.1:8000/processes | jq '.processes[] | .id'
```

### Quick package tests:
```bash
Rscript -e 'devtools::load_all(); testthat::test_dir("tests/testthat")'
```

## Adding New Processes

1. Add the R function with the decorator in `inst/ml/processes.R`:

```r
#* @openeo-process
my_new_process <- function(param1, param2 = NULL) {
    # Implementation
}
```

2. Create/edit the JSON specification: `inst/ml/processes/my_new_process.json`

3. Restart server:
   - **Local mode**: Just restart `Rscript docker/server.R`
   - **Docker mode**: Run `docker-compose up --build -d`

Auto-generated process JSON (when no hand-written file exists) includes
`"_generated_by": "openeocraft"` so tooling can distinguish scaffolded
descriptors from curated ones. Existing JSON files are never overwritten.

## Result serializers (dual path)

Synchronous `POST /result` sets the HTTP body via **`data_serializer.*`**
methods in `R/data.R` (used by `api_result()`). Plumber’s registered
`"serialize_result"` serializer dispatches through **`get_serializer.*`**
in `R/serializers.R`. Keep both paths aligned when adding formats
(GeoTIFF, NetCDF, RDS, tar, JSON).

## Troubleshooting

### Process not showing up in client
```r
# Refresh the process list
p <- processes()
```

### Check which file is being loaded
Look for this in server output:
```
Loading processes from source: inst/ml/processes.R  # Local mode
Loading processes from package: /usr/local/lib/R/... # Docker mode
```

### Server won't start locally
Make sure you're in the repo root when running:
```bash
pwd  # Should show: .../ml-dev/openeocraft
Rscript docker/server.R
```

## Recommendation

**For development**: Use **Local Mode** (Workflow 2)
- Much faster iteration
- No Docker overhead
- Direct error messages

**For testing production setup**: Use **Docker Mode** (Workflow 1)
- Tests the actual deployment environment
- Verifies package installation works correctly

## Current Status

✅ `merge_cubes` is now available and working
✅ `ndvi` returns only the NDVI band (requires merge)
✅ Both Docker and Local modes supported
✅ 27 processes registered

## Roadmap / TODO triage

Internal `# TODO` comments are triaged below so hardening stays additive and
does not break existing clients. Prefer completing an item and deleting its
TODO over leaving speculative notes in hot paths.

### Done / clarified (safe hardening)

| Area | Notes |
|------|--------|
| GeoTIFF plumber serializer | `get_serializer.openeo_gtiff` mirrors `data_serializer.openeo_gtiff` |
| Sync multi-file tar | Packs basenames only (no absolute paths) |
| Job list / info `links` | `self` (+ `results` when finished) |
| Job start messages | Distinct “already finished” vs “already started” |
| `export_ml_model` assets | Workspace href to saved `.rds` |
| Soft `job_check` | Requires `process`; fills plan / log_level defaults |
| Atomic `jobs.rds` / `logs.rds` | Temp file + rename |
| Process JSON provenance | `_generated_by: openeocraft` on scaffold writes |
| Token renewal | Already renews on expiry in `api_credential` |
| `format_content_type()` | Already implemented in `R/data.R` |
| `/jobs` and `/processes` `limit` | Optional pagination + `self`/`next`/`prev` links |
| `/processes` `links` | Includes `self` (and pagination links when limited) |
| Process graph `from_parameter` | Nested UDP / callback parameter references |
| `/process_graphs` CRUD | Per-user UDP store (`process_graphs.rds`) |
| Process `namespace` + UDP-by-`process_id` | `user` namespace; predefined when `namespace` is null |
| CRAN packaging prep | No Remotes; soft STAC via Docker; `--as-cran` clean |
| L3-ML `GET /ml_runtimes` (ML1-ML8) | `R/ml_runtimes.R`; `set_ml_runtimes(api, sits_ml_runtimes())` in `docker/plumber.R` |
| L3-ML `GET /ml_models[/{id}]` (ML10-ML11) | `R/ml_models.R`; same lookup order as `load_ml_model`; spec in `inst/openapi/l3-ml.yaml` |

### Deferred (larger / product-dependent)

| Item | Why deferred |
|------|----------------|
| Worker queue / max workers | Architecture + fairness; risk to running jobs |
| Split eval environments / per-request process load | Isolation project; high blast radius |
| Billing key on landing page | Needs product / config design |
| Optional landing `rel`s (terms, privacy, create-form, …) | Product content; core links already present |
| OIDC login, service types | Each is an openEO endpoint epic |
| URL-based process namespaces | Remote UDP fetch; L2 satisfied by `user` namespace |
| `POST /validation` | Optional companion to UDP CRUD |
| openstac on CRAN | Tracked with Open-Earth-Monitor/openstac; Docker installs from GitHub |
| Collection queryables (openSTAC) | Depends on STAC backend |
| sqlite / mongo job store | Only if file rename + locking prove insufficient |
| Process path markers + `usage_*` metrics | Spec-nice; large instrumentation |
| OpenAPI response models | Low demand until typed clients need schemas |
| `@openeo-import` / multi-file processes | Load-order and JSON layout design |
| L3-ML ML9 (runtime/metadata mismatch errors) | `save_ml_model` writes `mlm:artifact_type = "application/octet-stream"` by default while the runtime declares `saveRDS`; align the default first, then let `load_stac_ml` reject mismatches with an error that names the field |
| `GET /udf_runtimes` | Separate openEO endpoint epic; `/ml_runtimes` mirrors its shape |
| `limit` pagination on `/jobs` and `/processes` | Done — optional `limit`/`page`; omit `limit` returns all |
| `register_file_format()` API | Additive; formats remain hardcoded for now |

When picking up deferred work, add tests first and keep default request
shapes backward-compatible (optional query params, soft validation only).
