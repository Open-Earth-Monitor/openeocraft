# OpenEOcraft

**OpenEOcraft** is a generic R-based framework for Earth observation (EO) data cube analysis that plugs into the [openEO](https://openeo.org/) ecosystem. It connects R’s geospatial, statistical, and machine learning stacks to openEO clients (R, Python, Julia, JavaScript, Web Editor) through a standard REST API and process graphs, with STAC-oriented data discovery where configured. Compared with backends tied to a single cube engine, OpenEOcraft is built for **multi-library** workflows (e.g. [sits](https://github.com/e-sensing/sits), [stars](https://r-spatial.github.io/stars/), [terra](https://rspatial.org/terra/), [gdalcubes](https://github.com/appelmar/gdalcubes)) under one openEO-compliant surface.

The implementation follows a modular layout: an **R execution engine**, a **process graph translation core**, and a **REST API** (Plumber). A central extension point is the **decorator-based process registry**: R functions tagged with `#* @openeo-process` are discovered at startup, paired with JSON process definitions, and exposed on `/processes` for clients.

<img src="man/figures/openeocraft-architecture.png" alt="OpenEOcraft Architecture"  />

## Three-layer architecture

| Layer | Role |
| ----- | ---- |
| **API** | openEO-compliant HTTP routes (`/processes`, `/jobs`, `/result`, …), auth, CORS, optional STAC integration. |
| **Core** | Parses JSON process graphs, resolves dependencies, maps nodes to R calls, job orchestration (no HTTP in the core). |
| **R engine** | Runs workflows in isolated job contexts; integrates cube and ML code (e.g. sits raster cubes, or your own adapters). |

### R packages: end-to-end or mix-and-match

You are not locked into a single R library. **One option** is to standardize on a stack (e.g. [sits](https://github.com/e-sensing/sits) for cubes, indices, and ML) and use it **end to end** across the processes you expose. **Another option** is to combine packages **where each step needs them**: the same backend can register processes that call [stars](https://r-spatial.github.io/stars/), [terra](https://rspatial.org/terra/), or [gdalcubes](https://github.com/appelmar/gdalcubes) for raster / EO-cube work, [gstat](https://cran.r-project.org/package=gstat) or related tools for **geostatistical modelling**, [caret](https://cran.r-project.org/package=caret) or [tidymodels](https://www.tidymodels.org/) for **classical ML**, [torch](https://torch.mlverse.org/) for **deep learning**, and so on. Each `#* @openeo-process` function is ordinary R—import and wire dependencies per process, as long as inputs and outputs fit your job pipeline and process JSON contracts.

## Decorator-based process registration

Processes are **declarative**: you define an ordinary R function and prefix it with `#* @openeo-process` in a processes source file (see [`R/decorators.R`](R/decorators.R) and [`R/run.R`](R/run.R)). At load time, OpenEOcraft scans these chunks and looks for JSON descriptors in a `processes/` directory **next to that source file**—for example [`inst/ml/processes/ndvi.json`](inst/ml/processes/ndvi.json) next to [`inst/ml/processes.R`](inst/ml/processes.R). If you add your own process file elsewhere, the same layout applies (`…/processes/<id>.json` beside that `.R` file); this repo ships only the **ml** module (see [`DEVELOPMENT.md`](DEVELOPMENT.md)). That keeps the core free of one-off patches for every new algorithm and lowers the effort needed to expose additional R methods as openEO processes.

### Example: ML prediction with **sits**

A minimal pattern for wrapping sits classification as an openEO process is:

```r
#* @openeo-process
ml_predict <- function(model, data) {
  data <- sits::sits_classify(data, ml_model = model)
  data <- sits::sits_label_classification(data)
  data
}
```

You still add or maintain `inst/ml/processes/ml_predict.json` so parameters, return type, and descriptions match the openEO process spec expected by clients. The shipped implementation in this repo lives in [`inst/ml/processes.R`](inst/ml/processes.R) and extends this idea with the full ML process set (e.g. `ml_fit`, `mlm_class_tempcnn`, STAC-MLM helpers).

### Shipped **ndvi** (sits)

The only **ndvi** implementation included in this repository is in [`inst/ml/processes.R`](inst/ml/processes.R): it runs on **regular sits raster cubes** (NIR/red via sits’ raster apply path). Its openEO contract is [`inst/ml/processes/ndvi.json`](inst/ml/processes/ndvi.json): **`data`** (required datacube with a **bands** dimension), **`nir`** and **`red`** (optional band names / common names, defaults `"nir"` and `"red"`), **`target_band`** (optional `null` or a string matching `^\\w+$`). Returns: if **`target_band`** is `null`, the result has **no** bands dimension; if it is a string, the bands dimension is kept and gains that label (`BandExists` if the name already exists). Exceptions in the JSON (`DimensionAmbiguous`, `NirBandAmbiguous`, `RedBandAmbiguous`, `BandExists`) describe client-visible failures.

### Example **stars** / **terra** / **gdalcubes** **ndvi** (not shipped)

The **stars**, **terra**, and **gdalcubes** snippets below are **documentation examples only**. They are **not** present in the repo, **not** registered by the default server, and **not** part of the distributed package. They sketch how you could implement the **same** openEO `ndvi` process id using another cube representation if you add your own process file, copy [`inst/ml/processes/ndvi.json`](inst/ml/processes/ndvi.json) beside it (same parameter schema for clients), and wire [`load_processes()`](R/run.R) to that file. The decorator expects JSON at `dirname(<your_processes.R>)/processes/ndvi.json` (e.g. a hypothetical `inst/stars/processes.R` + `inst/stars/processes/ndvi.json`). Only one `ndvi` implementation should be active per server instance.

**R signature** (must stay aligned with [`inst/ml/processes.R`](inst/ml/processes.R) and [`ndvi.json`](inst/ml/processes/ndvi.json)):

```r
ndvi <- function(data, nir = "nir", red = "red", target_band = NULL)
```

**Example: stars** (template — cube should expose a **bands** dimension named `band` in `st_dimensions`; resolve `nir`/`red` against band **names** as in the spec):

```r
# Example only — not in repo. Place e.g. inst/stars/processes.R and copy ndvi.json alongside.
#* @openeo-process
ndvi <- function(data, nir = "nir", red = "red", target_band = NULL) {
  d <- stars::st_dimensions(data)
  band_i <- which(names(d) == "band")
  if (length(band_i) != 1L) {
    stop("DimensionAmbiguous: expected a single bands dimension")
  }
  margin <- setdiff(seq_along(d), band_i)
  labs <- as.character(d[[band_i]]$values)
  out <- stars::st_apply(data, margin, function(v) {
    names(v) <- labs
    (v[nir] - v[red]) / (v[nir] + v[red])
  })
  if (is.null(target_band)) {
    # openEO: output has no bands dimension — drop a length-1 band axis if present
    return(out)
  }
  if (!grepl("^\\w+$", target_band)) {
    stop("target_band must be NULL or match ^\\\\w+$")
  }
  names(out) <- target_band
  c(data, out, along = "band")
}
```

**Example: terra** (template — `SpatRaster` layers; `nir` / `red` are layer names; `target_band = NULL` returns **only** the NDVI layer, consistent with dropping the bands dimension):

```r
# Example only — not in repo. Place e.g. inst/terra/processes.R and copy ndvi.json alongside.
#* @openeo-process
ndvi <- function(data, nir = "nir", red = "red", target_band = NULL) {
  x <- terra::rast(data)
  nd <- (x[[nir]] - x[[red]]) / (x[[nir]] + x[[red]])
  if (is.null(target_band)) {
    return(nd)
  }
  if (!grepl("^\\w+$", target_band)) {
    stop("target_band must be NULL or match ^\\\\w+$")
  }
  names(nd) <- target_band
  terra::c(x, nd)
}
```

**Example: gdalcubes** (template — `data` is a [gdalcubes](https://github.com/appelmar/gdalcubes) raster cube; `expr` is a [tinyexpr](https://github.com/codeplea/tinyexpr)-style string using **band names** as they appear on the cube, e.g. `"(B08-B04)/(B08+B04)"`. Use `keep_bands = TRUE` when `target_band` is set so input bands stay and the NDVI label is added, matching openEO; when `target_band` is `NULL`, `keep_bands = FALSE` yields a single derived band.)

```r
# Example only — not in repo. Place e.g. inst/gdalcubes/processes.R and copy ndvi.json alongside.
#* @openeo-process
ndvi <- function(data, nir = "nir", red = "red", target_band = NULL) {
  if (!is.null(target_band) && !grepl("^\\w+$", target_band)) {
    stop("target_band must be NULL or match ^\\\\w+$")
  }
  expr <- sprintf("(%s-%s)/(%s+%s)", nir, red, nir, red)
  nm <- if (is.null(target_band)) "ndvi" else target_band
  keep_bands <- !is.null(target_band)
  gdalcubes::apply_pixel(data, expr, names = nm, keep_bands = keep_bands)
}
```

**Clients** (e.g. Sentinel-2) pass explicit band names when metadata does not use common names `red` / `nir`, e.g. `nir="B08", red="B04", target_band="NDVI"` — see the Python workflow below.

## Documentation

- **Vignette:** after installing the package (with suggested packages **knitr** and **rmarkdown** so the HTML is built), run `vignette("openeocraft")` or `browseVignettes("openeocraft")` for *Introduction to OpenEOcraft* (mock `load_processes` example and links to deeper topics). The source is [`vignettes/openeocraft.Rmd`](vignettes/openeocraft.Rmd).
- **R help:** decorator and runtime topics include `help("openeocraft_decorators", package = "openeocraft")`, `?load_processes`, and `help("openeo-process", package = "openeocraft")`.

## Quick Start with Docker Hub

The easiest way to get started with OpenEOcraft is by using the pre-built Docker image from Docker Hub:

```bash
docker run -p 8000:8000 brianpondi/openeocraft:latest
```

This will start the OpenEOcraft server and make it available at `http://localhost:8000`.

## Deployment Options

### Local Deployment with R
If you have R installed and the relevant packages like plumber, sits, torch, jsonlite, etc., you can run the server locally. 

1. Clone the repository:
```bash
git clone https://github.com/Open-Earth-Monitor/openeocraft.git
cd openeocraft
```

2. Run the server:
```bash
Rscript docker/server.R
```

### Development with Docker Compose
For development or custom modifications, you can build and run using Docker Compose:

```bash
git clone https://github.com/Open-Earth-Monitor/openeocraft.git
cd openeocraft
docker-compose up
```

#### Common Docker Compose Commands:
- Run in detached mode: `docker-compose up -d`
- Stop the containers: `docker-compose down`
- Force rebuild and restart: `docker-compose up --build --force-recreate --no-deps -d`
- Rebuild with no cache: `docker-compose build --no-cache && docker-compose up`

## Resource guidelines (Docker and production)

OpenEOcraft does **not** probe the physical host directly. It uses **whatever CPUs and memory the R process sees** (inside the container, that is usually the **cgroup** limit from Docker or Kubernetes).

### Default behaviour (ML / sits tuning paths)

Defined in `inst/ml/processes.R` (see comments at the top of that file):

| Mechanism                                                                   | Default                                                                                                                                                                                                                                                   |
| --------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **CPU parallel jobs** (`multicores` passed to sits, e.g. k-fold validation) | `parallel::detectCores(logical = FALSE)` × **`openeocraft.resource_fraction`** (default **0.75**), capped by **`openeocraft.multicores_max`** (default **16**).                                                                                           |
| **Sits `memsize` (GB)**                                                     | If **`openeocraft.memsize`** is unset and **`openeocraft.memsize_auto`** is `TRUE`: total RAM from **`Sys.meminfo()`** (R ≥ 4.4) or **`/proc/meminfo`** on Linux × **`resource_fraction`**, capped at 256 GB. If detection fails, falls back to **8** GB. |

So the backend aims for **~75% of the resources visible inside the container**, not 75% of a bare-metal host unless the container sees the full host.

### Docker: give the container the capacity you want

- **CPU:** e.g. `docker run --cpus="8" ...` or Compose `cpus:` / Kubernetes `resources.limits.cpu`.
- **Memory:** e.g. `docker run -m 32g ...` or Compose `mem_limit:` / K8s `resources.limits.memory`.

Without limits, behaviour depends on the host and cgroup version; **setting limits** makes OpenEOcraft’s auto settings align with your intended share of the machine.

Example:

```bash
docker run -p 8000:8000 --cpus="16" -m 64g brianpondi/openeocraft:latest
```

### R `options()` overrides

Set these **before** jobs run (e.g. at the top of `docker/server.R`, or via `.Rprofile` in the image):

```r
# Use 90% of detected resources instead of 75%
options(openeocraft.resource_fraction = 0.9)

# Allow more than 16 parallel cores (or Inf to disable the cap)
options(openeocraft.multicores_max = 32)

# Fixed sits memory budget in GB (skips auto-detection)
options(openeocraft.memsize = 48L)

# Turn off RAM auto-detection and use the 8 GB fallback
options(openeocraft.memsize_auto = FALSE)
```

### BLAS / Torch threading

Sits `multicores` is not the only source of parallelism. **OpenMP / MKL / torch** may spawn extra threads. For predictable use inside a container, set environment variables **before R starts** (Docker `ENV`, Compose `environment:`, systemd, etc.), for example:

- `OMP_NUM_THREADS`
- `MKL_NUM_THREADS`
- `TORCH_NUM_THREADS`

Align these with the CPU quota you gave the container.

### GPU (CUDA)

For NVIDIA GPUs, run the container with GPU access (e.g. `docker run --gpus all ...`). That is independent of the `multicores` / `memsize` logic; torch/sits can still use the GPU when available.

## Example Workflows

### Authentication

- **Docker Hub quick start** (above): often `user` / `password`.
- **Local / dev** (see `docker/plumber.R` and notebooks under `inst/demo-lps-2025/`): often `brian` / `123456`.

### Python example (first): P16D cube, NDVI, TempCNN — `00_ml_month.ipynb`

Same pipeline as [`inst/demo-lps-2025/00_ml_month.ipynb`](inst/demo-lps-2025/00_ml_month.ipynb) and [`inst/demo-paper-2025/tempcnn_model_training.R`](inst/demo-paper-2025/tempcnn_model_training.R), as **one runnable script**. The worker downloads training samples from **HTTPS** (no local `readRDS`).

Requires a Python client with ML helpers, e.g.:

```bash
pip install git+https://github.com/PondiB/openeo-python-client.git
```

**Timeline / bands:** [`samples_deforestation_rondonia.rds`](https://github.com/Open-Earth-Monitor/openeocraft/raw/main/inst/demo-paper-2025/data/samples_deforestation_rondonia.rds) was built for a **P16D** cube (not P1M) and the same preprocessing as that R script (**600 m** resolution, selected bands + NDVI). If `cube_regularize` uses a different **`period`** or **`resolution`**, sits raises **`.check_samples_tile_match_timeline: tile timeline does not match samples timeline`**. After regularize, call `ndvi` with explicit band names if needed, e.g. **`nir="B08"`**, **`red="B04"`**, **`target_band="NDVI"`** (see [`inst/ml/processes/ndvi.json`](inst/ml/processes/ndvi.json)). For a smaller 10-band cube without NDVI, see the second Python block and [`01_ml_api_eo_data_cubes.ipynb`](inst/demo-lps-2025/01_ml_api_eo_data_cubes.ipynb).

```python
#!/usr/bin/env python3
"""OpenEOcraft: P16D + NDVI + TempCNN (Rondonia). See 00_ml_month.ipynb."""

import openeo

BACKEND_URL = "http://127.0.0.1:8000"
USER = "user"
PASSWORD = "password"

TRAINING_RDS_URL = (
    "https://github.com/Open-Earth-Monitor/openeocraft/raw/main/"
    "inst/demo-paper-2025/data/samples_deforestation_rondonia.rds"
)

connection = openeo.connect(BACKEND_URL)
connection.authenticate_basic(USER, PASSWORD)

print("Available collections:", connection.list_collection_ids())
print("Available processes:", [p["id"] for p in connection.list_processes()])

bbox = {
    "west": -63.33,
    "south": -12.03,
    "east": -62.43,
    "north": -11.13,
    "crs": 4326,
}
temporal_extent = ["2022-01-01", "2022-12-31"]

datacube = connection.load_collection(
    "aws-sentinel-2-l2a",
    spatial_extent=bbox,
    temporal_extent=temporal_extent
)

datacube = datacube.process(
    process_id="cube_regularize",
    arguments={
        "data": datacube,
        "period": "P16D",
        "resolution": 600,
    },
)

datacube = datacube.ndvi(nir="B08", red="B04", target_band="NDVI")

tempcnn_model_init = connection.mlm_class_tempcnn(
    optimizer="adam",
    learning_rate=0.0005,
    seed=42,
)
tempcnn_model = tempcnn_model_init.fit(
    training_set=TRAINING_RDS_URL,
    target="label",
)

datacube = tempcnn_model.predict(datacube)
tempcnn_model.save_ml_model(name="tempcnn_rondonia")

result = datacube.save_result(format="GeoTiff")
job = result.create_job(
    title="Deforestation Prediction in Rondonia",
    description="Using TempCNN model to predict deforestation in Rondonia",
)
job.start_and_wait()
job.get_results().download_files("data/output")
```

### R example: TempCNN on Rondonia (aligned with `inst/demo-lps-2025/`)

Use **`ml_fit(model = …, training_set = …)`** — the first argument must be the model spec from `mlm_class_*`. For [`samples_deforestation_rondonia.rds`](https://github.com/e-sensing/sitsdata/raw/main/data/samples_deforestation_rondonia.rds), the prediction cube must use the **same spectral bands** as the samples (10 S2 bands; **no** extra NDVI/`cloud` layer unless the RDS was built with them).

```r
library(openeo)

con <- connect(
    host = "http://127.0.0.1:8000",
    user = "brian",
    password = "123456"
)
p <- processes()

# Public sits samples (HTTPS URL); alternatively use jsonlite::serializeJSON(your_sits_tibble)
rondonia_url <-
    "https://github.com/e-sensing/sitsdata/raw/main/data/samples_deforestation_rondonia.rds"

# Cube bands must match the training RDS (lowercase names are fine; backend maps them)
dc <- p$load_collection(
    id = "mpc-sentinel-2-l2a",
    spatial_extent = list(
        west = -63.50,
        south = -8.92,
        east = -63.35,
        north = -8.78
    ),
    temporal_extent = c("2022-01-01", "2022-12-31"),
    bands = list(
        "b02", "b03", "b04", "b05", "b06", "b07", "b08",
        "b11", "b12", "b8a"
    )
)

dc <- p$cube_regularize(data = dc, period = "P16D", resolution = 300)

spec <- p$mlm_class_tempcnn(
    optimizer = "adam",
    learning_rate = 0.0005,
    seed = 42,
    epochs = 20,
    batch_size = 64,
    verbose = TRUE
)

fitted <- p$ml_fit(
    model = spec,
    training_set = rondonia_url,
    target = "label"
)

dc <- p$ml_predict(data = dc, model = fitted)

# Optional: persist model metadata in the job workspace
# p$save_ml_model(data = fitted, name = "tempcnn_rondonia_2022")

job_graph <- p$save_result(data = dc, format = "GTiff")

job <- create_job(
    graph = job_graph,
    title = "Rondonia TempCNN",
    description = "Train on sits sample URL; predict GeoTIFF"
)
job <- start_job(job)

status <- describe_job(job)
if (status$status == "finished") {
    download_results(job, "./output")
}
```

More patterns (export cube, RF-only training, etc.): `inst/examples/`. Training **without** the HTTP API: `inst/examples/04_ml_tempcnn.R` (uses `jsonlite::serializeJSON` on an in-memory sits object and `export_ml_model`).

### Python: Rondonia TempCNN — `mpc`, P16D, 10 bands (no NDVI)

Shorter alternative when you want **`mpc-sentinel-2-l2a`**, a **small bbox**, and **`samples_deforestation_rondonia.rds`** (10 bands, no NDVI). Same idea as [`01_ml_api_eo_data_cubes.ipynb`](inst/demo-lps-2025/01_ml_api_eo_data_cubes.ipynb).

```python
import openeo

connection = openeo.connect("http://127.0.0.1:8000")
connection.authenticate_basic("brian", "123456")

rondonia_url = (
    "https://github.com/e-sensing/sitsdata/raw/main/data/samples_deforestation_rondonia.rds"
)

datacube = connection.load_collection(
    collection_id="mpc-sentinel-2-l2a",
    spatial_extent={
        "west": -63.50,
        "south": -8.92,
        "east": -63.35,
        "north": -8.78,
    },
    temporal_extent=["2022-01-01", "2022-12-31"],
    bands=[
        "b02", "b03", "b04", "b05", "b06", "b07", "b08",
        "b11", "b12", "b8a",
    ],
)

datacube = datacube.process(
    process_id="cube_regularize",
    arguments={
        "data": datacube,
        "period": "P16D",
        "resolution": 300,
    },
)

tempcnn_model_init = connection.mlm_class_tempcnn(
    optimizer="adam",
    learning_rate=5e-4,
    seed=42,
    epochs=20,
    batch_size=64,
    verbose=True,
)

tempcnn_model = tempcnn_model_init.fit(
    training_set=rondonia_url,
    target="label",
)

datacube = tempcnn_model.predict(datacube)

result = datacube.save_result(format="GeoTiff")

job = result.create_job(
    title="Rondonia TempCNN",
    description="Train on sits sample URL; predict GeoTiff",
)
job.start_and_wait()
job.get_results().download_files("output")
```

**Other workflows:** Random forest + Breizh-style alignment (constants shared with R) live in `inst/examples/breizh_openeo_training_predict_aligned.py`. Step-by-step notebooks: `inst/demo-lps-2025/`.


