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

# Check specific process
curl http://127.0.0.1:8000/processes | jq '.processes[] | select(.id == "merge_cubes")'
```

### From R client:
```r
library(openeo)
connection <- connect("http://127.0.0.1:8000")
p <- processes()

# Check if process exists
"merge_cubes" %in% names(p)
```

## Adding New Processes

1. Add function to `inst/ml/processes.R` with `#* @openeo-process` decorator:
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

