# Functionality Description (proposed L3-ML API profile)
# GET /ml_runtimes:
# - ML2: Works with and without authentication
# - ML3: Valid response with at least `default` and `versions` per framework;
#   framework names follow the STAC MLM `mlm:framework` vocabulary
# - ML4: States per version whether training and inference are supported
# - ML5: Lists the `mlm:artifact_type` values that can be loaded and saved
# - ML6: Lists the supported workflow types
# - ML7: Lists the training data formats; `vector_cube` whenever training
# - ML8: Lists accelerators using the STAC MLM `mlm:accelerator` vocabulary

valid_version <- function(...) {
    args <- utils::modifyList(
        list(
            load_artifact_types = "saveRDS",
            save_artifact_types = "saveRDS",
            workflow_types = "feature",
            accelerators = "amd64"
        ),
        list(...)
    )
    do.call(new_ml_runtime_version, args)
}

valid_runtime <- function(...) {
    new_ml_runtime(
        title = "Example",
        versions = list("1.0.0" = valid_version(...))
    )
}

runtimes_doc <- function(api, ...) {
    req <- mock_req("/ml_runtimes", method = "GET", ...)
    api_ml_runtimes(api, req, mock_res())
}

as_json <- function(x) {
    jsonlite::fromJSON(
        jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"),
        simplifyVector = FALSE
    )
}

test_that("GET /ml_runtimes: Empty JSON object when no runtime is set", {
    api <- mock_create_openeo_v1()
    doc <- runtimes_doc(api)
    expect_length(doc, 0L)
    expect_identical(
        as.character(jsonlite::toJSON(doc, auto_unbox = TRUE)),
        "{}"
    )
})

test_that("GET /ml_runtimes: Works with and without authentication", {
    api <- mock_create_openeo_v1()
    set_ml_runtimes(api, list("Example" = valid_runtime()))

    anonymous <- runtimes_doc(api)
    authenticated <- runtimes_doc(
        api,
        HTTP_AUTHORIZATION = "Bearer basic//5aad11e1d49b880a4468e1b252944e22"
    )
    invalid_token <- runtimes_doc(api, HTTP_AUTHORIZATION = "Bearer bogus")

    expect_named(anonymous, "Example")
    expect_identical(authenticated, anonymous)
    expect_identical(invalid_token, anonymous)
})

test_that("GET /ml_runtimes: default and versions per framework", {
    api <- mock_create_openeo_v1()
    runtime <- new_ml_runtime(
        title = "Example",
        versions = list(
            "1.0.0" = valid_version(),
            "2.0.0" = valid_version()
        ),
        default = "2.0.0"
    )
    set_ml_runtimes(api, list("Example" = runtime))
    doc <- as_json(runtimes_doc(api))

    expect_identical(doc$Example$default, "2.0.0")
    expect_named(doc$Example$versions, c("1.0.0", "2.0.0"))
    expect_true(doc$Example$default %in% names(doc$Example$versions))
})

test_that("GET /ml_runtimes > versions: Serializes list fields as arrays", {
    api <- mock_create_openeo_v1()
    set_ml_runtimes(api, list("Example" = valid_runtime()))
    version <- as_json(runtimes_doc(api))$Example$versions[["1.0.0"]]

    expect_identical(version$training, TRUE)
    expect_identical(version$inference, TRUE)
    expect_identical(version$artifact_types$load, list("saveRDS"))
    expect_identical(version$artifact_types$save, list("saveRDS"))
    expect_identical(version$workflow_types, list("feature"))
    expect_identical(version$training_data_formats, list("vector_cube"))
    expect_identical(version$accelerators, list("amd64"))
})

test_that("new_ml_runtime_version: Inference-only runtime is valid", {
    version <- new_ml_runtime_version(
        training = FALSE,
        inference = TRUE,
        load_artifact_types = c("torch.jit.save", "saveRDS"),
        workflow_types = c("time_series", "spatial_patch"),
        accelerators = c("amd64", "cuda")
    )
    expect_false(version$training)
    expect_length(version$training_data_formats, 0L)
    expect_length(version$artifact_types$save, 0L)
})

test_that("new_ml_runtime_version: Rejects values outside the vocabularies", {
    expect_error(valid_version(workflow_types = "pixel"), "workflow_types")
    expect_error(valid_version(workflow_types = character()), "workflow_types")
    expect_error(valid_version(accelerators = "gpu"), "mlm:accelerator")
    expect_error(valid_version(training = NA), "training")
    expect_error(
        valid_version(training = FALSE, inference = FALSE),
        "training, inference or both"
    )
})

test_that("new_ml_runtime_version: Training requires vector_cube and save", {
    expect_error(
        valid_version(training_data_formats = "sits_tibble_rds"),
        "vector_cube"
    )
    expect_error(
        valid_version(save_artifact_types = character()),
        "savable artifact type"
    )
    expect_error(
        valid_version(load_artifact_types = character()),
        "loadable artifact type"
    )
    expect_error(
        new_ml_runtime_version(
            training = FALSE,
            load_artifact_types = "saveRDS",
            training_data_formats = "vector_cube",
            accelerators = "amd64"
        ),
        "must be empty"
    )
})

test_that("new_ml_runtime: default must be a listed version", {
    expect_error(
        new_ml_runtime(
            title = "Example",
            versions = list("1.0.0" = valid_version()),
            default = "9.9.9"
        ),
        "default"
    )
    expect_error(
        new_ml_runtime(title = "Example", versions = list()),
        "versions"
    )
    expect_error(
        new_ml_runtime(title = "", versions = list("1" = valid_version())),
        "title"
    )
})

test_that("set_ml_runtimes: Framework names follow mlm:framework rules", {
    api <- mock_create_openeo_v1()
    expect_error(
        set_ml_runtimes(api, list(" PyTorch" = valid_runtime())),
        "mlm:framework"
    )
    expect_error(
        set_ml_runtimes(api, list(valid_runtime())),
        "mlm:framework"
    )
    expect_error(
        set_ml_runtimes(
            api,
            list("PyTorch" = valid_runtime(), "PyTorch" = valid_runtime())
        ),
        "mlm:framework"
    )
    broken <- valid_runtime()
    broken$versions[["1.0.0"]]$accelerators <- list("tpu")
    expect_error(
        set_ml_runtimes(api, list("PyTorch" = broken)),
        "Invalid ML runtime 'PyTorch'.*version '1.0.0'"
    )
    expect_invisible(set_ml_runtimes(api, list("PyTorch" = valid_runtime())))
    expect_named(get_ml_runtimes(api), "PyTorch")
})

test_that("ml_cpu_accelerator: Maps R platforms to mlm:accelerator", {
    expect_identical(ml_cpu_accelerator("x86_64", "linux-gnu"), "amd64")
    expect_identical(ml_cpu_accelerator("aarch64", "darwin20"), "macos-arm")
    expect_identical(ml_cpu_accelerator("aarch64", "linux-gnu"), character())
})

test_that("sits_ml_runtimes: Declares the bundled sits frameworks", {
    skip_if_not_installed("sits")
    runtimes <- sits_ml_runtimes()
    api <- mock_create_openeo_v1()
    # Must pass the same validation as any user-supplied declaration.
    expect_no_error(set_ml_runtimes(api, runtimes))

    # Keys must equal the `mlm:framework` values save_ml_model writes.
    processes <- readLines(system.file("ml/processes.R", package = "openeocraft"))
    written <- unique(sub(
        '.*mlm_framework = "([^"]+)".*', "\\1",
        grep('mlm_framework = "', processes, value = TRUE)
    ))
    expect_true(all(names(runtimes) %in% written))

    for (framework in names(runtimes)) {
        runtime <- runtimes[[framework]]
        version <- runtime$versions[[runtime$default]]
        expect_true(version$training)
        expect_true(version$inference)
        expect_true("saveRDS" %in% unlist(version$artifact_types$load))
        expect_true("saveRDS" %in% unlist(version$artifact_types$save))
        expect_true("vector_cube" %in% unlist(version$training_data_formats))
        expect_true("sits_tibble_rds" %in% unlist(version$training_data_formats))
    }
    if ("Torch for R" %in% names(runtimes)) {
        torch_version <- runtimes[["Torch for R"]]$versions[[1]]
        expect_true("time_series" %in% unlist(torch_version$workflow_types))
    }
    if ("R CARET" %in% names(runtimes)) {
        caret_version <- runtimes[["R CARET"]]$versions[[1]]
        expect_identical(unlist(caret_version$workflow_types), "feature")
    }
})
