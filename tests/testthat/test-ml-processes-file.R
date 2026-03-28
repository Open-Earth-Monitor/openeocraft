# Real ML process definitions live in inst/ml/processes.R (not the mock used by
# other tests). `parse()` always; `eval()` in the API sandbox matches the first
# step of `load_processes()` without `process_decorators()`, which would try to
# JSON-serialize some non-exportable parameter defaults for new process specs.

test_that("inst/ml/processes.R parses as valid R", {
    f <- system.file("ml/processes.R", package = "openeocraft")
    skip_if(f == "", "inst/ml/processes.R not found")
    exprs <- parse(f, encoding = "UTF-8")
    expect_true(length(exprs) > 0L)
})

test_that("ML processes file eval()s in API sandbox when sits is installed", {
    skip_if_not_installed("sits")
    f <- system.file("ml/processes.R", package = "openeocraft")
    skip_if(f == "", "inst/ml/processes.R not found")
    api <- create_openeo_v1(
        id = "ml-test",
        title = "t",
        description = "t",
        backend_version = "0",
        stac_api = NULL,
        work_dir = tempdir(),
        conforms_to = NULL,
        production = FALSE
    )
    openeocraft:::setup_namespace(api)
    ns <- openeocraft:::get_namespace(api)
    # Sandbox parent is emptyenv(); load_rlang must export primitives used by processes.
    expect_identical(eval(quote(1024^3), envir = ns), 1073741824)
    expect_error(
        eval(parse(f, encoding = "UTF-8"), envir = ns),
        NA
    )
    expect_true(exists("ml_tune_grid", envir = ns, inherits = FALSE))
    expect_true(exists("ml_tune_random", envir = ns, inherits = FALSE))
    expect_true(exists("load_collection", envir = ns, inherits = FALSE))
    loader <- get(".openeocraft_load_samples", envir = ns, inherits = FALSE)
    df <- data.frame(x = 1L)
    expect_identical(loader(df, "training_data"), df)
})
