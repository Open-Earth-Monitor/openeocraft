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
    expect_identical(eval(quote(character(0)), envir = ns), base::character(0))
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

    merge_opts <- get(
        ".openeocraft_save_result_merge_options",
        envir = ns,
        inherits = FALSE
    )
    expect_identical(merge_opts(NULL)$merge_mode, "auto")
    expect_identical(merge_opts(list(merge_tiles = TRUE))$merge_mode, "yes")
    expect_identical(merge_opts(list(merge_tiles = "true"))$merge_mode, "yes")
    expect_identical(merge_opts(list(merge_tiles = FALSE))$merge_mode, "no")
    expect_identical(merge_opts(list(merge_tiles = "false"))$merge_mode, "no")
    expect_identical(
        merge_opts(list(target_crs = "EPSG:4326"))$target_crs,
        "EPSG:4326"
    )

    fmt <- get(".openeocraft_format_is_geotiff", envir = ns, inherits = FALSE)
    expect_true(fmt("GTiff"))
    expect_true(fmt("GeoTIFF"))
    expect_false(fmt("netCDF"))

    metrics_fn <- get(
        ".openeocraft_validation_metrics_payload",
        envir = ns,
        inherits = FALSE
    )
    data(samples_l8_rondonia_2bands, package = "sits", envir = environment())
    acc <- sits::sits_kfold_validate(
        samples_l8_rondonia_2bands,
        folds = 3L,
        progress = FALSE
    )
    payload <- metrics_fn(
        acc,
        scoring = c("accuracy", "kappa", "f1", "precision", "recall"),
        method = "kfold",
        folds = 3L
    )
    expect_true("accuracy" %in% names(payload$overall))
    expect_true(length(payload$by_class) > 0L)
    expect_false(is.null(payload$confusion_matrix))
    expect_error(jsonlite::toJSON(payload, auto_unbox = TRUE), NA)

    is_trained <- get(
        ".openeocraft_is_trained_sits_model",
        envir = ns,
        inherits = FALSE
    )
    validate_trained <- get(
        ".openeocraft_validate_trained_model",
        envir = ns,
        inherits = FALSE
    )
    data(samples_l8_rondonia_2bands, package = "sits", envir = environment())
    n <- nrow(samples_l8_rondonia_2bands)
    train <- samples_l8_rondonia_2bands[seq_len(floor(n * 0.8)), ]
    val <- samples_l8_rondonia_2bands[seq(floor(n * 0.8) + 1, n), ]
    trained <- sits::sits_train(train, sits::sits_rfor(num_trees = 50))
    base::attr(trained, "mlm_labels") <- sits:::.samples_labels(train)
    expect_true(is_trained(trained))
    expect_false(is_trained(list(ml_method = sits::sits_rfor, ml_args = list())))
    holdout_acc <- validate_trained(trained, val)
    holdout_payload <- metrics_fn(
        holdout_acc,
        scoring = c("accuracy", "kappa", "f1"),
        method = "holdout"
    )
    expect_true("accuracy" %in% names(holdout_payload$overall))
    expect_error(jsonlite::toJSON(holdout_payload, auto_unbox = TRUE), NA)
})

test_that("load_uploaded_files process loads RDS from workspace paths", {
    candidates <- c(
        "inst/ml/processes.R",
        "../inst/ml/processes.R",
        "../../inst/ml/processes.R"
    )
    f <- candidates[file.exists(candidates)][1]
    skip_if(is.na(f), "inst/ml/processes.R not found")

    api <- create_openeo_v1(
        id = "ml-test-load-uploaded",
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
    expect_error(
        eval(parse(f, encoding = "UTF-8"), envir = ns),
        NA
    )
    expect_true(exists("load_uploaded_files", envir = ns, inherits = FALSE))

    root_dir <- file.path(api_user_workspace(api, "mock_user"), "root", "uploads")
    dir.create(root_dir, recursive = TRUE, showWarnings = FALSE)
    expected <- data.frame(v = c(1L, 2L))
    saveRDS(expected, file.path(root_dir, "cube.rds"))

    req <- mock_req("/", method = "GET")
    env <- create_env(api, "mock_user", list(id = "job-id"), req)

    result <- eval(
        quote(load_uploaded_files(paths = list("./uploads/cube.rds"), format = "RDS")),
        envir = env,
        enclos = ns
    )
    expect_identical(result, expected)

    expect_error(
        eval(
            quote(load_uploaded_files(paths = list("../outside.rds"), format = "RDS")),
            envir = env,
            enclos = ns
        ),
        "FileNotFound"
    )
})
