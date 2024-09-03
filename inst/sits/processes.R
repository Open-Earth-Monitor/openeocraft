# TODO allow many files processes definition
#* @openeo-import math.R
#* @openeo-import data.R

.data_timeline <- function(data) {
  stars::st_get_dimension_values(data, "t")
}

.data_bands <- function(data) {
  bands <- "default"
  if ("bands" %in% base::names(stars::st_dimensions(data)))
    bands <- stars::st_get_dimension_values(data, "bands")
  bands
}

.data_get_time <- function(data, time) {
  i <- base::match(time, .data_timeline(data))
  if (any(is.na(i))) {
    time <- paste0(time[is.na(i)], collapse = ",")
    openeocraft::api_stop(404, "Time(s) ", time, " not found")
  }
  if ("bands" %in% base::names(stars::st_dimensions(data)))
    return(data[,,,i])
  data[,,i]
}

.data_get_band <- function(data, band) {
  j <- match(band, .data_bands(data))
  if (any(is.na(j))) {
    band <- paste0(band[is.na(j)], collapse = ",")
    openeocraft::api_stop(404, "Band(s) ", band, " not found")
  }
  dplyr::select(data, dplyr::all_of(j))
}

#* @openeo-process
save_result <- function(data, format, options = NULL) {
  format <- base::tolower(format)
  supported_formats <- openeocraft::file_formats()
  outputFormats <- supported_formats$output
  if (!(format %in% base::tolower(base::names(outputFormats)))) {
    stop(base::paste("Format", format, "is not supported."))
  }
  # TODO: split data object into different files based on dates and
  #  bands dimensions.
  #  The number of output files will be: N(dates) * N(bands)
  #  The format parameter just defines the file type to be saved in
  #  this process.
  api <- openeocraft::current_api()
  user <- openeocraft::current_user()
  job <- openeocraft::current_job()
  req <- openeocraft::current_request()
  job_dir <- openeocraft::job_get_dir(api, user, job$id)
  host <- openeocraft::get_host(api, req)
  times <- .data_timeline(data)
  bands <- .data_bands(data)
  # TODO implement generic function not relying on an specific data type
  results <- base::list()
  for (i in base::seq_along(times)) {
    assets_file <- base::lapply(base::seq_along(bands), \(j) {
      asset_file <- base::paste0(base::paste(bands[[j]], times[[i]], sep = "_"),
                                 openeocraft::format_ext(format))
      stars::write_stars(.data_get_band(.data_get_time(data, i), j),
                         base::file.path(job_dir, asset_file))
      asset_file
    })
    # TODO: implement asset tokens
    assets <- base::lapply(assets_file, \(asset_file) {
      asset_path <- base::file.path("/files/jobs", job$id, asset_file)
      base::list(
        href = openeocraft::make_url(
          host = host,
          asset_path,
          token = base64enc::base64encode(base::charToRaw(user))
        ),
        # TODO: implement format_content_type() function
        type = openeocraft::format_content_type(format),
        roles = base::list("data")
      )
    })
    base::names(assets) <- base::paste0(bands, "_", times[[i]])
    results <- base::c(results, assets)
  }
  # TODO: where to out assets? links? assets?
  collection <- openeocraft::job_empty_collection(api, user, job)
  collection$assets <- results
  jsonlite::write_json(
    x = collection,
    path = base::file.path(job_dir, "_collection.json"),
    auto_unbox = TRUE
  )

  return(TRUE)
}

#* @openeo-process
reduce_dimension <- function(data, reducer, dimension, context = NULL) {
  reducer_fn <- function(data, context) {}
  base::body(reducer_fn) <- base::substitute(reducer)
  if (base::is.character(dimension))
    dimension <- base::match(dimension, base::dimnames(data))
  dims <- base::setdiff(base::seq_along(base::dimnames(data)), dimension)
  dimnames <- stars::st_dimensions(data)[[dimension]]$values
  stars::st_apply(data, dims, \(x) {
    base::names(x) <- dimnames
    reducer_fn(x, context = context)
  })
}

#* @openeo-process
load_collection <- function(id,
                            spatial_extent = NULL,
                            temporal_extent = NULL,
                            bands = NULL,
                            properties = NULL) {
  mock_img <- function(bands, size, origin, res, ...) {
    data <- base::data.frame(
      x = base::rep(
        base::rep(
          base::seq(from = origin[[1]], by = res[[1]], length.out = size[[1]]),
          each = size[[2]]
        ),
        times = base::length(bands)
      ),
      y = base::rep(
        base::rep(
          base::seq(from = origin[[2]], by = res[[2]], length.out = size[[2]]),
          times = size[[1]]
        ),
        times = base::length(bands)
      ),
      bands = base::rep(bands, each = size[[1]] * size[[2]])
    )
    extra <- base::as.data.frame(base::list(...))
    if (base::nrow(extra))
      data <- base::cbind(data, extra)
    data$values <- stats::runif(size[[1]] * size[[2]] * base::length(bands))
    data
  }

  spatial_extent <- base::c(
    west = -45.0,
    east = -45.0 * 10 * 0.009,
    south = -10.0 * 10 * -0.009,
    north = -10.0
  )
  temporal_extent <- base::c(
    t0 = "2020-01-01",
    t1 = "2021-01-01"
  )
  bands <- base::c("B02", "B04", "B08")

  data <- stars::st_as_stars(
    base::rbind(
      mock_img(
        bands = bands,
        size = base::c(10, 10),
        origin = base::c(-45.0, -10.0),
        res = base::c(0.009, 0.009),
        t = "2020-01-01"
      ),
      mock_img(
        bands = bands,
        size = base::c(10, 10),
        origin = base::c(-45.0, -10.0),
        res = base::c(0.009, 0.009),
        t = "2021-01-01"
      )
    ),
    dims = 1:4,
    xy = 1:2,
    y_decreasing = TRUE,
    coords = 1:4
  )

  base::split(data, 3)
}

#* @openeo-process
multiply <- function(x, y) {
  x * y
}

#* @openeo-process
divide <- function(x, y) {
  x / y
}

#* @openeo-process
subtract <- function(x, y) {
  x - y
}

#* @openeo-process
add <- function(x, y) {
  x + y
}

#* @openeo-process
array_element <- function(data, index = NULL, label = NULL,
                          return_nodata = FALSE) {
  if (!base::is.null(index))
    return(data[[index]])
  if (!base::is.null(label))
    return(data[[label]])
  data
}

# TODO: implement extended annotations
#* @openeo-process sum
#*
#* This is the title
#*
#* This is the first line of the description
#* This is the second line of the description
#*
#* This is the third line of the description and the
#* previous line is a blank line.
#*
#* @param data string
#* @param ignore_nodata logical
#* @return data_cube
sum <- function(data, ignore_nodata = TRUE) {
  base::sum(base::unlist(data), na.rm = ignore_nodata)
}

#* @openeo-process
min <- function(data, ignore_nodata = TRUE) {
  base::min(base::unlist(data), na.rm = ignore_nodata)
}
