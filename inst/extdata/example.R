expr <- quote(
    save_result(
        data = reduce_dimension(
            data = reduce_dimension(
                data = load_collection(
                    id = "Sentinel-2",
                    spatial_extent = list(
                        west = 16.1,
                        east = 16.6,
                        north = 48.6,
                        south = 47.2
                    ),
                    temporal_extent = list(
                        "2018-01-01",
                        "2018-02-01"
                    ),
                    bands = list(
                        "B02",
                        "B04",
                        "B08"
                    )
                ),
                dimension = "bands",
                reducer = multiply(
                    x = 2.5,
                    y = divide(
                        x = subtract(
                            x = array_element(data = data, label = "B08"),
                            y = array_element(data = data, label = "B04")
                        ),
                        y = sum(
                            data = list(
                                1L,
                                array_element(data = data, label = "B08"),
                                multiply(
                                    x = 6L,
                                    y = array_element(
                                        data = data,
                                        label = "B04"
                                    )
                                ),
                                multiply(
                                    x = -7.5,
                                    y = array_element(
                                        data = data,
                                        label = "B02"
                                    )
                                )
                            )
                        )
                    )
                )
            ),
            dimension = "t",
            reducer = min(data = data)
        ),
        format = "GTiff"
    )
)

create_procs("R/procs.R")
eval(expr, envir = get_procs())
