library(openeo)
con <- connect("http://127.0.0.1:8001")
p <- processes()
login("rolf", "123456")

x <- p$save_result(
    data = p$reduce_dimension(
        data = p$reduce_dimension(
            data = p$load_collection(
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
            reducer = p$multiply(
                x = 2.5,
                y = p$divide(
                    x = p$subtract(
                        x = p$array_element(data = data, label = "B08"),
                        y = p$array_element(data = data, label = "B04")
                    ),
                    y = p$sum(
                        data = list(
                            1L,
                            p$array_element(data = data, label = "B08"),
                            p$multiply(
                                x = 6L,
                                y = p$array_element(
                                    data = data,
                                    label = "B04"
                                )
                            ),
                            p$multiply(
                                x = -7.5,
                                y = p$array_element(
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
        reducer = p$min(data = data)
    ),
    format = "GTiff"
)

compute_result(x, output_file = "example.tif")
