library(dplyr)
library(lubridate)
library(sf)
library(sits)

# Read data cube
west <- -63.63661 #  northern part of rondonia in Brazil
east <- -63.41824
north <- -8.43097
south <- -8.648347

cube_rondonia <- sits_cube(
    source = "AWS",
    collection = "SENTINEL-2-L2A",
    bands = c(
        "B02",
        "B03",
        "B04",
        "B05",
        "B06",
        "B07",
        "B08"
    ),
    roi = c(
        "lat_min" = south,
        "lon_min" = west,
        "lat_max" = north,
        "lon_max" = east
    ),
    start_date = "2022-01-01",
    end_date = "2022-12-31"
)


cube_rondonia_reg <- sits_regularize(
    cube = cube_rondonia,
    period = "P1M",
    res = 100,
    output_dir = "~/tempdir",
    multicores = 8L,
    progress = TRUE
)

cube_rondonia_reg
timeline_cube1 <- sits_timeline(cube_rondonia)
timeline_cube1

sits_bands(cube_rondonia)

# Check cube timeline
timeline_cube <- sits_timeline(cube_rondonia_reg)
timeline_cube


# Load sits tibble Training data
data_deforestation_rondonia <-
    readRDS("./inst/examples/samples_deforestation_rondonia.rds")

summary(data_deforestation_rondonia)

data_deforestation_rondonia <- sits_select(data_deforestation_rondonia,
    bands = c(
        "B02",
        "B03",
        "B04",
        "B05",
        "B06",
        "B07",
        "B08"
    )
)

# Check samples timeline
timeline_samples <- sits_timeline(data_deforestation_rondonia)
timeline_samples

# Function to aggregate time series to monthly frequency
# Function to aggregate time series data to monthly means
aggregate_to_monthly <- function(ts) {
    Index <- NULL # remove warnings

    ts %>%
        mutate(month = floor_date(Index, "month")) %>% # Extract month
        group_by(month) %>%
        summarise(
            across(starts_with("B"), mean, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        rename(Index = month) # Rename back to Index
}

# Apply the function to each row's time_series column
data_deforestation_rondonia <- data_deforestation_rondonia %>%
    mutate(time_series = lapply(time_series, aggregate_to_monthly))

# Check the structure after modification
class(data_deforestation_rondonia)
print(data_deforestation_rondonia)

# Verify the structure of the sits tibble
print(data_deforestation_rondonia)
print(data_deforestation_rondonia$time_series[[1]])

timeline_m <- sits_timeline(data_deforestation_rondonia)
timeline_m

# set the seed to get the same result
set.seed(42)
# Train the  random forest model
rfor_model <- sits_train(
    samples = data_deforestation_rondonia,
    ml_method = sits_rfor(num_trees = 100)
)
# Plot the most important variables of the model
plot(rfor_model)

# Classify data cube to obtain a probability cube
cube_rondonia_reg_probs <- sits_classify(
    data = cube_rondonia_reg,
    ml_model = rfor_model,
    output_dir = "~/tempdir",
    version = "rf-raster",
    multicores = 8,
    memsize = 16
)

plot(cube_rondonia_reg_probs, labels = "Forest", palette = "YlGn")
