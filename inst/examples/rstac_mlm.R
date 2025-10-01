# Load the rstac library
library(rstac)

# Define the STAC API URL
url <- "http://localhost:8082/"

# Create an rstac catalog object
catalog <- stac(url)

# Specify search parameters
bbox <- c(-180, -90, 180, 90)
datetime_range <- "2019-01-01T00:00:00Z/2024-12-31T23:59:59Z"
collection <- "ml-models-rs"

# Perform the search
search <- stac(url) %>%
    stac_search(
        collections = collection,
        bbox = bbox,
        datetime = datetime_range
    ) %>%
    get_request()


# Retrieve items
items <- items_fetch(search)

# Print the results
print(items)

# Download assets
rstac::assets_url(items)

rstac::assets_download(
    items = items,
    asset_names = "water-bodies-model-pystac",
    output_dir = "~/Desktop/"
)
