# Scale a rectangular lon/lat bounding box to a target *area fraction*
# while keeping the same center and aspect ratio (both axes scaled by sqrt(f)).

#' @param west,east,south,north Original bounds (degrees).
#' @param area_fraction Target area as a fraction of the original (e.g. 2/7).
#' @return A list(west, east, south, north) for openEO `spatial_extent`.
scaled_spatial_extent <- function(west,
                                  east,
                                  south,
                                  north,
                                  area_fraction = 2 / 7) {
  if (area_fraction <= 0 || area_fraction > 1) {
    stop("area_fraction must be in (0, 1].", call. = FALSE)
  }
  cx <- (west + east) / 2
  cy <- (south + north) / 2
  half_w <- (east - west) / 2
  half_h <- (north - south) / 2
  s <- sqrt(area_fraction)
  list(
    west = cx - half_w * s,
    east = cx + half_w * s,
    south = cy - half_h * s,
    north = cy + half_h * s
  )
}

# Original ~1° x 1° Rondônia-style box (from UC2-style scripts).
original <- list(
  west = -63.9,
  east = -62.9,
  south = -9.14,
  north = -8.14
)

spatial_extent_2_7_area <- scaled_spatial_extent(
  original$west,
  original$east,
  original$south,
  original$north,
  area_fraction = 2 / 7
)

# When `source()`ing this file, print the result.
print(spatial_extent_2_7_area)
