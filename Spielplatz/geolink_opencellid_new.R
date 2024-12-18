# Load required libraries using pacman
pacman::p_load(sf, raster, geosphere, data.table)


# Combined function to calculate tower stats and return the nearest lat/lon for a polygon
geolink_opencellid <- function(cell_tower_file, shp_dt) {

  # Load the OpenCellID data
  cell_towers <- read_opencellid_data(cell_tower_file)

  # Check if shapefile_input is a file path (character) or an in-memory sf object
  if (is.character(shp_dt)) {
    # Load shapefile if it's a file path
    if (!file.exists(shp_dt)) {
      stop("Shapefile not found at the specified path")
    }
    polygons <- st_read(shp_dt)
  } else if (inherits(shp_dt, "sf")) {
    # Use the sf object directly
    polygons <- shp_dt
  } else {
    stop("Invalid shapefile input: must be a file path or an sf object.")
  }

  # Ensure CRS matches between towers and polygons
  cell_towers_sf <- st_as_sf(cell_towers, coords = c("lon", "lat"), crs = 4326)
  cell_towers_sf <- st_transform(cell_towers_sf, st_crs(polygons))

  # Create a list to store results
  results <- list()

  # Loop through each polygon to calculate stats
  for (i in 1:nrow(polygons)) {
    polygon <- polygons[i, ]

    # Towers within the polygon
    towers_in_polygon <- st_within(cell_towers_sf, polygon, sparse = FALSE)
    num_towers <- sum(towers_in_polygon)

    # Calculate centroid of the polygon
    centroid <- st_centroid(polygon)

    # Calculate nearest tower distance
    if (num_towers > 0) {
      towers_sf <- cell_towers_sf[towers_in_polygon, ]
      distances <- st_distance(centroid, towers_sf, by_element = FALSE)
      nearest_idx <- which.min(distances)
      nearest_distance <- min(distances)

      nearest_lon <- st_coordinates(towers_sf)[nearest_idx, "X"]
      nearest_lat <- st_coordinates(towers_sf)[nearest_idx, "Y"]
    } else {
      nearest_distance <- NA
      nearest_lon <- NA
      nearest_lat <- NA
    }

    # Store results
    results[[i]] <- data.frame(
      polygon_id = i,
      num_towers = num_towers,
      nearest_distance = nearest_distance,
      nearest_lon = nearest_lon,
      nearest_lat = nearest_lat
    )
  }

  # Combine all results into a data frame
  results_df <- do.call(rbind, results)

  return(results_df)
}

# Example usage
# Assume `shp_dt` is a preloaded shapefile object

results <- geolink_opencellid(cell_tower_file = "C:/Users/Diana Jaganjac/Downloads/nigeria.csv.gz",
                              shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])

