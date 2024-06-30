library(sf)
library(data.table)
library(httr)

geolink_opencellid <- function(shp_dt,
                               shp_fn = NULL,
                               grid_size = 1500,
                               survey_dt,
                               survey_fn = NULL,
                               survey_lat = NULL,
                               survey_lon = NULL,
                               buffer_size = NULL,
                               extract_fun = "mean",
                               survey_crs = 4326) {

  max_attempts = 3

  call_opencellid_api <- function(latmin, lonmin, latmax, lonmax, temp_file, attempt = 1) {
    key <- "pk.b6cb635812d221e898b979b1a6440b22"
    url <- paste0("http://opencellid.org/cell/getInArea?key=", key, "&BBOX=",
                  latmin, ",", lonmin, ",", latmax, ",", lonmax, "&format=csv")
    timeout_seconds <- 720000000
    response <- GET(url, timeout(timeout_seconds))

    if (status_code(response) == 200) {
      content <- content(response, "text")
      # Open file in append mode and write content
      con <- file(temp_file, "a")  # "a" for append mode
      writeLines(content, con)
      close(con)  # Close the file connection
      print(paste("Data downloaded successfully for BBOX:", latmin, lonmin, latmax, lonmax))
    } else {
      if (attempt < max_attempts) {
        Sys.sleep(5)  # Wait for 5 seconds before retrying
        print(paste("Retrying API call for BBOX:", latmin, lonmin, latmax, lonmax))
        recall <- Recall(latmin, lonmin, latmax, lonmax, temp_file, attempt + 1)
        return(recall)
      } else {
        print(paste("Failed to download data for BBOX:", latmin, lonmin, latmax, lonmax))
      }
    }
  }

  # Function to calculate bounding box coordinates
  get_bounding_box <- function(coords) {
    latmin <- min(coords[, "Y"])
    lonmin <- min(coords[, "X"])
    latmax <- max(coords[, "Y"])
    lonmax <- max(coords[, "X"])
    return(list(latmin = latmin, lonmin = lonmin, latmax = latmax, lonmax = lonmax))
  }

  # Read shapefile if provided
  if (!is.null(shp_dt)) {
    coords <- st_coordinates(shp_dt)
    bbox <- get_bounding_box(coords)
  } else if (!is.null(shp_fn)) {
    shp_dt <- st_read(shp_fn)
    coords <- st_coordinates(shp_dt)
    bbox <- get_bounding_box(coords)
  } else {
    stop("Provide either shp_dt or shp_fn.")
  }

  # Initialize temp file for saving API responses
  temp_file <- file.path(tempdir(), "data.csv")
  if (file.exists(temp_file)) file.remove(temp_file)  # Remove if it exists

  # Define grid size (in degrees)
  grid_size_deg <- grid_size / 111000  # Convert meters to degrees (approximation at equator)

  # Calculate number of grid cells needed to cover the entire area
  lat_range <- seq(bbox$latmin, bbox$latmax, by = grid_size_deg)
  lon_range <- seq(bbox$lonmin, bbox$lonmax, by = grid_size_deg)

  # Iterate over grid cells and call OpenCellID API
  for (lat_start in lat_range) {
    lat_end <- lat_start + grid_size_deg
    for (lon_start in lon_range) {
      lon_end <- lon_start + grid_size_deg
      call_opencellid_api(lat_start, lon_start, lat_end, lon_end, temp_file)
    }
  }

  # Read the CSV file into a data.table
  dt <- fread(temp_file)

  # Remove duplicate records based on all columns
  dt <- unique(dt)

  return(dt)
}

# Example usage
data <- geolink_opencellid(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
