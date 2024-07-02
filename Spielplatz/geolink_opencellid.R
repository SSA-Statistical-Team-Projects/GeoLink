library(sf)
library(httr)
library(data.table)

# Function to read shapefile and call OpenCellID API
geolink_opencellid <- function(shp_dt,
                               shp_fn = NULL,
                               shapefile_crs = 4326,
                               grid_size_meters = 1000,
                               key = "pk.b6cb635812d221e898b979b1a6440b22") {

  max_attempts <- 3

  call_opencellid_api <- function(latmin, lonmin, latmax, lonmax, temp_file, attempt = 1) {
    key <- key
    url <- paste0("http://opencellid.org/cell/getInArea?key=", key, "&BBOX=",
                  latmin, ",", lonmin, ",", latmax, ",", lonmax, "&format=csv")
    timeout_seconds <- 7200
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

  # Ensure the CRS is defined
  if (is.na(st_crs(shp_dt))) {
    st_crs(shp_dt) <- 4326  # Assign a default CRS (WGS 84) if none is defined
  }

  # Transform the shapefile to CRS 4326 if it is not already in that CRS
  if (st_crs(shp_dt)$epsg != 4326) {
    shp_dt <- st_transform(shp_dt, crs = 4326)
  }

  # Get the bounding box in the target CRS coordinates
  bbox <- st_bbox(shp_dt)

  # Convert grid size from meters to degrees (approximation)
  grid_size_lat_degrees <- grid_size_meters / 111320  # Latitude degrees (approx.)

  # Initialize temp file for saving API responses
  temp_file <- file.path(tempdir(), "data.csv")
  if (file.exists(temp_file)) file.remove(temp_file)  # Remove if it exists

  # Loop through each grid cell and call the API
  latmin <- bbox["ymin"]
  while (latmin < bbox["ymax"]) {
    lonmin <- bbox["xmin"]
    while (lonmin < bbox["xmax"]) {
      latmax <- latmin + grid_size_lat_degrees
      if (latmax > bbox["ymax"]) latmax <- bbox["ymax"]

      # Calculate longitude degree size based on the latitude
      grid_size_lon_degrees <- grid_size_meters / (111320 * cos(latmin * pi / 180))
      lonmax <- lonmin + grid_size_lon_degrees
      if (lonmax > bbox["xmax"]) lonmax <- bbox["xmax"]

      # Call the API for the current grid cell
      call_opencellid_api(latmin, lonmin, latmax, lonmax, temp_file)

      lonmin <- lonmax
    }
    latmin <- latmax
  }

  # Read the CSV file into a data.table
  dt <- fread(temp_file)

  return(dt)
}



