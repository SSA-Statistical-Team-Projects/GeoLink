
geolink_opencellid <- function(shp_dt,
                               shp_fn = NULL,
                               grid_size = 1000,
                               survey_dt,
                               survey_fn = NULL,
                               survey_lat = NULL,
                               survey_lon = NULL,
                               buffer_size = NULL,
                               extract_fun = "mean",
                               survey_crs = 4326){

  if (!is.null(shp_dt)) {
    coords <- st_coordinates(shp_dt)
    latmin <- min(coords[, "Y"])
    lonmin <- min(coords[, "X"])
    latmax <- max(coords[, "Y"])
    lonmax <- max(coords[, "X"])
  } else if (!is.null(shp_fn)) {
    shp_dt <- st_read(shp_fn)
    coords <- st_coordinates(shp_dt)
    latmin <- min(coords[, "Y"])
    lonmin <- min(coords[, "X"])
    latmax <- max(coords[, "Y"])
    lonmax <- max(coords[, "X"])
  } else {
    stop("Provide either shp_dt or shp_fn.")
  }


  key = "pk.b6cb635812d221e898b979b1a6440b22"

  url <- paste0("http://opencellid.org/cell/getInArea?key=",key, "&BBOX=", latmin, ",", lonmin, ",", latmax, ",", lonmax,"&format=csv")

  timeout_seconds <- 7200

  response <- GET(url, timeout(timeout_seconds))

  # Check if the request was successful
  if (status_code(response) == 200) {
    # Write the content to a CSV file in a temporary directory
    content <- content(response, "text")
    temp_file <- file.path(tempdir(), "data.csv")
    writeLines(content, temp_file)
    print(paste("Data downloaded successfully and saved to", temp_file))

    # Read the CSV file into a data.table
    dt <- fread(temp_file)

  } else {
    print(paste("Failed to download data. Status code:", status_code(response)))
  }


  return(dt)

}
