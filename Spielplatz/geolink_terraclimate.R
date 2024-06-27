geolink_terraclimate <- function(var,
                                 year,
                              shp_dt = NULL,
                              shp_fn = NULL,
                              grid_size = 1000,
                              survey_dt,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              extract_fun = "mean",
                              survey_crs = 4326){

  unlink(tempdir(), recursive = TRUE)

  # Generate URL
  url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_", var, "_", year, ".nc")

  # Extract the filename from the URL
  filename <- basename(url)

  # Create the destination path
  destination_dir <- tempdir()
  destination <- file.path(destination_dir, filename)

  # Ensure the temporary directory exists
  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
  }

  # Set the timeout
  timeout_seconds <- 240

  # Print the URL for debugging purposes
  print(paste("URL:", url))

  # Perform the GET request
  response <- try(GET(url, timeout(timeout_seconds)), silent = TRUE)

  # Check if the GET request was successful
  if (inherits(response, "try-error")) {
    print("Error performing the GET request.")
  } else if (http_status(response)$category == "Success") {
    # Write the content to a file if the status code is 200
    tryCatch({
      writeBin(content(response, "raw"), destination)
      print("File downloaded successfully.")
      print(paste("File saved to:", destination))
    }, error = function(e) {
      print(paste("Error writing the file:", e$message))
    })
  } else {
    # Print the error status
    print(paste("Error downloading the file. Status code:", http_status(response)$status_code))
  }

  if (http_status(response)$status_code == 200) {
    writeBin(content(response, "raw"), destination)
    print("File downloaded successfully.")
  } else {
    print("Error downloading the file.")
  }

  raster_stack <- stack(destination)

  name_set <- paste0(var, "_")

  print("Terraclimate Raster Downloaded")

  dt <- postdownload_processor(shp_dt = shp_dt,
                               raster_objs = raster_stack,
                               shp_fn = shp_fn,
                               grid_size = grid_size,
                               survey_dt = survey_dt,
                               survey_fn = survey_fn,
                               survey_lat = survey_lat,
                               survey_lon = survey_lon,
                               extract_fun = extract_fun,
                               buffer_size = buffer_size,
                               survey_crs = survey_crs,
                               name_set = name_set)

  print("Process Complete!!!")

  return(dt)}

df <- geolink_terraclimate(var ="tmax", year ="2015", shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])





