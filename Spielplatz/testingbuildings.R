temp_dir <- tempdir()
iso_code = "COM"


  url <- paste0("https://data.worldpop.org/repo/wopr/_MULT/buildings/v1.1/", iso_code, "_buildings_v1_1.zip")
  tryCatch({
    # Download the ZIP file
    response <- GET(url, write_disk(file.path(tempdir(), basename(url)), overwrite = TRUE))
    if (http_type(response) == "application/zip") {
      message("File downloaded successfully.")

      # Unzip the downloaded file
      unzip(file.path(tempdir(), basename(url)), exdir = tempdir())
      message("File unzipped successfully.")
    } else {
      warning("Downloaded file may not be a ZIP file.")
    }
  }, error = function(e) {
    print(e)
  })

  tif_files <- list.files(temp_dir, pattern = "\\.tif$", full.names = TRUE)

  raster_objs <- lapply(tif_files, terra::rast)

  raster_list <- lapply(raster_objs, raster)


  for (i in seq_along(raster_list)) {
    raster_proj <- projectRaster(raster_list[[i]], crs = crs(shp_dt), method = 'ngb')
    if (is.null(raster_proj)) {
      print(paste("Projection failed for raster", i))
    } else {
      raster_list[[i]] <- raster_proj
      print(paste("Raster", i, "projected successfully."))
    }
  }



























