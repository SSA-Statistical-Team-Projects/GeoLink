
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


  url <- "https://datacatalogfiles.worldbank.org/ddh-published/0038043/DR0046250/opencellid_global_1km_int.tif?versionId=2023-01-18T19:13:42.1710620Z"

  destination <- file.path(tempdir(), "opencellid_global_1km_int.tif")

  timeout_seconds <- 7200

  response <- GET(url, timeout(timeout_seconds)) ##check to see if response is tiff?

  tif_files <- list.files(tempdir(), pattern = "\\.tif$", full.names = TRUE)

  raster_objs <- lapply(tif_files, terra::rast)

  raster_list <- lapply(raster_objs, raster)

  epsg_4326 <- "+init=EPSG:4326"

  for (i in seq_along(raster_list)) {
    projection(raster_list[[i]]) <- epsg_4326
    if (is.null(projection(raster_list[[i]]))) {
      print(paste("Projection failed for raster", i))
    } else {
      print(paste("Raster", i, "projected successfully."))
    }
  }

  name_set <- paste0("opencellid_")

  print("Opencellid Raster Downloaded")


  dt <- postdownload_processor(shp_dt = shp_dt,
                               raster_objs = raster_list,
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

  return(dt)

}

