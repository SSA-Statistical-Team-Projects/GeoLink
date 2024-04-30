geolink_terraclimate <- function(var,
                                 year,
                              shp_dt,
                              shp_fn = NULL,
                              grid_size = 1000,
                              survey_dt,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              extract_fun = "mean",
                              survey_crs = 4326){

  url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_", var, "_", year, ".nc")

  filename <- basename(url)

  destination <- file.path(tempdir(), filename)

  timeout_seconds <- 240

  response <- GET(url, timeout(timeout_seconds))

  if (http_status(response)$status_code == 200) {
    writeBin(content(response, "raw"), destination)
    print("File downloaded successfully.")
  } else {
    print("Error downloading the file.")
  }


  ## add in code to convert from .nc to .tif




  raster_objs <- lapply(tif_files, terra::rast)

  raster_list <- lapply(raster_objs, raster)

  name_set <- paste0(var, "_")

  print("Terraclimate Raster Downloaded")

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

  return(dt)}

df <- geolink_terraclimate(var ="PDSI", year ="2016", shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])


