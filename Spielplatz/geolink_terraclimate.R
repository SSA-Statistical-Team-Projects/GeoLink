pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf, geodata, httr, ncdf4, rgdal, exactextractr)

geolink_worldclim <- function(iso_code,
                              var,
                              res,
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


  if(!is.null(iso_code)){
    print(paste("Checking data for", iso_code))
  } else{
    stop("Please input a valid country Name or ISO3 Code")
  }

  unlink(tempdir(), recursive = TRUE)

  destination_wc <- tempdir()

  raster_file <- geodata::worldclim_country(country = iso_code, version = "2.1",
                                            var = var, res = res, path = destination_wc)

  tif_files <- list.files(destination_wc, pattern = "\\.tif$", full.names = TRUE,
                          recursive = TRUE)

  rasters_combined <- terra::rast(tif_files)

  raster_list <- lapply(1:terra::nlyr(rasters_combined), function(i) rasters_combined[[i]])


  name_set <- c()

  num_layers <- terra::nlyr(rasters_combined)
  print(raster_list)

  months <- month.abb
  print(months)


  name_set <- paste0(iso_code,"_WC_", var, "_", months)

  print("WorldClim Raster Downloaded")

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

df <- geolink_terraclimate(var ="tmin", year = 2017, shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])

