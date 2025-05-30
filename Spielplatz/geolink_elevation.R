
pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf, geodata, units)


geolink_elevation <- function(country_name = "",
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

  if(!is.null(country_name)){
    print(paste("Checking data for", country_name))
  } else{
    stop("Please input a valid country Name or ISO3 Code")
  }

  unlink(tempdir(), recursive = TRUE)

  data <- geodata::elevation_30s(country = country_name, path=tempdir())

  tif_files <- list.files(tempdir(), pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

  name_set <- c()

  for (file in tif_files) {
    base_name <- basename(file)

    extracted_string <- sub("\\.tif$", "", base_name)

    name_set <- c(name_set, extracted_string)
  }

  raster_list <- lapply(tif_files, terra::rast)


  epsg_4326 <- "+init=EPSG:4326"

  for (i in seq_along(raster_list)) {
    terra::crs(raster_list[[i]]) <- epsg_4326
    if (is.null(terra::crs(raster_list[[i]]))) {
      print(paste("Projection failed for raster", st_crs(raster_list[[i]])$input))
    } else {
      print(paste("Raster", i, "projected successfully."))
    }
  }

  print("Elevation Raster Downloaded")


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





test_dt <- geolink_elevation(country_name = "NGA",
                             survey_dt =  st_as_sf(hhgeo_dt[1:10,],
                                                   crs = 4326),
                             buffer_size = 1000)


test_dt <- geolink_elevation(country_name = "Nigeria",
                             shp_dt[shp_dt$ADM1_EN == "Abia",])






