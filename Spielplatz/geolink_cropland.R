pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf, geodata, units)

geolink_cropland <- function(source = "WorldCover",
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
  unlink(tempdir(), recursive = TRUE)

  raster_objs <- geodata::cropland(source = source, path = tempdir())

  name_set <- "cropland"

  epsg_4326 <- "+init=EPSG:4326"

  terra::crs(raster_objs) <- epsg_4326
    if (is.null(crs(raster_objs))) {
      print("Projection failed for raster")
    } else {
      print(paste("Raster projected successfully."))
    }

  raster_list <- as.list(raster_objs)

  print("WorldCover Raster Downloaded")

  df <- postdownload_processor(shp_dt = shp_dt,
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

  return(df)}



df <- geolink_cropland(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])

test_dt <- geolink_cropland( survey_dt =  st_as_sf(hhgeo_dt[1:10,],
                                                   crs = 4326),
                             buffer_size = 1000)
test_dt <- geolink_cropland(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])






