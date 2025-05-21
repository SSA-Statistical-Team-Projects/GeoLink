################################################################################
##################### TESTING THE GEOLINK CHIRPS FUNCTION ######################
################################################################################

### read in the data

fpath <- system.file("extdata", "pop.tif", package = "GeoLink")
pop_raster <- terra::rast(fpath)

## raster download to the extent of a shapefile
raster_obj <-
geolink_chirps(time_unit = "month",
               start_date = "2020-01-01",
               end_date = "2020-03-01",
               shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
               return_raster = TRUE)

#examples of zonal statistics computation for a tesselated shapefile
df <- geolink_chirps(time_unit = "month",
                     start_date = "2020-01-01",
                     end_date = "2020-03-01",
                     shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                     grid_size = 1000,
                     extract_fun = "mean")

df <- geolink_chirps(time_unit = "month",
                     start_date = "2020-01-01",
                     end_date = "2020-03-01",
                     shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                     grid_size = 1000,
                     extract_fun = "mean",
                     weight_raster = pop_raster)
