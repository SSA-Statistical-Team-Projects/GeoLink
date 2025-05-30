################################################################################
##################### TESTING THE GEOLINK CHIRPS FUNCTION ######################
################################################################################

### read in the data

fpath <- system.file("extdata", "pop.tif", package = "GeoLink")
pop_raster <- terra::rast(fpath)

chirps_list <- list.files(path = "tests/testthat/testdata",
                          pattern = "^chirps.*\\.tif$",
                          full.names = TRUE) %>%
  lapply(X = .,
         FUN = terra::rast)

# raster download to the extent of a shapefile
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
                     extract_fun = "weighted_mean",
                     weight_raster = pop_raster)

df2 <- geolink_chirps(time_unit = "month",
                     start_date = "2020-01-01",
                     end_date = "2020-03-01",
                     shp_fn = "tests/testthat/testdata/shp_dt.shp",
                     grid_size = 1000,
                     extract_fun = "weighted_mean",
                     weight_raster = pop_raster)

df6 <- geolink_chirps(time_unit = "month",
                     start_date = "2020-01-01",
                     end_date = "2020-03-01",
                     survey_dt = hhgeo_dt[hhgeo_dt$ADM1_PCODE == "NG001",],
                     buffer_size = 1000,
                     extract_fun = "weighted_mean",
                     weight_raster = pop_raster)

df5 <- geolink_chirps(time_unit = "month",
                     start_date = "2020-01-01",
                     end_date = "2020-03-01",
                     survey_fn = "tests/testthat/testdata/xy_hhgeo_dt.dta",
                     survey_lon = "x",
                     survey_lat = "y",
                     buffer_size = 1000,
                     extract_fun = "weighted_mean",
                     weight_raster = pop_raster)





#### lets map the plot



> fpath <- system.file("extdata", "pop.tif", package = "GeoLink")
> pop_raster <- terra::rast(fpath)
> df6 <- geolink_chirps(time_unit = "month",
                        +                      start_date = "2020-01-01",
                        +                      end_date = "2020-03-01",
                        +                      survey_dt = hhgeo_dt[hhgeo_dt$ADM1_PCODE == "NG001",],
                        +                      buffer_size = 1000,
                        +                      extract_fun = "weighted_mean",
                        +                      weight_raster = pop_raster)
trying URL 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/chirps-v2.0.2020.01.tif.gz'
Content type 'application/octet-stream' length 14474246 bytes (13.8 MB)
==================================================
  downloaded 13.8 MB

trying URL 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/chirps-v2.0.2020.02.tif.gz'
Content type 'application/octet-stream' length 14550346 bytes (13.9 MB)
==================================================
  downloaded 13.9 MB

trying URL 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/chirps-v2.0.2020.03.tif.gz'
Content type 'application/octet-stream' length 14504705 bytes (13.8 MB)
==================================================
  downloaded 13.8 MB

[1] "Global Rainfall Raster Downloaded"
[1] "Process Complete!!!"
Warning message:
  In value[[3L]](cond) :
  Error preparing survey data: Your dataset is missing an existing CRS definition.
Either assign an appropriate CRS to your dataset or find one with
the crsuggest::guess_crs() function.

>


