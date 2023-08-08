
### test some functions

shp_dt <- readRDS("data/shapefile.RDS")

hhsample_dt <- readRDS("data/hhsample.RDS")


### testing pull chirps data with geolink_chirps()

shp_dt$area <- units::set_units(sf::st_area(shp_dt), "km^2")


### figuring out which areas of use correspond to each group
dt <- rgdal::make_EPSG()

testraster <- raster("//esapov/esapov/RWA/GEO/Population/WorldPop/sle_ppp_2020_UNadj.tif")

test <-
geolink_chirps(time_unit = "month",
               shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
               grid_size = 1000,
               grid = TRUE,
               use_survey = TRUE,
               survey_dt = st_as_sf(hhsample_dt[, c("hhid", "geometry")],
                                    crs = 4326,
                                    agr = "constant"))
