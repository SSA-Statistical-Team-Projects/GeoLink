## code to prepare `DATASET` dataset goes here

hhgeo_dt <- fread("data-raw/nga_householdgeovars_y4.csv")

shp_dt <- sf::st_read(dsn = "data-raw/shapefiles",
                      layer = "nga_admbnda_adm2_osgof_20190417")


hhgeo_dt <- sf::st_as_sf(hhgeo_dt,
                         coords = c("lon_dd_mod", "lat_dd_mod"),
                         crs = 4326)

shp_dt <- shp_dt[, c("ADM0_EN", "ADM0_PCODE", "ADM1_EN",
                     "ADM1_PCODE", "ADM2_EN", "ADM2_PCODE")]

hhgeo_dt <- sf::st_join(hhgeo_dt, shp_dt)

hhgeo_dt <- as.data.table(hhgeo_dt)


#### save the household geocoded survey and the shapefile
saveRDS(hhgeo_dt, "data/hhsample.RDS")
saveRDS(shp_dt, "data/shapefile.RDS")
