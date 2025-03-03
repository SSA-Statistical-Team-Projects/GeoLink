## code to prepare `DATASET` dataset goes here

hhgeo_dt <- fread("data-raw/nga_householdgeovars_y4.csv")


haven::write_dta(data = hhgeo_dt,
                 path = "data-raw/nga_househldgeovars_y4.dta")

shp_dt <- sf::st_read(dsn = "data-raw/shapefiles",
                      layer = "nga_admbnda_adm2_osgof_20190417")


hhgeo_dt <- sf::st_as_sf(hhgeo_dt,
                         coords = c("lon_dd_mod", "lat_dd_mod"),
                         crs = 4326)

shp_dt <- shp_dt[, c("ADM0_EN", "ADM0_PCODE", "ADM1_EN",
                     "ADM1_PCODE", "ADM2_EN", "ADM2_PCODE")]

hhgeo_dt <- sf::st_join(hhgeo_dt, shp_dt)

hhgeo_dt <- as.data.table(hhgeo_dt)


# #### save the household geocoded survey and the shapefile
# saveRDS(hhgeo_dt, "data/hhsample.RDS")
# saveRDS(shp_dt, "data/shapefile.RDS")

# Define bounding box for Nigeria (approximate)
xmin <- 2.69  # Westernmost longitude
xmax <- 14.62 # Easternmost longitude
ymin <- 4.27  # Southernmost latitude
ymax <- 13.89 # Northernmost latitude

# Generate 1000 random points within Nigeria's bounding box
set.seed(123)  # For reproducibility
n <- 1000
longitude <- runif(n, xmin, xmax)
latitude <- runif(n, ymin, ymax)

# Create an sf data frame
point_dt <- data.frame(
  id = 1:n,
  lon = longitude,
  lat = latitude,
  value = rnorm(n, mean = 50, sd = 10) # Some random variable
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  # WGS 84 CRS


usethis::use_data(hhgeo_dt)
usethis::use_data(shp_dt)
usethis::use_data(point_dt)



