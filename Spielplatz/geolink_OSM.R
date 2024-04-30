if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")

#load packages
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)



q <- getbb("Madrid") %>%
  opq() %>%
  add_osm_feature("amenity", "cinema")

str(q)


cinema <- osmdata_sf(q)

housing <- cinema$osm_points

library(raster)

r <- raster(extent(cinema$osm_points), res = 0.01)

cinema$osm_points$constant_value <- 1

value_column <- "constant_value"

housing_raster <- rasterize(cinema$osm_points, r, field = value_column, fun = "sum")

housing_raster[housing_raster == 0] <- NA

count <- length(cinema$osm_points$osm_id)





