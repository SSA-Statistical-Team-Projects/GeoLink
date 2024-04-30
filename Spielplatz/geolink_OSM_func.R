pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf)


geolink_osm <- function(osm_feature_category,
                        osm_feature_subcategory,
                        location = NULL,
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


  bbox <- st_bbox(shp_dt)

  datapull <- opq(c(bbox = bbox)) %>%
    add_osm_feature(osm_feature_category, osm_feature_subcategory)

  features <- osmdata_sf(datapull)


  if (nrow(features$osm_points) == 0) {
    print("No points of interest")
    return()
  } else {
    results <- raster(extent(features$osm_points), res = 0.01)
  }

  features$osm_points$constant_value <- 1

  value_column <- "constant_value"

  osm_raster <- rasterize(features$osm_points, results, field = value_column, fun = "sum")

  osm_raster[osm_raster == 0] <- NA

  count <- length(features$osm_points$osm_id)

  name_count <- lubridate::year(start_year) - lubridate::year(end_year) + 1

  name_set <- paste0("OSM_", 1:name_count)

  print("Open Street Maps Raster Downloaded")

  dt <- postdownload_processor(shp_dt = shp_dt,
                               raster_objs = osm_raster,
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




df <- geolink_osm(osm_feature_category = "building",
                  osm_feature_subcategory ="houses",
                  shp_dt = shp_dt,
                  shp_fn = NULL,
                  grid_size = 1000,
                  survey_dt = NULL,
                  survey_fn = NULL,
                  survey_lat = NULL,
                  survey_lon = NULL,
                  buffer_size = NULL,
                  extract_fun = "mean",
                  survey_crs = 4326)














osm_feature_category = "amenity"
osm_feature_subcategory ="cinema"
shp_dt = shp_dt

bbox <- st_bbox(shp_dt)

datapull <- opq("Madrid") %>%
  add_osm_feature(osm_feature_category, osm_feature_subcategory)

features <- osmdata_sf(datapull)

if (nrow(features$osm_points) == 0) {
  print("No points of interest")
  return()
} else {
  results <- raster(extent(features$osm_points), res = 0.01)
}


features$osm_points$constant_value <- 1

value_column <- "constant_value"

osm_raster <- rasterize(features$osm_points, results, field = value_column, fun = "sum")

osm_raster[osm_raster == 0] <- NA

count <- length(features$osm_points$osm_id)



