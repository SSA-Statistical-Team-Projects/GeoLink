pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf)


geolink_get_poi <- function(osm_feature_category,
                        osm_feature_subcategory,
                        shp_dt,
                        shp_dsn = NULL,
                        buffer = NULL,
                        stata = FALSE){

  if (!is.null(shp_dsn)) {
    shp_dt <- st_read(shp_dsn)
  }


  bbox <- create_query_bbox(shp_dt = shp_dt,
                          area_name = NULL,
                          buffer_dist = c(0, 0, 0, 0),
                          metric_crs = FALSE,
                          osm_crs = 4326)

  datapull <- opq(c(bbox = bbox)) %>%
    add_osm_feature(osm_feature_category, osm_feature_subcategory)

  features <- osmdata_sf(datapull)


  if (nrow(features$osm_points) == 0) {
    print("No points of interest")
    return()
  } else {
    results <- (features$osm_points)
  }

  query_dt <- st_join(results, shp_dt)

  if (stata) {

        query_dt <- query_dt[, !grepl("geometry", names(query_dt))]
  }




  print("Open Street Maps Raster Downloaded")



  print("Process Complete!!!")

  return(query_dt)}

df <- geolink_get_poi(osm_feature_category = "building",
                  osm_feature_subcategory ="farm",
                  shp_dt = shp_dt)

