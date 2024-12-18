pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf, dplyr)

geolink_get_poi <- function(osm_key,
                            osm_value,
                            shp_dt,
                            survey_dt = NULL,
                            shp_fn = NULL,
                            survey_fn = NULL,
                            buffer = NULL,
                            stata = FALSE){


  if (exists("shp_dt")) {
    shp_dt <- ensure_crs_4326(shp_dt)

  } else if (exists("survey_dt")) {
    survey_dt <- ensure_crs_4326(survey_dt)
  }


  if (!is.null(shp_fn)) {
    shp_dt <- st_read(shp_fn)
  }

  bbox <- create_query_bbox(shp_dt = shp_dt,
                            area_name = NULL,
                            buffer_dist = c(0, 0, 0, 0),
                            metric_crs = FALSE,
                            osm_crs = 4326)

  datapull <- opq(c(bbox = bbox, timeout = 7200)) %>%
    add_osm_feature(key = osm_key, value = osm_value)

  features <- osmdata_sf(datapull)

  results <- (features$osm_points)

  results <- results %>%
    filter(if_any(-c(osm_id, geometry), ~ !is.na(.x)))



  query_dt <- st_join(results, shp_dt, left = FALSE)


  if (nrow(query_dt) == 0) {
    print("No points of interest")}




  if (stata) {

    query_dt <- query_dt[, !grepl("geometry", names(query_dt))]
  }




  print("Open Street Maps Raster Downloaded")



  print("Process Complete!!!")

  return(query_dt)}



df <- geolink_get_poi(osm_key = "amenity",
                  survey_dt = hhgeo_dt[hhgeo_dt$ADM1_EN == "Lagos",],
                  buffer = 100000)


