pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf, dplyr)

geolink_get_poi <- function(osm_key,
                            osm_value = NULL,
                            shp_dt = NULL,
                            survey_dt = NULL,
                            shp_fn = NULL,
                            survey_fn = NULL,
                            buffer = NULL,
                            stata = FALSE) {

  # Automatically convert survey_dt to sf if provided and shp_dt is NULL
  if (!is.null(survey_dt)) {
    if (!inherits(survey_dt, "sf")) {
      if (inherits(survey_dt, c("data.table", "data.frame"))) {
        if ("geometry" %in% names(survey_dt)) {
          survey_dt <- st_as_sf(survey_dt)
        } else if ("lon" %in% names(survey_dt) && "lat" %in% names(survey_dt)) {
          survey_dt <- st_as_sf(survey_dt, coords = c("lon", "lat"), crs = 4326)
        } else {
          stop("survey_dt must have a 'geometry' column or both 'lon' and 'lat' columns to be converted to an sf object.")
        }
      } else {
        stop("survey_dt must be an sf object, data.table, or data.frame.")
      }
    }
    survey_dt <- ensure_crs_4326(survey_dt)
    shp_dt <- survey_dt
  }

  if (!is.null(shp_fn)) {
    shp_dt <- st_read(shp_fn)
  }

  if (is.null(shp_dt)) {
    stop("Either shp_dt or survey_dt must be provided.")
  }

  if (nrow(shp_dt) == 0) {
    stop("shp_dt is empty after filtering. Please check the filter conditions.")
  }
  if (any(is.na(st_geometry(shp_dt)))) {
    stop("shp_dt contains invalid geometries. Please ensure all geometries are valid.")
  }

  # Updated warning suppression function
  oldw <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = oldw))

  if (!is.null(survey_dt)) {
    points <- st_geometry(survey_dt)
    results_list <- list()

    for (point in points) {
      point_bbox <- st_bbox(point)
      datapull <- opq(c(bbox = point_bbox, timeout = 7200)) %>%
        add_osm_feature(key = osm_key, value = osm_value)
      features <- osmdata_sf(datapull)
      results_list[[length(results_list) + 1]] <- features$osm_points
    }

    results <- do.call(rbind, results_list)
  } else {
    bbox <- st_bbox(shp_dt)
    if (any(is.na(bbox))) {
      stop("Bounding box contains NA values. Please ensure shp_dt has valid geometries.")
    }

    datapull <- opq(c(bbox = bbox, timeout = 7200)) %>%
      add_osm_feature(key = osm_key, value = osm_value)

    features <- osmdata_sf(datapull)
    results <- features$osm_points
  }

  results <- results %>%
    filter(if_any(-c(osm_id, geometry), ~ !is.na(.x)))

  query_dt <- st_join(results, shp_dt, left = FALSE)

  if (nrow(query_dt) == 0) {
    print("No points of interest")
  }

  if (stata) {
    query_dt <- query_dt[, !grepl("geometry", names(query_dt))]
  }

  print("OpenStreetMap data downloaded.")
  print("Process complete!")

  return(query_dt)
}


poi_survey_df <- geolink_get_poi(osm_key = "amenity",
                                 shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])





df <- geolink_get_poi(osm_key = "amenity",
                  survey_dt = hhgeo_dt[hhgeo_dt$ADM1_EN == "Lagos",],
                  buffer = 100000)


