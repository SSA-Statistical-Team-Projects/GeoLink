
# Helper function to get OSM data with retries
get_osm_data <- function(bbox, osm_key, osm_value, max_retries = 3, timeout = 300) {
  for (i in 1:max_retries) {
    tryCatch({
      datapull <- opq(bbox = bbox, timeout = timeout) %>%
        add_osm_feature(key = osm_key, value = osm_value)
      return(osmdata_sf(datapull))
    }, error = function(e) {
      if (i == max_retries) stop(e)
      message(sprintf("Attempt %d failed. Retrying in 5 seconds...", i))
      Sys.sleep(5)  # Wait 5 seconds before retrying
      NULL
    })
  }
}

# Helper function to process bbox quadrants
process_bbox_quadrant <- function(quad_bbox, osm_key, osm_value) {
  tryCatch({
    features <- get_osm_data(quad_bbox, osm_key, osm_value)
    if (!is.null(features$osm_points)) {
      return(features$osm_points %>%
               filter(if_any(-c(osm_id, geometry), ~ !is.na(.x))))
    }
    NULL
  }, error = function(e) {
    message(sprintf("Error processing quadrant: %s", e$message))
    NULL
  })
}

# Main function
geolink_get_poi <- function(osm_key,
                            osm_value = NULL,
                            shp_dt = NULL,
                            shp_fn = NULL,
                            survey_dt = NULL,
                            survey_fn = NULL,
                            survey_lat = NULL,
                            survey_lon = NULL,
                            buffer_size = NULL,
                            survey_crs = NULL,
                            grid_size = NULL,
                            max_retries = 3,
                            timeout = 300,
                            area_threshold = 1) {

  # Validate OSM key-value pairs
  if (!osm_key %in% available_features()) {
    stop(sprintf("'%s' is not a valid OSM key", osm_key))
  }
  if (!is.null(osm_value)) {
    available_tags <- available_tags(osm_key)
    if (!osm_value %in% available_tags) {
      warning(sprintf("'%s' may not be a valid value for key '%s'", osm_value, osm_key))
    }
  }

  # Set CRS to 4326 if data is provided as survey_dt
  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
  }


  # Handle survey file input and projection
  if (!is.null(survey_fn)) {
    if (is.null(survey_lat) || is.null(survey_lon)) {
      stop("Both survey_lat and survey_lon must be provided when using survey_fn")
    }

    # Read the survey file
    tryCatch({
      if (grepl("\\.dta$", survey_fn)) {
        survey_dt <- haven::read_dta(survey_fn)
      } else if (grepl("\\.csv$", survey_fn)) {
        survey_dt <- read.csv(survey_fn)
      } else {
        stop("Unsupported file format. Please provide .dta or .csv file")
      }
    }, error = function(e) {
      stop(sprintf("Error reading survey file: %s", e$message))
    })

    # Convert to sf object with specified projection
    survey_dt <- st_as_sf(survey_dt,
                          coords = c(survey_lon, survey_lat),
                          crs = survey_crs)

    # Transform to EPSG:4326 if needed
    if (st_crs(survey_dt)$epsg != 4326) {
      survey_dt <- st_transform(survey_dt, 4326)
    }
  }

  # Process input data using helper functions
  if (!is.null(shp_dt)) {
    sf_obj <- zonalstats_prepshp(shp_dt = shp_dt, grid_size = grid_size) %>%
      ensure_crs_4326()
  } else if (!is.null(shp_fn)) {
    sf_obj <- zonalstats_prepshp(shp_fn = shp_fn, grid_size = grid_size) %>%
      ensure_crs_4326()
  } else if (!is.null(survey_dt) || !is.null(survey_fn)) {
    sf_obj <- zonalstats_prepsurvey(
      survey_dt = survey_dt,
      survey_fn = survey_fn,
      survey_lat = survey_lat,
      survey_lon = survey_lon,
      buffer_size = buffer_size,
      survey_crs = survey_crs) %>%
      ensure_crs_4326()
  } else {
    stop("Please provide either a shapefile (shp_dt/shp_fn) or survey data (survey_dt/survey_fn)")
  }

  # Validate input data
  if (nrow(sf_obj) == 0) {
    stop("Input data is empty after filtering")
  }
  if (any(is.na(st_geometry(sf_obj)))) {
    stop("Input contains invalid geometries")
  }

  # Suppress warnings
  oldw <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = oldw))

  # Get bounding box
  bbox <- st_bbox(sf_obj)
  bbox_area <- (bbox["xmax"] - bbox["xmin"]) * (bbox["ymax"] - bbox["ymin"])

  # Process based on area size
  if (bbox_area > area_threshold) {
    message("Large area detected. Splitting into quadrants...")

    # Split into quadrants
    mid_x <- (bbox["xmax"] + bbox["xmin"]) / 2
    mid_y <- (bbox["ymax"] + bbox["ymin"]) / 2

    quadrants <- list(
      q1 = c(xmin = bbox["xmin"], ymin = mid_y, xmax = mid_x, ymax = bbox["ymax"]),
      q2 = c(xmin = mid_x, ymin = mid_y, xmax = bbox["xmax"], ymax = bbox["ymax"]),
      q3 = c(xmin = bbox["xmin"], ymin = bbox["ymin"], xmax = mid_x, ymax = mid_y),
      q4 = c(xmin = mid_x, ymin = bbox["ymin"], xmax = bbox["xmax"], ymax = mid_y)
    )

    # Process each quadrant
    results_list <- lapply(quadrants, function(quad_bbox) {
      process_bbox_quadrant(quad_bbox, osm_key, osm_value)
    })

    # Remove NULL results
    results_list <- results_list[!sapply(results_list, is.null)]

    if (length(results_list) == 0) {
      message("No results found in any quadrant")
      return(NULL)
    }

    # Get union of all column names
    all_cols <- unique(unlist(lapply(results_list, names)))

    # Ensure all data frames have the same columns
    results_list <- lapply(results_list, function(df) {
      missing_cols <- setdiff(all_cols, names(df))
      if (length(missing_cols) > 0) {
        for (col in missing_cols) {
          df[[col]] <- NA
        }
      }
      return(df[, all_cols])
    })

    # Combine results
    results <- do.call(rbind, results_list)

  } else {
    message("Processing area as single unit...")
    features <- get_osm_data(bbox, osm_key, osm_value, max_retries, timeout)
    results <- features$osm_points %>%
      filter(if_any(-c(osm_id, geometry), ~ !is.na(.x)))
  }

  # Check if results exist
  if (is.null(results)) {
    return(NULL)
  }

  # Join results with input geometries
  query_dt <- st_join(results, sf_obj, left = FALSE)

  # Check results
  if (nrow(query_dt) == 0) {
    message("No points of interest found in the specified area")
  } else {
    message(sprintf("Found %d points of interest", nrow(query_dt)))
  }

  message("OpenStreetMap data download complete!")

  return(query_dt)
}
