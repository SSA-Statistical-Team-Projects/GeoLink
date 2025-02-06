geolink_electaccess <- function(
    start_date = NULL,
    end_date = NULL,
    shp_dt = NULL,
    shp_fn = NULL,
    grid_size = 1000,
    survey_dt = NULL,
    survey_fn = NULL,
    survey_lat = NULL,
    survey_lon = NULL,
    buffer_size = NULL,
    extract_fun = "mean",
    survey_crs = 4326
) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Process input data first to get sf_obj
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

  # Create sf_obj based on input type
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

  # Now use sf_obj for STAC search
  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1", force_version = "1.0.0")
  it_obj <- s_obj %>%
    stac_search(
      collections = "hrea",
      bbox = sf::st_bbox(sf_obj),
      datetime = paste(start_date, end_date, sep = "/")
    ) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  if (length(it_obj$features) == 0) {
    stop("No data found for the specified date range and location")
  }

  # Modify url_list extraction with error checking
  url_list <- lapply(1:length(it_obj$features), function(x) {
    feature <- it_obj$features[[x]]
    required_assets <- c("lightscore", "light-composite", "night-proportion", "estimated-brightness")

    # Check if all required assets exist
    missing_assets <- required_assets[!required_assets %in% names(feature$assets)]
    if (length(missing_assets) > 0) {
      warning(sprintf("Missing assets for feature %d: %s", x, paste(missing_assets, collapse = ", ")))
      return(NULL)
    }

    urls <- list(
      lightscore = paste0("/vsicurl/", feature$assets$lightscore$href),
      light_composite = paste0("/vsicurl/", feature$assets$`light-composite`$href),
      night_proportion = paste0("/vsicurl/", feature$assets$`night-proportion`$href),
      estimated_brightness = paste0("/vsicurl/", feature$assets$`estimated-brightness`$href)
    )
    return(urls)
  })

  # Remove NULL entries from url_list
  url_list <- Filter(Negate(is.null), url_list)

  if (length(url_list) == 0) {
    stop("No valid data URLs found")
  }

  # Convert URLs to rasters with progress tracking
  raster_objs <- list()
  for (i in seq_along(url_list)) {
    for (asset_name in names(url_list[[i]])) {
      url <- url_list[[i]][[asset_name]]
      tryCatch({
        rast_obj <- terra::rast(url)
        if (!is.null(rast_obj)) {
          raster_objs[[length(raster_objs) + 1]] <- rast_obj
        }
      }, error = function(e) {
        warning(sprintf("Failed to load raster for %s: %s", asset_name, e$message))
      })
    }
  }

  if (length(raster_objs) == 0) {
    stop("No rasters could be successfully loaded")
  }

  # Create name_set based on actual successful raster downloads
  name_set <- character(length(raster_objs))

  # Generate names based on actual raster count
  indicators <- c("lightscore", "light_composite", "night_proportion", "estimated_brightness")
  year_sequence <- seq(lubridate::year(start_date), lubridate::year(end_date))

  for (i in seq_along(raster_objs)) {
    indicator_idx <- ((i - 1) %% length(indicators)) + 1
    year_idx <- ((i - 1) %/% length(indicators)) + 1
    if (year_idx <= length(year_sequence)) {
      name_set[i] <- paste0(indicators[indicator_idx], "_", year_sequence[year_idx])
    }
  }

  print("Electrification Access Raster Downloaded")
  print(sprintf("Processing %d rasters with %d names", length(raster_objs), length(name_set)))

  dt <- postdownload_processor(
    shp_dt = sf_obj,
    raster_objs = raster_objs,
    shp_fn = shp_fn,
    grid_size = grid_size,
    survey_dt = survey_dt,
    survey_fn = survey_fn,
    survey_lat = survey_lat,
    survey_lon = survey_lon,
    extract_fun = extract_fun,
    buffer_size = buffer_size,
    survey_crs = survey_crs,
    name_set = name_set
  )

  print("Process Complete!!!")
  return(dt)
}
