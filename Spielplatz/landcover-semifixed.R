geolink_landcover <- function(start_date = NULL,
                              end_date = NULL,
                              shp_dt = NULL,
                              shp_fn = NULL,
                              survey_dt = NULL,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              survey_crs = 4326,
                              grid_size = NULL,
                              use_resampling = FALSE) {

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Process input data using helper functions
  if (!is.null(shp_dt)) {
    sf_obj <- zonalstats_prepshp(shp_dt = shp_dt,
                                 grid_size = grid_size) %>%
      ensure_crs_4326()
  } else if (!is.null(shp_fn)) {
    sf_obj <- zonalstats_prepshp(shp_fn = shp_fn,
                                 grid_size = grid_size) %>%
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


  # Clear existing Python config
  Sys.unsetenv("RETICULATE_PYTHON")

  # Set up virtual environment
  venv_path <- file.path(system.file(package = "GeoLink"), "python", "virtual_env")
  reticulate::use_virtualenv(venv_path, required = TRUE)

  # Source Python utilities
  python_utils_path <- system.file("python_scripts", "raster_utils.py", package = "GeoLink")
  if (!file.exists(python_utils_path)) {
    stop("Python utilities not found. Check package installation.")
  }
  reticulate::source_python(python_utils_path)

  # Keep one filter_features function that checks both conditions
  filter_features <- function(feature, start_date, end_date) {
    # Check if it's a boundary tile
    bbox <- feature$bbox
    is_valid_bbox <- !any(bbox %in% c(180, -180))

    # Check if dates represent a full calendar year
    start_year <- format(as.Date(start_date), "%Y")
    end_year <- format(as.Date(end_date), "%Y")
    start_md <- format(as.Date(start_date), "%m-%d")
    end_md <- format(as.Date(end_date), "%m-%d")

    is_full_year <- start_year == end_year &&
      start_md == "01-01" &&
      end_md == "12-31"

    if (is_full_year) {
      # For full calendar year, create the pattern dynamically
      year <- start_year
      next_year <- as.character(as.numeric(year) + 1)
      pattern <- paste0(year, "0101-", next_year, "0101")

      # Check URL pattern
      url <- feature$assets$data$href
      is_correct_year <- grepl(pattern, url, fixed = TRUE)
    } else {
      # For other date ranges, check the feature date
      feature_date <- as.Date(feature$properties$start_datetime)
      is_correct_year <- feature_date >= start_date && feature_date <= end_date
    }

    # Return TRUE only if both conditions are met
    return(is_valid_bbox && is_correct_year)
  }

  # STAC search
  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  it_obj <- s_obj %>%
    stac_search(
      collections = "io-lulc-annual-v02",
      bbox = sf::st_bbox(sf_obj),
      datetime = paste(start_date, end_date, sep = "/")
    ) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  # Apply filtering once with both conditions
  it_obj$features <- it_obj$features[sapply(it_obj$features, function(feature) {
    filter_features(feature, start_date, end_date)
  })]

  # Download and process rasters
  temp_dir <- tempdir()
  raster_year_map <- list()

  for (i in seq_along(it_obj$features)) {
    if (is.null(it_obj$features[[i]]$assets$data$href)) next

    year <- format(as.Date(it_obj$features[[i]]$properties$start_datetime), "%Y")
    url <- it_obj$features[[i]]$assets$data$href
    raster_path <- file.path(temp_dir, paste0(year, "_", i, "_raster.tif"))

    tryCatch({
      download.file(url, raster_path, mode = "wb")
      cat("Downloaded raster to:", raster_path, "\n")

      if (!year %in% names(raster_year_map)) {
        raster_year_map[[year]] <- list()
      }
      raster_year_map[[year]] <- append(raster_year_map[[year]], raster_path)
    }, error = function(e) {
      warning(paste("Could not download raster for year", year, ":", e$message))
    })
  }

  # Process rasters
  mosaicked_rasters <- list()

  for (year in names(raster_year_map)) {
    cat("Processing rasters for year:", year, "\n")

    raster_paths <- raster_year_map[[year]]
    raster_paths <- as.character(raster_paths)
    raster_paths <- normalizePath(raster_paths)

    if (!all(file.exists(raster_paths))) {
      stop("Some raster files do not exist")
    }


    if (use_resampling) {
      resampled_rasters <- resample_rasters(
        input_files = raster_paths,
        output_folder = file.path(temp_dir, "resampled", year),
        target_resolution = 1000
      )

      mosaicked_path <- if (length(resampled_rasters) == 1) {
        resampled_rasters[[1]]
      } else {
        mosaic_rasters(input_files = resampled_rasters)
      }
    } else {
      # Skip resampling and directly mosaic if multiple rasters exist
      mosaicked_path <- if (length(raster_paths) == 1) {
        raster_paths[1]
      } else {
        mosaic_rasters(input_files = raster_paths)
      }
    }

    mosaicked_raster <- terra::rast(mosaicked_path)

    if (is.na(terra::crs(mosaicked_raster))) {
      terra::crs(mosaicked_raster) <- "EPSG:4326"
    } else if (terra::crs(mosaicked_raster, describe = TRUE)$code != "4326") {
      mosaicked_raster <- terra::project(mosaicked_raster, "EPSG:4326")
    }

    mosaicked_rasters[[year]] <- mosaicked_raster
  }

  # Crop rasters to extent
  cropped_rasters <- lapply(mosaicked_rasters, function(raster_obj) {
    terra::crop(raster_obj, terra::ext(sf_obj))
  })
  names(cropped_rasters) <- names(mosaicked_rasters)

  # Extract land cover proportions
  file_values <- it_obj$features[[1]]$assets$data$`file:values`
  class_values <- unlist(lapply(file_values, `[[`, "values"))
  class_names <- tolower(gsub(" ", "_", unlist(lapply(file_values, `[[`, "summary"))))
  all_column_names <- c(class_names, "no_data")

  results_list <- list()

  for (i in seq_along(cropped_rasters)) {
    year <- names(cropped_rasters)[i]
    raster <- cropped_rasters[[i]]

    # Extract values using exactextractr
    extracted_values <- exactextractr::exact_extract(raster, sf_obj, coverage_area = TRUE)

    # Calculate proportions
    polygon_proportions <- lapply(seq_along(extracted_values), function(j) {
      ev <- extracted_values[[j]]
      total_area <- sum(ev$coverage_area, na.rm = TRUE)

      if (total_area == 0) {
        proportions <- setNames(rep(NA, length(all_column_names)), all_column_names)
      } else {
        proportions <- setNames(sapply(class_values, function(class_val) {
          class_area <- sum(ev$coverage_area[ev$value == class_val], na.rm = TRUE)
          round((class_area / total_area) * 100, 2)
        }), class_names)

        remainder <- 100 - sum(proportions, na.rm = TRUE)
        proportions["no_data"] <- round(max(0, remainder), 2)
      }

      return(proportions)
    })

    proportions_matrix <- do.call(rbind, polygon_proportions)

    # Create results dataframe
    year_results <- sf::st_drop_geometry(sf_obj)

    for (col in all_column_names) {
      year_results[[col]] <- proportions_matrix[, col]
    }

    year_results$year <- year
    results_list[[year]] <- year_results
  }

  # Combine results and reattach geometry
  final_result <- do.call(rbind, results_list)
  final_result <- sf::st_sf(final_result, geometry = sf::st_geometry(sf_obj))

  return(final_result)
}
