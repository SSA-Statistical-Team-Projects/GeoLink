geolink_landcover <- function(start_date,
                              end_date,
                              shp_dt = NULL,
                              shp_fn = NULL,
                              survey_dt = NULL,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              survey_crs = 4326,
                              grid_size = NULL,
                              use_resampling = TRUE,
                              target_resolution = 1000) {

  # Convert dates
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # SECTION 1: PROCESS INPUT DATA ---------------------------------------------

  # Handle survey data if provided
  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
  } else if (!is.null(survey_fn)) {
    if (is.null(survey_lat) || is.null(survey_lon)) {
      stop("Both survey_lat and survey_lon must be provided when using survey_fn")
    }

    # Read survey file
    survey_dt <- try({
      if (grepl("\\.dta$", survey_fn)) {
        haven::read_dta(survey_fn)
      } else if (grepl("\\.csv$", survey_fn)) {
        utils::read.csv(survey_fn)
      } else {
        stop("Unsupported file format. Please provide .dta or .csv file")
      }
    }, silent = TRUE)

    if (inherits(survey_dt, "try-error")) {
      stop("Error reading survey file")
    }

    # Convert to sf object
    survey_dt <- st_as_sf(survey_dt,
                          coords = c(survey_lon, survey_lat),
                          crs = survey_crs)

    # Transform to EPSG:4326 if needed
    if (st_crs(survey_dt)$epsg != 4326) {
      survey_dt <- st_transform(survey_dt, 4326)
    }
  }

  # Create spatial object from inputs
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

  # Fix invalid geometries
  sf_obj <- sf::st_make_valid(sf_obj)

  # SECTION 2: CONFIGURE PYTHON ENVIRONMENT ----------------------------------

  # Clear existing Python config
  Sys.unsetenv("RETICULATE_PYTHON")

  # Get current Python configuration from package environment
  pkg_env <- get("pkg_env", envir = asNamespace("GeoLink"))
  geo_env_name <- pkg_env$conda_env_name
  os_type <- pkg_env$os_type

  # Function to run a system command with proper error handling
  run_system_cmd <- function(cmd, args, error_msg = "Command failed") {
    result <- try({
      output <- system2(cmd, args, stdout = TRUE, stderr = TRUE)
      list(success = TRUE, output = output)
    }, silent = TRUE)

    if (inherits(result, "try-error") || !result$success) {
      warning(paste(error_msg, ":", conditionMessage(result)))
      return(FALSE)
    }
    return(TRUE)
  }

  # Function to verify and repair Python environment
  verify_and_repair_env <- function() {
    # Get current paths from package environment
    current_python_path <- pkg_env$python_path

    # Check if path exists
    if (is.null(current_python_path) || !file.exists(current_python_path)) {
      message("Python path not found or invalid. Attempting to locate...")

      # Try to find the conda environment
      envs <- try(reticulate::conda_list(), silent = TRUE)
      if (!inherits(envs, "try-error") && geo_env_name %in% envs$name) {
        # Found the environment, get its Python
        new_py_path <- try(reticulate::conda_python(geo_env_name), silent = TRUE)
        if (!inherits(new_py_path, "try-error") && !is.null(new_py_path) && file.exists(new_py_path)) {
          # Update the path in the package environment
          pkg_env$python_path <- new_py_path
          assign("pkg_env", pkg_env, envir = asNamespace("GeoLink"))
          current_python_path <- new_py_path
          message("Located Python at: ", current_python_path)
        } else {
          # If conda_python fails, try direct conda command
          conda_bin <- try(reticulate::conda_binary(), silent = TRUE)
          if (!inherits(conda_bin, "try-error") && file.exists(conda_bin)) {
            cmd_result <- system2(conda_bin, c("info", "--json"), stdout = TRUE)
            if (length(cmd_result) > 0) {
              conda_info <- try(jsonlite::fromJSON(paste(cmd_result, collapse = "")), silent = TRUE)
              if (!inherits(conda_info, "try-error") && !is.null(conda_info$envs)) {
                # Search for our environment path
                for (env_path in conda_info$envs) {
                  if (grepl(paste0(geo_env_name, "$"), env_path)) {
                    # Find Python within this environment
                    if (os_type == "Windows") {
                      possible_py <- file.path(env_path, "python.exe")
                    } else {
                      possible_py <- file.path(env_path, "bin", "python")
                    }
                    if (file.exists(possible_py)) {
                      pkg_env$python_path <- possible_py
                      assign("pkg_env", pkg_env, envir = asNamespace("GeoLink"))
                      current_python_path <- possible_py
                      message("Located Python at: ", current_python_path)
                      break
                    }
                  }
                }
              }
            }
          }
        }
      }

      # If still not found, create a new environment
      if (is.null(current_python_path) || !file.exists(current_python_path)) {
        message("Python environment not found. Creating new environment...")
        conda_bin <- try(reticulate::conda_binary(), silent = TRUE)
        if (!inherits(conda_bin, "try-error") && file.exists(conda_bin)) {
          # Create a new environment
          run_system_cmd(conda_bin,
                         c("create", "-y", "-n", geo_env_name, "python=3.10"),
                         "Failed to create conda environment")

          # Get the new Python path
          new_py_path <- try(reticulate::conda_python(geo_env_name), silent = TRUE)
          if (!inherits(new_py_path, "try-error") && !is.null(new_py_path) && file.exists(new_py_path)) {
            pkg_env$python_path <- new_py_path
            assign("pkg_env", pkg_env, envir = asNamespace("GeoLink"))
            current_python_path <- new_py_path
            message("Created new Python environment at: ", current_python_path)

            # Install required packages in the new environment
            if (os_type == "Linux") {
              pip_path <- file.path(dirname(new_py_path), "pip")
              if (file.exists(pip_path)) {
                run_system_cmd(pip_path, c("install", "numpy", "gdal", "rasterio"),
                               "Failed to install packages")
              } else {
                run_system_cmd(conda_bin,
                               c("install", "-y", "-n", geo_env_name,
                                 "-c", "conda-forge", "numpy", "gdal", "rasterio", "libtiff6"),
                               "Failed to install packages")
              }
            } else {
              run_system_cmd(conda_bin,
                             c("install", "-y", "-n", geo_env_name,
                               "-c", "conda-forge", "numpy", "gdal", "rasterio", "libtiff>=4.6.1"),
                             "Failed to install packages")
            }
          }
        }
      }
    }

    # At this point, we should have a valid Python path or have failed completely
    if (is.null(current_python_path) || !file.exists(current_python_path)) {
      stop("Failed to locate or create a valid Python environment")
    }

    # Activate the Python environment
    message("Activating Python at: ", current_python_path)
    reticulate::use_python(current_python_path, required = TRUE)

    # Verify GDAL and rasterio are available
    verify_result <- try({
      reticulate::py_run_string("
import sys
print('Python executable:', sys.executable)

# Test imports
try:
    import numpy
    print('NumPy available')
except ImportError:
    raise ImportError('NumPy not available')

try:
    from osgeo import gdal
    print('GDAL available')
except ImportError:
    try:
        import gdal
        print('GDAL available (direct import)')
    except ImportError:
        raise ImportError('GDAL not available')

try:
    import rasterio
    print('Rasterio available')
except ImportError:
    raise ImportError('Rasterio not available')
      ")
      TRUE
    }, silent = TRUE)

    # If verification fails, try to repair
    if (inherits(verify_result, "try-error")) {
      message("Python environment verification failed. Attempting repairs...")

      # Get pip path
      pip_path <- file.path(dirname(current_python_path), "pip")
      if (os_type == "Windows") {
        pip_path <- file.path(dirname(current_python_path), "pip.exe")
      }

      # Try to repair with pip if available
      if (file.exists(pip_path)) {
        message("Using pip to repair environment...")
        run_system_cmd(pip_path, c("install", "--upgrade", "pip"), "Pip upgrade failed")
        run_system_cmd(pip_path, c("install", "--upgrade", "numpy"), "NumPy installation failed")
        run_system_cmd(pip_path, c("install", "--upgrade", "gdal"), "GDAL installation failed")
        run_system_cmd(pip_path, c("install", "--upgrade", "rasterio"), "Rasterio installation failed")
      } else {
        # Fallback to conda
        conda_bin <- try(reticulate::conda_binary(), silent = TRUE)
        if (!inherits(conda_bin, "try-error") && file.exists(conda_bin)) {
          message("Using conda to repair environment...")
          if (os_type == "Linux") {
            run_system_cmd(conda_bin,
                           c("install", "-y", "-n", geo_env_name, "-c", "conda-forge",
                             "numpy", "gdal", "rasterio", "libtiff6"),
                           "Conda package installation failed")
          } else {
            run_system_cmd(conda_bin,
                           c("install", "-y", "-n", geo_env_name, "-c", "conda-forge",
                             "numpy", "gdal", "rasterio", "libtiff>=4.6.1"),
                           "Conda package installation failed")
          }
        }
      }

      # Verify again after repair
      verify_again <- try({
        reticulate::py_run_string("
import sys
try:
    import numpy
    from osgeo import gdal
    import rasterio
    print('All required packages available')
except ImportError as e:
    raise ImportError(f'Required package still missing after repair: {e}')
        ")
        TRUE
      }, silent = TRUE)

      if (inherits(verify_again, "try-error")) {
        warning("Failed to repair Python environment. Proceeding with caution...")
        return(FALSE)
      }
    }

    return(TRUE)
  }

  # Verify and set up Python environment
  env_ready <- verify_and_repair_env()

  if (!env_ready) {
    warning("Python environment may not be fully functional. Some operations might fail.")
  }

  # Configure SSL certificates
  try({
    reticulate::py_run_string("
    import certifi
    import os
    os.environ['SSL_CERT_FILE'] = certifi.where()
    ")
  }, silent = TRUE)

  # Load Python utilities
  python_utils_path <- system.file("python_scripts", "raster_utils.py", package = "GeoLink")
  if (!file.exists(python_utils_path)) {
    stop("Python utilities not found. Check package installation.")
  }
  reticulate::source_python(python_utils_path)

  # SECTION 3: STAC SEARCH FOR LANDCOVER DATA --------------------------------

  # Define feature filter function
  filter_features <- function(feature, start_date, end_date) {
    # Get feature date and check if it's in the time range
    feature_date <- as.Date(feature$properties$start_datetime)
    feature_year <- format(feature_date, "%Y")
    start_year <- format(as.Date(start_date), "%Y")
    end_year <- format(as.Date(end_date), "%Y")

    return(feature_year >= start_year && feature_year <= end_year)
  }

  # Perform STAC search
  stac_result <- try({
    # Connect to Planetary Computer STAC API
    s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

    # Get bounding box with buffer
    bbox <- sf::st_bbox(sf_obj)
    buffered_bbox <- c(
      bbox["xmin"] - 0.1,
      bbox["ymin"] - 0.1,
      bbox["xmax"] + 0.1,
      bbox["ymax"] + 0.1
    )

    # Search for land cover data
    it_obj <- s_obj %>%
      stac_search(
        collections = "io-lulc-annual-v02",
        bbox = buffered_bbox,
        datetime = paste(start_date, end_date, sep = "/")
      ) %>%
      get_request() %>%
      items_sign(sign_fn = sign_planetary_computer())

    # Filter features by date
    it_obj$features <- it_obj$features[sapply(it_obj$features, function(feature) {
      filter_features(feature, start_date, end_date)
    })]

    it_obj
  }, silent = TRUE)

  # Handle failed STAC search
  if (inherits(stac_result, "try-error") || length(stac_result$features) == 0) {
    warning("No features found. Returning empty dataset.")
    return(create_empty_result(sf_obj, start_date))
  }

  # SECTION 4: DOWNLOAD AND PROCESS RASTERS ---------------------------------

  temp_dir <- tempdir()
  raster_year_map <- list()

  # Define land cover classes
  land_cover_classes <- list(
    list(values = 0, summary = "No Data"),
    list(values = 1, summary = "Water"),
    list(values = 2, summary = "Trees"),
    list(values = 4, summary = "Flooded vegetation"),
    list(values = 5, summary = "Crops"),
    list(values = 7, summary = "Built area"),
    list(values = 8, summary = "Bare ground"),
    list(values = 9, summary = "Snow/ice"),
    list(values = 10, summary = "Clouds"),
    list(values = 11, summary = "Rangeland")
  )


  # Download rasters
  for (i in seq_along(stac_result$features)) {
    feature <- stac_result$features[[i]]

    if (is.null(feature$assets$data$href)) {
      print(paste("Feature", i, "has no data URL"))
      next
    }

    year <- format(as.Date(feature$properties$start_datetime), "%Y")
    url <- feature$assets$data$href
    raster_path <- file.path(temp_dir, paste0(year, "_", i, "_raster.tif"))

    print(paste("Downloading raster for year", year))

    # Try to download the file
    download_success <- try({
      response <- httr::GET(
        url,
        httr::write_disk(raster_path, overwrite = TRUE),
        httr::config(ssl_verifypeer = FALSE),
        httr::timeout(300)
      )

      httr::status_code(response) == 200
    }, silent = TRUE)

    if (!inherits(download_success, "try-error") && download_success) {
      file_size <- file.info(raster_path)$size

      if (file_size > 0) {
        if (!year %in% names(raster_year_map)) {
          raster_year_map[[year]] <- list()
        }
        raster_year_map[[year]] <- append(raster_year_map[[year]], raster_path)
      }
    }
  }

  # Check if any rasters were downloaded
  if (length(raster_year_map) == 0) {
    warning("No raster data could be downloaded. Returning empty dataset.")
    return(create_empty_result(sf_obj, start_date))
  }

  # SECTION 5: PROCESS RASTERS AND EXTRACT DATA -----------------------------

  # Process each year's rasters
  results_list <- list()

  for (year in names(raster_year_map)) {
    message("Processing rasters for year:", year)

    raster_paths <- as.character(raster_year_map[[year]])

    # Check file existence
    if (!all(file.exists(raster_paths))) {
      warning(paste("Some raster files for year", year, "do not exist. Skipping."))
      next
    }

    # Apply resampling if requested
    if (use_resampling) {
      processed_paths <- try({
        resample_rasters(
          input_files = raster_paths,
          output_folder = file.path(temp_dir, "resampled", year),
          target_resolution = target_resolution
        )
      }, silent = TRUE)

      if (!inherits(processed_paths, "try-error") && length(processed_paths) > 0) {
        raster_paths <- processed_paths
      }
    }

    # Mosaic rasters if needed
    if (length(raster_paths) > 1) {
      mosaic_path <- try({
        mosaic_rasters(input_files = raster_paths)
      }, silent = TRUE)

      if (!inherits(mosaic_path, "try-error")) {
        raster_path <- mosaic_path
      } else {
        raster_path <- raster_paths[1]
      }
    } else {
      raster_path <- raster_paths[1]
    }

    # Load the raster
    raster <- try({
      terra::rast(raster_path)
    }, silent = TRUE)

    if (inherits(raster, "try-error")) {
      warning(paste("Failed to load raster for year", year))
      next
    }

    # Ensure CRS is set correctly
    if (is.na(terra::crs(raster))) {
      terra::crs(raster) <- "EPSG:4326"
    }

    # Extract class values and names
    class_values <- unlist(lapply(land_cover_classes, function(x) x$values))
    class_names <- tolower(gsub(" ", "_", unlist(lapply(land_cover_classes, function(x) x$summary))))
    all_column_names <- c(class_names, "no_data")

    # Extract land cover proportions
    message(paste("Extracting land cover proportions for year:", year))

    # Create results dataframe
    year_results <- sf::st_drop_geometry(sf_obj)

    # Initialize all land cover columns to 0
    for (col in all_column_names) {
      year_results[[col]] <- 0
    }

    # Extract values using exactextractr
    extracted_values <- try({
      exactextractr::exact_extract(raster, sf::st_make_valid(sf_obj),
                                   coverage_area = TRUE)
    }, silent = TRUE)

    if (!inherits(extracted_values, "try-error")) {
      # Process extracted values
      for (i in seq_along(extracted_values)) {
        ev <- extracted_values[[i]]

        if (is.null(ev) || nrow(ev) == 0) {
          next
        }

        # Calculate total area
        total_area <- sum(ev$coverage_area, na.rm = TRUE)

        if (total_area <= 0) {
          next
        }

        # Calculate proportion for each class
        for (class_idx in seq_along(class_values)) {
          class_val <- class_values[class_idx]
          class_name <- class_names[class_idx]

          class_rows <- ev$value == class_val

          if (any(class_rows, na.rm = TRUE)) {
            class_area <- sum(ev$coverage_area[class_rows], na.rm = TRUE)
            year_results[i, class_name] <- round((class_area / total_area) * 100, 2)
          }
        }

        # Calculate no_data percentage
        na_rows <- is.na(ev$value)
        if (any(na_rows)) {
          na_area <- sum(ev$coverage_area[na_rows], na.rm = TRUE)
          year_results[i, "no_data"] <- round((na_area / total_area) * 100, 2)
        }
      }
    }

    year_results$year <- year
    results_list[[year]] <- year_results
  }

  # SECTION 6: COMBINE RESULTS AND RETURN ------------------------------------

  # Combine all years of results
  if (length(results_list) == 0) {
    warning("No results generated. Returning empty dataset.")
    return(create_empty_result(sf_obj, start_date))
  }

  # Combine results and add geometries
  result_df <- do.call(rbind, results_list)
  final_result <- sf::st_sf(result_df, geometry = sf::st_geometry(sf_obj)[rep(1:nrow(sf_obj), length(results_list))])

  return(final_result)
}

# Helper function to create empty result
create_empty_result <- function(sf_obj, start_date) {
  empty_result <- sf::st_drop_geometry(sf_obj)

  land_cover_classes <- c("No Data", "Water", "Trees", "Flooded vegetation", "Crops",
                          "Built area", "Bare ground", "Snow/ice", "Clouds", "Rangeland")


  for (col in land_cover_classes) {
    empty_result[[col]] <- NA
  }

  empty_result$year <- format(start_date, "%Y")
  return(sf::st_sf(empty_result, geometry = sf::st_geometry(sf_obj)))
}
