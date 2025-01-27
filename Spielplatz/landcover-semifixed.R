geolink_landcover <- function(start_date = NULL,
                              end_date = NULL,
                              shp_dt = NULL,
                              survey_dt = NULL,
                              shp_fn = NULL,
                              grid_size = NULL,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              survey_crs = 4326,
                              use_resampling = FALSE) {

  if (is.null(start_date) || is.null(end_date)) {
    stop("start_date and end_date must be provided")
  }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Ensure shapefile and survey are in the correct CRS
  if (!is.null(shp_dt)) {
    sf_obj <- zonalstats_prepshp(shp_dt = shp_dt,
                                 grid_size = grid_size) %>%
      ensure_crs_4326()

  } else if (!is.null(survey_dt)) {
    sf_obj <-  zonalstats_prepsurvey(survey_dt = survey_dt,
                                     buffer_size = buffer_size) %>%
      ensure_crs_4326()

  } else if (!is.null(shp_fn)) {
    sf_obj <-  zonalstats_prepshp(shp_dt = NULL,
                                  shp_fn = shp_fn,
                                  grid_size = grid_size) %>%
      ensure_crs_4326()

  } else if (!is.null(survey_fn)) { # Changed condition to `survey_fn`
    sf_obj <- zonalstats_prepsurvey(
      survey_fn = survey_fn,
      survey_lat = survey_lat,
      survey_lon = survey_lon,
      buffer_size = buffer_size,
      survey_crs = survey_crs) %>%
      ensure_crs_4326()

  } else {
    print("Input a valid sf object or geosurvey")
    sf_obj <- NULL  # Optional: Define a default value to avoid potential errors
  }

  # Clear existing Python config
  Sys.unsetenv("RETICULATE_PYTHON")

  # Set up virtual environment
  venv_path <- file.path(system.file(package = "GeoLink"), "python", "virtual_env")
  reticulate::use_virtualenv(venv_path, required = TRUE)

  # Source Python utilities from correct path
  python_utils_path <- system.file("python_scripts", "raster_utils.py", package = "GeoLink")
  if (!file.exists(python_utils_path)) {
    stop("Python utilities not found. Check package installation.")
  }
  reticulate::source_python(python_utils_path)

  filter_by_year <- function(features, start_date, end_date) {
    start_year <- as.numeric(format(as.Date(start_date), "%Y"))
    end_year <- as.numeric(format(as.Date(end_date), "%Y"))

    filtered <- features[sapply(features, function(feature) {
      year <- as.numeric(format(as.Date(feature$properties$start_datetime), "%Y"))
      year >= start_year && year <= end_year
    })]

    return(filtered)
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

  it_obj$features <- filter_by_year(it_obj$features, start_date, end_date)

  if (length(it_obj$features) == 0) {
    stop("No data found for the specified years.")
  }

  # Filter out features with problematic bounding boxes
  filter_features <- function(feature) {
    bbox <- feature$bbox
    !any(bbox %in% c(180, -180))
  }
  it_obj$features <- it_obj$features[sapply(it_obj$features, filter_features)]

  # Check if any features are found
  if (length(it_obj$features) == 0) {
    stop("No suitable raster features found for the given date range and shapefile.")
  }

  # Temporary directory for downloads
  temp_dir <- tempdir()

  # Download and organize rasters
  raster_year_map <- list()

  for (i in seq_along(it_obj$features)) {
    if (is.null(it_obj$features[[i]]$assets$data$href)) {
      next
    }

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

  # Python-based mosaicking and resampling
  mosaicked_rasters <- list()

  for (year in names(raster_year_map)) {
    cat("Processing rasters for year:", year, "\n")

    raster_paths <- raster_year_map[[year]]
    raster_paths <- as.character(raster_paths)
    raster_paths <- normalizePath(raster_paths)

    nonexistent_files <- raster_paths[!file.exists(raster_paths)]
    if (length(nonexistent_files) > 0) {
      stop("The following files do not exist: ", paste(nonexistent_files, collapse = ", "))
    }

    cat("Rasters for year:", year, "are located at:", raster_paths, "\n")

    if (use_resampling) {
      resampled_rasters <- resample_rasters(
        input_files = raster_paths,
        output_folder = file.path(temp_dir, "resampled", year),
        target_resolution = 1000
      )

      if (length(resampled_rasters) == 1) {
        mosaicked_path <- resampled_rasters[[1]]
      } else {
        mosaicked_path <- mosaic_rasters(input_files = resampled_rasters)
      }
    } else {
      if (length(raster_paths) == 1) {
        mosaicked_path <- raster_paths[[1]]
      } else {
        mosaicked_path <- mosaic_rasters(input_files = raster_paths)
      }
    }

    mosaicked_raster <- terra::rast(mosaicked_path)

    if (is.na(terra::crs(mosaicked_raster))) {
      terra::crs(mosaicked_raster) <- "EPSG:4326"
    } else {
      if (terra::crs(mosaicked_raster, describe = TRUE)$code != "4326") {
        mosaicked_raster <- terra::project(
          mosaicked_raster,
          "EPSG:4326"
        )
      }
    }

    mosaicked_rasters[[year]] <- mosaicked_raster
  }

  # Transform shapefile
  projected_shapefile <- if (sf::st_crs(sf_obj)$input == "EPSG:4326") {
    sf_obj
  } else {
    tryCatch({
      sf::st_transform(sf_obj, crs = sf::st_crs("EPSG:4326"))
    }, error = function(e) {
      warning("Could not reproject shapefile. Assigning EPSG:4326 CRS.")
      sf::st_set_crs(sf_obj, "EPSG:4326")
    })
  }

  cropped_rasters <- lapply(mosaicked_rasters, function(raster_obj) {
    terra::crop(raster_obj, terra::ext(projected_shapefile))
  })
  names(cropped_rasters) <- names(mosaicked_rasters)

  # Extract land cover proportions
  file_values <- it_obj$features[[1]]$assets$data$`file:values`
  class_values <- unlist(lapply(file_values, `[[`, "values"))
  class_names <- tolower(gsub(" ", "_", unlist(lapply(file_values, `[[`, "summary"))))

  # Add an additional column name for "no_data"
  all_column_names <- c(class_names, "no_data")

  # Create a list to store results for each year
  results_list <- list()

  for (i in seq_along(cropped_rasters)) {
    year <- names(cropped_rasters)[i]
    raster <- cropped_rasters[[i]]

    # Extract values for each polygon
    extracted_values <- exactextractr::exact_extract(raster, projected_shapefile, coverage_area = TRUE)

    # In the polygon_proportions calculation section:
    polygon_proportions <- lapply(seq_along(extracted_values), function(j) {
      ev <- extracted_values[[j]]

      # Debug print
      print(paste("Values in extracted data for polygon", j))
      print(table(ev$value))

      total_area <- sum(ev$coverage_area, na.rm = TRUE)

      if (total_area == 0) {
        print(paste("Zero total area for polygon", j))
        proportions <- setNames(rep(NA, length(all_column_names)), all_column_names)
      } else {
        proportions <- setNames(sapply(class_values, function(class_val) {
          class_area <- sum(ev$coverage_area[ev$value == class_val], na.rm = TRUE)
          prop <- (class_area / total_area) * 100
          if(prop > 0) print(paste("Non-zero proportion for class", class_val, ":", prop))
          prop
        }), class_names)

        # Calculate remainder and assign to "no_data"
        remainder <- 100 - sum(proportions, na.rm = TRUE)
        proportions["no_data"] <- max(0, remainder)

        # Debug print
        print("Calculated proportions:")
        print(proportions)
      }

      return(proportions)
    })

    # Convert to matrix
    proportions_matrix <- do.call(rbind, polygon_proportions)

    #Clean colnames
    colnames(proportions_matrix)  <- paste0(colnames(proportions_matrix), "_", year)
    colnames(proportions_matrix) <- gsub("/", "_", colnames(proportions_matrix))

    #create data frame
    results_list[[year]] <- as.data.frame(proportions_matrix)
  }

  # Combine results from all years
  final_result <- do.call(cbind, results_list)
  colnames(final_result) <- gsub("^\\d+\\.", "", colnames(final_result))


  # Re-attach geometry to the combined result
  final_result <- cbind(projected_shapefile,final_result)

  return(final_result)
}


test_dt1 <- geolink_landcover(start_date = "2020-01-01",
                              end_date = "2020-12-30",
                              survey_fn = "tests/testthat/testdata/xy_hhgeo_dt.dta",
                              survey_lon = "x",
                              survey_lat = "y",
                              buffer_size = NULL)


#space
#survey no data
#clear tempdir
