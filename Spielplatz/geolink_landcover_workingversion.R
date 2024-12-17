geolink_landcover <- function(start_date = NULL,
                              end_date = NULL,
                              shp_dt = NULL,
                              survey_dt = NULL,
                              use_resampling = TRUE) {

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  source_python(file.path("inst", "python", "raster_utils.py"))

  # STAC search
  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  it_obj <- s_obj %>%
    stac_search(
      collections = "io-lulc-annual-v02",
      bbox = sf::st_bbox(shp_dt),
      datetime = paste(start_date, end_date, sep = "/")
    ) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

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
  projected_shapefile <- if (sf::st_crs(shp_dt)$input == "EPSG:4326") {
    shp_dt
  } else {
    tryCatch({
      sf::st_transform(shp_dt, crs = sf::st_crs("EPSG:4326"))
    }, error = function(e) {
      warning("Could not reproject shapefile. Assigning EPSG:4326 CRS.")
      sf::st_set_crs(shp_dt, "EPSG:4326")
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

    # Prepare proportions for each polygon
    polygon_proportions <- lapply(seq_along(extracted_values), function(j) {
      ev <- extracted_values[[j]]
      total_area <- sum(ev$coverage_area, na.rm = TRUE)

      if (total_area == 0) {
        # Return NA for all proportions if total area is zero
        proportions <- setNames(rep(NA, length(all_column_names)), all_column_names)
      } else {
        proportions <- setNames(sapply(class_values, function(class_val) {
          class_area <- sum(ev$coverage_area[ev$value == class_val], na.rm = TRUE)
          (class_area / total_area) * 100
        }), class_names)

        # Calculate remainder and assign to "no_data"
        remainder <- 100 - sum(proportions, na.rm = TRUE)
        proportions["no_data"] <- max(0, remainder)
      }

      return(proportions)
    })

    # Convert to matrix
    proportions_matrix <- do.call(rbind, polygon_proportions)

    # Create a data frame with shapefile data and proportions
    year_results <- sf::st_drop_geometry(projected_shapefile)

    # Add proportions as columns
    for (col in all_column_names) {
      year_results[[col]] <- proportions_matrix[, col]
    }

    # Add year column
    year_results$year <- year

    # Add results for this year to the list
    results_list[[year]] <- year_results
  }

  # Combine results from all years
  final_result <- do.call(rbind, results_list)

  # Re-attach geometry to the combined result
  final_result <- sf::st_sf(final_result, geometry = sf::st_geometry(projected_shapefile))

  return(final_result)
}
