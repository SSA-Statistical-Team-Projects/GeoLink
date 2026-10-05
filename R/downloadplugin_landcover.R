# Helper functions
# #' Ensure coordinates are in CRS 4326
# #'
# #' @param data Spatial data object
# #' @return Spatial data object with CRS 4326
# ensure_crs_4326 <- function(data) {
#   if (is.null(sf::st_crs(data)) || sf::st_crs(data)$epsg != 4326) {
#     return(sf::st_transform(data, 4326))
#   }
#   return(data)
# }

#' Create default results dataframe for a year
#'
#' @param sf_obj Spatial object
#' @param year Year for which to create default results
#' @return A dataframe with default values
create_default_results <- function(sf_obj, year) {
  original_data <- sf::st_drop_geometry(sf_obj)
  results <- original_data
  results$year <- year
  results$no_data <- 100
  results$water <- 0
  results$trees <- 0
  results$flooded_vegetation <- 0
  results$crops <- 0
  results$built_area <- 0
  results$bare_ground <- 0
  results$`snow/ice` <- 0
  results$clouds <- 0
  results$rangeland <- 0

  sf_result <- sf::st_sf(results, geometry = sf::st_geometry(sf_obj))
  return(sf_result)
}

#' Filter STAC features based on date and bounding box
#'
#' @param feature STAC feature
#' @param start_date Start date
#' @param end_date End date
#' @return Boolean indicating if feature should be included
filter_features <- function(feature, start_date, end_date) {
  bbox <- feature$bbox
  is_valid_bbox <- !any(bbox %in% c(180, -180))

  # Check feature date
  feature_date <- as.Date(feature$properties$start_datetime)
  is_correct_year <- feature_date >= start_date && feature_date <= end_date

  # For full calendar year, also check URL pattern
  if (format(start_date, "%m-%d") == "01-01" && format(end_date, "%m-%d") == "12-31") {
    year <- format(start_date, "%Y")
    next_year <- as.character(as.numeric(year) + 1)
    pattern <- paste0(year, "0101-", next_year, "0101")

    # Check URL pattern
    url <- feature$assets$data$href
    is_correct_year <- is_correct_year || grepl(pattern, url, fixed = TRUE)
  }

  # Return TRUE only if both conditions are met
  return(is_valid_bbox && is_correct_year)
}

#' Download a raster from URL
#'
#' @param url URL to download from
#' @param output_path Path to save the downloaded file
#' @return Path to downloaded file or NULL if download failed
download_raster <- function(url, output_path) {
  tryCatch({
    response <- httr::GET(
      url = url,
      httr::write_disk(output_path, overwrite = TRUE),
      httr::progress(),
      httr::timeout(300)
    )

    if (httr::http_error(response)) {
      httr_status <- httr::http_status(response)
      warning(sprintf("HTTP error: %s (Status code: %s)",
                      httr_status$message,
                      httr::status_code(response)))
      return(NULL)
    }

    if (!file.exists(output_path) || file.size(output_path) == 0) {
      warning("Download failed or produced an empty file")
      return(NULL)
    }

    cat("Downloaded raster to:", output_path, "\n")
    return(output_path)

  }, error = function(e) {
    warning(paste("Could not download raster:", e$message))
    return(NULL)
  })
}

#' Process a single raster (project to 4326)
#'
#' @param path Path to input raster
#' @param output_path Path to save processed raster
#' @return Path to processed raster or NULL if processing failed
process_raster <- function(path, output_path) {
  tryCatch({
    rast <- terra::rast(path)

    # Set or project CRS to 4326
    if (is.na(terra::crs(rast))) {
      terra::crs(rast) <- "EPSG:4326"
    } else if (terra::crs(rast, describe = TRUE)$code != "4326") {
      rast <- terra::project(rast, "EPSG:4326")
    }

    # Save processed raster
    terra::writeRaster(rast, output_path, overwrite = TRUE)
    return(output_path)
  }, error = function(e) {
    warning(paste("Error processing raster", path, ":", e$message))
    return(NULL)
  })
}

#' Extract and process raster values for polygons
#'
#' @param rast_obj Raster object
#' @param sf_obj Spatial object
#' @param class_mapping Class mapping dataframe
#' @param results Results dataframe to populate
#' @return Updated results dataframe
extract_and_process_values <- function(rast_obj, sf_obj, class_mapping, results) {
  # Extract raster values for each polygon
  extracted_values <- tryCatch({
    exactextractr::exact_extract(rast_obj, sf_obj, coverage_area = TRUE)
  }, error = function(e) {
    warning(paste("Error in exact_extract:", e$message))
    return(NULL)
  })

  # Only process extraction results if we have them
  if (!is.null(extracted_values)) {
    # Process each polygon's extracted values
    for (i in seq_along(extracted_values)) {
      ev <- extracted_values[[i]]

      # Skip empty or NULL extraction results (keep defaults)
      if (is.null(ev) || nrow(ev) == 0) {
        next
      }

      # Calculate total area
      total_area <- sum(ev$coverage_area, na.rm = TRUE)

      # Skip if no area (keep defaults)
      if (total_area <= 0) {
        next
      }

      # Reset the default values for this polygon (we'll replace them)
      for (j in 1:nrow(class_mapping)) {
        results[i, class_mapping$name[j]] <- 0
      }

      # Calculate proportions for each class
      for (j in 1:nrow(class_mapping)) {
        class_val <- class_mapping$value[j]
        class_name <- class_mapping$name[j]

        if (class_name == "no_data") next  # Skip no_data, calculate at end

        class_area <- sum(ev$coverage_area[ev$value == class_val], na.rm = TRUE)
        results[i, class_name] <- round((class_area / total_area) * 100, 2)
      }

      # Calculate no_data as the remainder
      class_sum <- sum(unlist(results[i, class_mapping$name[class_mapping$name != "no_data"]]), na.rm = TRUE)
      results[i, "no_data"] <- max(0, round(100 - class_sum, 2))
    }
  }

  # Quality check - replace any NA values with defaults
  for (i in 1:nrow(results)) {
    for (j in 1:nrow(class_mapping)) {
      class_name <- class_mapping$name[j]
      if (is.na(results[i, class_name])) {
        if (class_name == "no_data") {
          results[i, "no_data"] <- 100
        } else {
          results[i, class_name] <- 0
        }
      }
    }
  }

  return(results)
}

#' Create class mapping from STAC features
#'
#' @param it_obj STAC item object
#' @return Class mapping dataframe
create_class_mapping <- function(it_obj) {
  # Use the original class names
  class_mapping <- data.frame(
    value = c(0, 1, 2, 4, 5, 7, 8, 9, 10, 11),
    name = c("no_data", "water", "trees", "flooded_vegetation", "crops",
             "built_area", "bare_ground", "snow/ice", "clouds", "rangeland"),
    stringsAsFactors = FALSE
  )

  # Try to get class values from raster metadata if available
  if (length(it_obj$features) > 0 && !is.null(it_obj$features[[1]]$assets$data$`file:values`)) {
    file_values <- it_obj$features[[1]]$assets$data$`file:values`

    # Only process a limited number of classes to avoid byte limit
    max_classes <- min(20, length(file_values))

    for (i in 1:max_classes) {
      val <- file_values[[i]]$values
      name <- tolower(gsub("[^a-zA-Z0-9]", "_", file_values[[i]]$summary))

      # Truncate name to maximum 20 characters to be safe
      name <- substr(name, 1, 20)

      # Only add if not already in mapping
      if (!val %in% class_mapping$value) {
        class_mapping <- rbind(class_mapping,
                               data.frame(value = val, name = name, stringsAsFactors = FALSE))
      }
    }
  }

  return(class_mapping)
}

#' Process year data
#'
#' @param year Year to process
#' @param sf_obj Spatial object
#' @param session_temp_dir Temporary directory
#' @param resample_resolution Resolution for resampling
#' @return SF object with results for this year
process_year_data <- function(year, sf_obj, session_temp_dir, resample_resolution = NULL) {
  year_start <- as.Date(paste0(year, "-01-01"))
  year_end <- as.Date(paste0(year, "-12-31"))

  cat(sprintf("Processing year %d...\n", year))

  # STAC search for this year
  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  it_obj <- s_obj %>%
    stac_search(
      collections = "io-lulc-annual-v02",
      bbox = sf::st_bbox(sf_obj),
      datetime = paste(year_start, year_end, sep = "/")
    ) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  it_obj$features <- it_obj$features[sapply(it_obj$features, function(feature) {
    filter_features(feature, year_start, year_end)
  })]

  if (length(it_obj$features) == 0) {
    expanded_start <- as.Date(paste0(year, "-01-01"))
    expanded_end <- as.Date(paste0(year, "-12-31"))

    it_obj <- s_obj %>%
      stac_search(
        collections = "io-lulc-annual-v02",
        bbox = sf::st_bbox(sf_obj),
        datetime = paste(expanded_start, expanded_end, sep = "/")
      ) %>%
      get_request() %>%
      items_sign(sign_fn = sign_planetary_computer())

    it_obj$features <- it_obj$features[sapply(it_obj$features, function(feature) {
      filter_features(feature, expanded_start, expanded_end)
    })]

    if (length(it_obj$features) == 0) {
      cat(sprintf("No data found for year %d, using default values\n", year))
      return(create_default_results(sf_obj, year))
    }
  }

  # Download rasters
  raster_paths <- list()

  for (i in seq_along(it_obj$features)) {
    if (is.null(it_obj$features[[i]]$assets$data$href)) next

    url <- it_obj$features[[i]]$assets$data$href
    raster_path <- file.path(session_temp_dir, paste0("raster_", year, "_", i, ".tif"))

    downloaded_path <- download_raster(url, raster_path)
    if (!is.null(downloaded_path)) {
      raster_paths[[length(raster_paths) + 1]] <- downloaded_path
    }
  }

  if (length(raster_paths) == 0) {
    cat(sprintf("No rasters could be downloaded for year %d, using default values\n", year))
    return(create_default_results(sf_obj, year))
  }

  # Create a year-specific folder
  year_folder <- file.path(session_temp_dir, paste0("year_", year))
  dir.create(year_folder, showWarnings = FALSE, recursive = TRUE)

  # Process rasters
  processed_rasters <- character()

  if (!is.null(resample_resolution)) {
    resampled_folder <- file.path(year_folder, "resampled")
    dir.create(resampled_folder, showWarnings = FALSE, recursive = TRUE)

    # Check if the resample_rasters function exists in the python environment
    if (exists("resample_rasters", envir = .GlobalEnv) ||
        exists("resample_rasters", envir = .GlobalEnv, inherits = TRUE) ||
        exists("resample_rasters", mode = "function")) {
      processed_rasters <- tryCatch({
        resample_rasters(
          input_files = unlist(raster_paths),
          output_folder = resampled_folder,
          target_resolution = resample_resolution
        )
      }, error = function(e) {
        warning(paste("Error in resampling:", e$message))
        return(NULL)
      })
    } else {
      warning("resample_rasters function not found. Using original resolution instead.")
      # Fall back to processing rasters without resampling
      processed_rasters <- character()
      projected_folder <- file.path(year_folder, "projected")
      dir.create(projected_folder, showWarnings = FALSE, recursive = TRUE)

      for (path in raster_paths) {
        proj_path <- file.path(projected_folder, paste0("proj_", basename(path)))
        processed_path <- process_raster(path, proj_path)
        if (!is.null(processed_path)) {
          processed_rasters <- c(processed_rasters, processed_path)
        }
      }
    }
  } else {
    # Use original resolution
    projected_folder <- file.path(year_folder, "projected")
    dir.create(projected_folder, showWarnings = FALSE, recursive = TRUE)

    for (path in raster_paths) {
      proj_path <- file.path(projected_folder, paste0("proj_", basename(path)))
      processed_path <- process_raster(path, proj_path)
      if (!is.null(processed_path)) {
        processed_rasters <- c(processed_rasters, processed_path)
      }
    }
  }

  if (length(processed_rasters) == 0) {
    cat(sprintf("No processed rasters for year %d, using default values\n", year))
    return(create_default_results(sf_obj, year))
  }

  # Create a mosaic from processed rasters
  mosaic_output_path <- file.path(year_folder, "final_mosaic.tif")

  mosaicked_path <- tryCatch({
    if (length(processed_rasters) == 1) {
      # Use the single raster directly
      file.copy(processed_rasters[1], mosaic_output_path, overwrite = TRUE)
      mosaic_output_path
    } else {
      # Create mosaic
      mosaic_rasters(
        input_files = processed_rasters,
        output_path = mosaic_output_path
      )
    }
  }, error = function(e) {
    warning(paste("Error during mosaic operation:", e$message))
    # Fallback to first raster
    if (length(processed_rasters) > 0) {
      file.copy(processed_rasters[1], mosaic_output_path, overwrite = TRUE)
      return(mosaic_output_path)
    }
    return(NULL)
  })

  if (is.null(mosaicked_path) || !file.exists(mosaicked_path)) {
    cat(sprintf("Mosaic failed for year %d, using default values\n", year))
    return(create_default_results(sf_obj, year))
  }

  rast_obj <- tryCatch({
    terra::rast(mosaicked_path)
  }, error = function(e) {
    warning(paste("Error loading mosaic raster:", e$message))
    if (length(processed_rasters) > 0) {
      return(terra::rast(processed_rasters[1]))
    }
    return(NULL)
  })

  if (is.null(rast_obj)) {
    cat(sprintf("Failed to load raster for year %d, using default values\n", year))
    return(create_default_results(sf_obj, year))
  }

  # Create class mapping
  class_mapping <- create_class_mapping(it_obj)

  # Create default results dataframe with original columns
  results <- sf::st_drop_geometry(sf_obj)

  # Set year
  results$year <- year

  # Set default values for all polygons
  for (i in 1:nrow(results)) {
    results[i, "no_data"] <- 100  # Default to 100% no_data

    # Set all other classes to 0%
    for (j in 1:nrow(class_mapping)) {
      if (class_mapping$name[j] != "no_data") {
        results[i, class_mapping$name[j]] <- 0
      }
    }
  }

  # Extract values & override defaults where possible
  results <- extract_and_process_values(rast_obj, sf_obj, class_mapping, results)

  # Define the column order: original columns, class columns, and year
  original_cols <- names(sf::st_drop_geometry(sf_obj))
  original_cols <- original_cols[original_cols != "geometry"]

  # Make sure all class columns exist
  class_cols <- class_mapping$name
  for (col in class_cols) {
    if (!col %in% names(results)) {
      results[[col]] <- 0
      if (col == "no_data") results[[col]] <- 100
    }
  }

  # Define the column order: original columns, class columns, and year
  columns_to_keep <- c(original_cols, class_cols, "year")

  # Remove id column if it was added by us and not in original data
  if (!"id" %in% original_cols) {
    columns_to_keep <- columns_to_keep[columns_to_keep != "id"]
  }

  # Select columns in the desired order (removing duplicates)
  columns_to_keep <- unique(columns_to_keep)

  # Keep only columns that exist in the results
  columns_to_keep <- columns_to_keep[columns_to_keep %in% names(results)]

  # Select columns in the desired order
  results <- results[, columns_to_keep]

  # Reattach geometry
  sf_result <- sf::st_sf(results, geometry = sf::st_geometry(sf_obj))

  return(sf_result)
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

#' Combine io-lulc land cover tiles of one year into a single EPSG:4326 raster in R
#'
#' The R counterpart of the Python `resample_rasters()` + `mosaic_rasters()` used when
#' `python = TRUE`. Every tile is reprojected with nearest neighbour (the values are classes)
#' onto one common EPSG:4326 grid and the tiles are then merged. io-lulc tiles are in
#' different UTM zones, so they cannot be mosaicked in their own CRS.
#'
#' @param tiles character vector of raster files, or a list of `SpatRaster`s
#' @param target_resolution resolution in meters, converted to degrees as in the Python code
#'   (meters / 111320). NULL keeps each tile's native resolution (that of the first tile).
#' @param bbox optional `sf` bbox (or numeric xmin, ymin, xmax, ymax) in EPSG:4326 to which
#'   the output is limited
#' @return a `SpatRaster` in EPSG:4326 with the class values; class 0 ("No Data") and cells
#'   outside every tile are NA. NULL if no tile overlaps `bbox`.
#' @noRd
.landcover_combine_tiles <- function(tiles, target_resolution = NULL, bbox = NULL) {

  tiles <- lapply(tiles, function(x) if (inherits(x, "SpatRaster")) x else terra::rast(x))
  if (length(tiles) == 0) stop("No land cover tile to combine")

  res_in_deg <- function(r) {
    if (terra::is.lonlat(r)) terra::res(r)[1] else terra::res(r)[1] / 111320
  }
  res_deg <- if (is.null(target_resolution)) res_in_deg(tiles[[1]]) else target_resolution / 111320

  ## footprint of each tile in EPSG:4326
  tile_ext <- lapply(tiles, function(r) {
    if (is.na(terra::crs(r)) || terra::crs(r) == "") terra::crs(r) <- "EPSG:4326"
    terra::project(terra::ext(r), from = terra::crs(r), to = "EPSG:4326")
  })
  full_ext <- Reduce(terra::union, tile_ext)
  if (!is.null(bbox)) {
    bbox <- as.numeric(bbox[c("xmin", "xmax", "ymin", "ymax")])
    full_ext <- terra::intersect(full_ext, terra::ext(bbox))
    if (is.null(full_ext)) return(NULL)
  }

  ## one grid for all tiles, anchored at (0, 0) so that per-tile crops stay aligned
  snap <- function(v, f) f(v / res_deg) * res_deg
  grid_ext <- terra::ext(snap(terra::xmin(full_ext), floor), snap(terra::xmax(full_ext), ceiling),
                         snap(terra::ymin(full_ext), floor), snap(terra::ymax(full_ext), ceiling))
  template <- terra::rast(grid_ext, resolution = res_deg, crs = "EPSG:4326")

  projected <- list()
  for (k in seq_along(tiles)) {
    e <- terra::intersect(tile_ext[[k]], terra::ext(template))
    if (is.null(e)) next
    tmpl_k <- terra::crop(template, e, snap = "out")
    p <- terra::project(tiles[[k]], tmpl_k, method = "near")
    p <- terra::subst(p, 0, NA)   # class 0 is "No Data": let the other tiles fill it
    projected[[length(projected) + 1]] <- p
  }
  if (length(projected) == 0) return(NULL)

  out <- if (length(projected) == 1) projected[[1]] else terra::merge(terra::sprc(projected))
  names(out) <- "landcover"
  out
}

#' Land cover class shares (percent) of each polygon
#'
#' @param rast `SpatRaster` of land cover classes (from `.landcover_combine_tiles`)
#' @param sf_obj `sf` polygons
#' @param class_values,class_names the class codes and the output column names; the class
#'   coded 0 is "no data"
#' @param weights optional `SpatRaster` of weights aligned with `rast`
#' @return a data.frame with one column per class name: the percentage of the polygon area
#'   (weighted if `weights`) in each class, rounded to 2 decimals. `no_data` is the share
#'   without data (NA pixels or class 0). A polygon without any pixel with data (outside the
#'   tiles) gets NA for every class and 100 for `no_data`.
#' @noRd
.landcover_class_shares <- function(rast, sf_obj, class_values, class_names, weights = NULL) {

  out <- as.data.frame(matrix(NA_real_, nrow = nrow(sf_obj), ncol = length(class_names),
                              dimnames = list(NULL, class_names)), check.names = FALSE)
  out[["no_data"]] <- 100

  ev_list <- if (is.null(weights)) {
    exactextractr::exact_extract(rast, sf_obj, coverage_area = TRUE, progress = FALSE)
  } else {
    exactextractr::exact_extract(rast, sf_obj, coverage_area = TRUE, weights = weights,
                                 progress = FALSE)
  }

  for (i in seq_along(ev_list)) {
    ev <- ev_list[[i]]
    if (is.null(ev) || nrow(ev) == 0) next

    w <- ev$coverage_area
    if (!is.null(weights)) w <- w * ev$weight
    total <- sum(w, na.rm = TRUE)
    has_data <- !is.na(ev$value) & ev$value != 0
    if (total <= 0 || sum(w[has_data], na.rm = TRUE) <= 0) next

    for (k in seq_along(class_values)) {
      if (class_names[k] == "no_data") next
      out[i, class_names[k]] <- round(sum(w[ev$value %in% class_values[k]], na.rm = TRUE) /
                                        total * 100, 2)
    }
    out[i, "no_data"] <- round(sum(w[!has_data], na.rm = TRUE) / total * 100, 2)
  }

  out
}
