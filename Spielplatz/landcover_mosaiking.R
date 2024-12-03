geolink_landcover <- function(time_unit = "annual",
                              start_date,
                              end_date,
                              shp_dt) {


  # Convert dates
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

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

      if (!year %in% names(raster_year_map)) {
        raster_year_map[[year]] <- list()
      }
      raster_year_map[[year]] <- c(raster_year_map[[year]], raster_path)
    }, error = function(e) {
      warning(paste("Could not download raster for year", year, ":", e$message))
    })
  }

  # Integrated mosaic_rasters function
  mosaic_rasters <- function(raster_paths, output_crs = "EPSG:4326", mosaic_fun = "mean") {
    if (length(raster_paths) < 1) {
      stop("Provide at least one raster path for mosaicking.")
    }

    # Read all rasters and align them
    raster_list <- lapply(raster_paths, function(path) {
      r <- terra::rast(path)
      return(r)
    })

    # Ensure consistent CRS
    raster_list <- lapply(raster_list, function(r) {
      terra::project(r, output_crs)
    })

    # Align rasters to a common resolution and extent
    if (length(raster_list) == 1) {
      return(raster_list[[1]])
    }

    aligned_rasters <- terra::mosaic(raster_list[[1]], raster_list[[2]], fun = mosaic_fun)

    if (length(raster_list) > 2) {
      for (i in 3:length(raster_list)) {
        tryCatch({
          aligned_rasters <- terra::mosaic(aligned_rasters, raster_list[[i]], fun = mosaic_fun)
        }, error = function(e) {
          warning(paste("Could not mosaic raster:", raster_paths[[i]], "Attempting merge"))
          aligned_rasters <- terra::merge(aligned_rasters, raster_list[[i]])
        })
      }
    }

    return(aligned_rasters)
  }

  # Mosaic rasters for each year
  mosaicked_rasters <- lapply(names(raster_year_map), function(year) {
    cat("Mosaicking rasters for year:", year, "\n")
    mosaic_rasters(raster_year_map[[year]])
  })
  names(mosaicked_rasters) <- names(raster_year_map)

  # Transform shapefile
  projected_shapefile <- sf::st_transform(shp_dt, crs = sf::st_crs("EPSG:4326"))

  # Crop rasters
  cropped_rasters <- lapply(mosaicked_rasters, function(raster_obj) {
    terra::crop(raster_obj, terra::ext(projected_shapefile))
  })

  # Extract land cover proportions
  file_values <- it_obj$features[[1]]$assets$data$`file:values`
  class_values <- unlist(lapply(file_values, `[[`, "values"))
  class_names <- unlist(lapply(file_values, `[[`, "summary"))

  proportions_list <- lapply(cropped_rasters, function(raster) {
    extracted_values <- exactextractr::exact_extract(raster, projected_shapefile, coverage_area = TRUE)
    total_area <- sum(sapply(extracted_values, function(ev) sum(ev$coverage_area, na.rm = TRUE)))

    if (total_area == 0) return(NULL)

    class_proportions <- sapply(class_values, function(class_val) {
      class_area <- sum(sapply(extracted_values, function(ev) {
        sum(ev$coverage_area[ev$value == class_val], na.rm = TRUE)
      }))
      (class_area / total_area) * 100
    })

    class_proportions
  })

  # Remove NULL entries and create dataframe
  proportions_list <- proportions_list[!sapply(proportions_list, is.null)]
  proportions_df <- do.call(rbind, proportions_list)
  colnames(proportions_df) <- class_names

  return(proportions_df)
}

# Example usage
df <- geolink_landcover(start_date = "2020-01-02", end_date = "2020-09-10", shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
