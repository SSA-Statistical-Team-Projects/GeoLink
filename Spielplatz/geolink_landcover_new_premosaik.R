
geolink_landcover <- function(time_unit = "annual",
                              start_date,
                              end_date,
                              shp_dt) {


  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  it_obj <- s_obj %>%
    stac_search(collections = "io-lulc-annual-v02",
                bbox = sf::st_bbox(shp_dt),
                datetime = paste(start_date, end_date, sep = "/")) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())


  # Filter out features with bbox containing 180 or -180
  filter_features <- function(feature) {
    bbox <- feature$bbox

    # Check if bbox contains 180 or -180
    if (any(bbox %in% c(180, -180))) {
      return(FALSE)
    }
    return(TRUE)
  }

  # Apply feature filtering
  it_obj$features <- it_obj$features[sapply(it_obj$features, filter_features)]


  url_list <- lapply(1:length(it_obj$features),
                     function(x) {
                       url <- paste0("/vsicurl/", it_obj$features[[x]]$assets$data$href)
                       return(url)
                     })


  # Verify each URL is valid and accessible
  lapply(url_list, function(url) {
    tryCatch({
      rast(url)
    }, error = function(e) {
      cat("Error with URL:", url, "\n")
      cat("Error message:", e$message, "\n")
      return(NULL)
    })
  })

  # Alternative loading method
  raster_objs <- list()
  for(url in url_list) {
    tryCatch({
      raster_objs[[url]] <- terra::rast(url)
    }, error = function(e) {
      warning(paste("Could not load raster from", url, ":", e$message))
    })
  }


  # Name rasters with their corresponding date ranges
  raster_list <- lapply(seq_along(raster_objs), function(i) {
    # Extract start and end dates from the corresponding feature
    start_date <- it_obj$features[[i]]$properties$start_datetime
    end_date <- it_obj$features[[i]]$properties$end_datetime

    # Format the dates to only keep the date part (YYYY-MM-DD)
    start_date_formatted <- substr(start_date, 1, 10)  # Extracting "YYYY-MM-DD"
    end_date_formatted <- substr(end_date, 1, 10)      # Extracting "YYYY-MM-DD"

    # Create name using date range
    raster_name <- paste(start_date_formatted, "/", end_date_formatted, sep = "")

    setNames(raster_objs[[i]], raster_name)
  })

  # Ensure `shp_dt` is a SpatVector
  if (inherits(shp_dt, "sf")) {
    shp_dt <- vect(shp_dt)  # Convert sf to SpatVector
  } else if (is.character(shp_dt)) {
    shp_dt <- vect(shp_dt)  # Load from file path
  } else if (!inherits(shp_dt, "SpatVector")) {
    stop("`shp_dt` must be a SpatVector, sf object, or file path.")
  }

  # Transform shapefile CRS to raster CRS
  raster_crs <- crs(raster_objs[[1]])
  projected_shapefile <- project(shp_dt, raster_crs)


  # Create cropped rasters for each raster object
  cropped_rasters <- lapply(raster_objs, function(raster_obj) {
    # Crop the raster to the extent of the shapefile
    cropped_raster <- crop(raster_obj, ext(projected_shapefile))

    # Optional: mask the raster if you want to clip to shapefile boundary
    # cropped_raster <- mask(cropped_raster, projected_shapefile)

    return(cropped_raster)
  })


  # Extract file values
  file_values <- it_obj$features[[1]]$assets$data$`file:values`

  # Initialize vectors for class names and class values
  class_names <- c()
  class_values <- c()

  # Iterate over the list to populate class names and values
  for (item in file_values) {
    class_values <- c(class_values, item$values)
    class_names <- c(class_names, item$summary)
  }

  # Create a named vector for class values with class names as names
  class_values_named <- setNames(class_values, class_names)

  projected_shapefile <- sf::st_as_sf(projected_shapefile)

  # Initialize a list to store proportions for each class
  proportions_list <- list()

  # Apply the summarizing function to each filtered raster and combine results
  for (i in seq_along(cropped_rasters)) {
    print(paste("Processing raster:", i))

    # Extract values from raster that intersect with transformed shapefile
    extracted_values <- exact_extract(cropped_rasters[[i]], projected_shapefile, coverage_area = TRUE)

    # Debug: Check the extracted values structure
    if (length(extracted_values) == 0) {
      warning("No values extracted for raster ", i)
      proportions_list[[i]] <- rep(0, length(class_values))
      next
    }

    # Calculate total coverage area for normalization
    total_area <- sum(sapply(extracted_values, function(ev) sum(ev$coverage_area, na.rm = TRUE)))

    if (total_area == 0) {
      warning("Total area is zero for raster ", i)
      proportions_list[[i]] <- rep(0, length(class_values))
      next
    }

    # Summarize the extracted values into proportions for each class
    class_proportions <- sapply(class_values, function(class_val) {
      class_area <- sum(sapply(extracted_values, function(ev) {
        sum(ev$coverage_area[ev$value == class_val], na.rm = TRUE)
      }))
      class_proportion <- (class_area / total_area)*100
      return(class_proportion)
    })

    # Append the proportions for the current raster to the list
    proportions_list[[i]] <- class_proportions
  }

  # Combine the proportions for all rasters into a data frame
  proportions_df <- do.call(rbind, proportions_list)

  # Set column names to class names
  colnames(proportions_df) <- class_names

  # Return the proportions table along with other information
  return(proportions_df)
}
