library(terra)
library(sf)
library(exactextractr)

# Function to filter and reproject tiles
filter_tiles <- function(raster_objs, dt, numCores = NULL) {

  filter_worker <- function(x, dt) {

    # Ensure dt and x have the same CRS by transforming dt if necessary
    if (terra::crs(x) != terra::crs(dt)) {
      dt <- st_transform(dt, crs = terra::crs(x))
    }

    y <- terra::crop(x, terra::ext(dt))
    y <- terra::mask(y, dt)

    return(y)
  }

  if (!is.null(numCores)) {

    # Parallelization processing when numCores is not NULL
    numCores <- min(numCores, parallel::detectCores())
    parallelMap::parallelLibrary("foreach")
    parallelMap::parallelLibrary("raster")
    parallelMap::parallelLibrary("terra")
    parallelMap::parallelLibrary("exactextractr")

    doParallel::registerDoParallel(cores = numCores)

    raster_objs <- foreach(i = 1:numCores) %dopar% {
      filter_worker(x = raster_objs[[i]], dt = dt)
    }

  } else {
    # Use this when we do not want to parallelize
    raster_objs <- lapply(raster_objs, filter_worker, dt = dt)
  }

  return(raster_objs)
}

# Main function to process land cover
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

  url_list <- lapply(1:length(it_obj$features),
                     function(x) {
                       url <- paste0("/vsicurl/", it_obj$features[[x]]$assets$data$href)
                       return(url)
                     })

  # Load rasters without projecting initially
  raster_objs <- lapply(url_list, terra::rast)

  # Name rasters
  raster_list <- lapply(seq_along(raster_objs), function(i) {
    setNames(raster_objs[[i]], as.character(i))
  })

  # Ensure shapefile CRS matches raster CRS for consistency
  raster_crs <- terra::crs(raster_objs[[1]])
  shp_dt_transformed <- st_transform(shp_dt, crs = raster_crs)

  # Filter raster objects using the filter_tiles function
  raster_objs <- filter_tiles(raster_list, dt = shp_dt_transformed)


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

  # Initialize a list to store proportions for each class
  proportions_list <- list()

  # Apply the summarizing function to each filtered raster and combine results
  for (i in seq_along(raster_objs)) {
    print(paste("Processing raster:", i))

    # Extract values from raster that intersect with transformed shapefile
    extracted_values <- exact_extract(raster_objs[[i]], shp_dt_transformed, coverage_area = TRUE)

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

# Example usage (assuming shp_dt is correctly loaded as an sf object with a column named ADM1_EN)
df <- geolink_landcover(start_date = "2020-01-01",
                        end_date = "2020-03-01",
                        shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
