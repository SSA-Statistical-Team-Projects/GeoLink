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

  raster_objs <- lapply(url_list, terra::rast)

  raster_list <- lapply(raster_objs, raster)

  raster_list <- lapply(seq_along(raster_objs), function(i) {
    setNames(raster_objs[[i]], as.character(i))
  })

  # Define class names based on the given unique values and corresponding categories
  class_names <- c("No Data", "Water", "Trees", "Flooded vegetation", "Crops", "Built area", "Bare ground", "Snow/ice", "Clouds", "Rangeland")

  # Define class names and values based on the given unique values and corresponding categories
  class_values <- c(No_Data = 0, Water = 1, Trees = 2, Flooded_Vegetation = 4,
                    Crops = 5, Built_Area = 7, Bare_Ground = 8, Snow_Ice = 9, Clouds = 10, Rangeland = 11)

  # Initialize a list to store proportions for each class
  proportions_list <- list()

  # Apply the summarizing function to each raster and combine results
  for (i in seq_along(raster_list)) {
    print(paste("Processing raster for year:", i))

    # Extract values from raster that intersect with shapefile
    extracted_values <- exact_extract(raster_list[[i]], shp_dt, coverage_area = TRUE)

    # Summarize the extracted values into proportions for each class
    class_proportions <- lapply(class_values, function(class_val) {
      class_proportion <- sum(extracted_values$value == class_val, na.rm = TRUE) / length(extracted_values$value)
      return(class_proportion)
    })

    # Append the proportions for the current raster to the list
    proportions_list[[i]] <- class_proportions
  }

  # Combine the proportions for all rasters into a data frame
  proportions_df <- do.call(rbind, proportions_list)

  # Set column names to class names
  colnames(proportions_df) <- names(class_values)

  # Return the proportions table along with other information
  return(proportions_df)
}






df <- geolink_landcover(time_unit,
                      start_date = "2020-01-01",
                      end_date = "2020-03-01",
                      shp_dt = shp_dt)




