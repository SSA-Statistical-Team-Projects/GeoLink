pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf, geodata, httr, ncdf4, rgdal, exactextractr, parallel, progress)


# Set date range
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2019-12-31")

# Define the specific scenario you want
scenario <- "ssp245"  # Change this to your desired scenario

desired_models <- c("UKESM1-0-LL")

# Filter shapefile
shp_dt <- shp_dt[shp_dt$ADM1_EN == "Abia",]

# Create STAC connection
s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

# Retrieve URLs for the specified scenario
it_obj <- s_obj %>%
  stac_search(
    collections = "nasa-nex-gddp-cmip6",
    bbox = sf::st_bbox(shp_dt),
    datetime = paste(start_date, end_date, sep = "/")
  ) %>%
  get_request() %>%
  items_sign(sign_fn = sign_planetary_computer())


filtered_features <- Filter(function(feature) {
  feature$properties$`cmip6:scenario` == scenario &&
    feature$properties$`cmip6:model` %in% desired_models
}, it_obj$features)


# Extract URLs for each feature with the specified scenario
urls <- lapply(seq_along(filtered_features), function(x) {
  list(
    scenario = scenario,
    pr = (filtered_features[[x]]$assets$pr$href),
    tas = (filtered_features[[x]]$assets$tas$href),
    hurs = (filtered_features[[x]]$assets$hurs$href),
    huss = (filtered_features[[x]]$assets$huss$href),
    rlds = (filtered_features[[x]]$assets$rlds$href),
    rsds = (filtered_features[[x]]$assets$rsds$href),
    tasmax = (filtered_features[[x]]$assets$tasmax$href),
    tasmin = (filtered_features[[x]]$assets$tasmin$href),
    sfcWind = (filtered_features[[x]]$assets$sfcWind$href)
  )
})


# Define a function to download files, load as rasters, and organize them
process_rasters <- function(urls, filtered_features) {
  # Create a temporary directory
  temp_dir <- tempdir()

  # Variables to process
  variables <- c("pr", "tas", "hurs", "huss", "rlds", "rsds", "tasmax", "tasmin", "sfcWind")

  # Initialize raster storage
  raster_list <- list()

  # Progress bar setup
  pb <- progress_bar$new(
    total = length(urls) * length(variables),
    format = "  Downloading [:bar] :percent (:current/:total)"
  )

  # Loop through each feature set
  for (i in seq_along(urls)) {
    # Extract model, year, and scenario from the feature
    model <- filtered_features[[i]]$properties$`cmip6:model`
    year <- filtered_features[[i]]$properties$`cmip6:year`
    scenario <- urls[[i]]$scenario

    # Create a list to store rasters for this feature
    current_rasters <- list()

    # Loop through each variable and download/load raster
    for (var in variables) {
      pb$tick()  # Increment progress bar

      # Get URL
      url <- urls[[i]][[var]]
      if (is.null(url)) next  # Skip if URL is missing

      # Define a temporary file name
      temp_file <- file.path(temp_dir, paste0(model, "_", var, "_", year, ".nc"))

      # Download the file
      tryCatch({
        GET(url, write_disk(temp_file, overwrite = TRUE))

        # Load the file as a raster
        raster <- rast(temp_file)

        # Assign CRS to EPSG:4326
        crs(raster) <- "EPSG:4326"

        # Add to current raster list
        current_rasters[[var]] <- raster
      }, error = function(e) {
        # Handle download or loading errors
        warning(paste("Error processing", var, "for model", model, "year", year, ":", e$message))
      })
    }

    # Add current rasters to raster_list
    if (length(current_rasters) > 0) {
      label <- paste0(model, "_", year, "_", scenario)
      raster_list[[label]] <- current_rasters
    }
  }

  # Return the raster list
  return(raster_list)
}

raster_list <- process_rasters(urls, filtered_features)

raster_objs <- unlist(raster_list, recursive = FALSE)





# Create name_set with all 4 indicators for each year
year_sequence <- seq(lubridate::year(start_date), lubridate::year(end_date))

# Generate name_set with all indicators
name_set <- unlist(lapply(year_sequence, function(year) {
  c(
    paste0("pr_", year),
    paste0("tas_", year),
    paste0("hurs_", year),
    paste0("huss_", year),
    paste0("rlds_", year),
    paste0("rsds_", year),
    paste0("tasmax_", year),
    paste0("tasmin_", year),
    paste0("sfcWind_", year)


  )
}))



dt <- postdownload_processor(
  shp_dt = shp_dt,
  raster_objs = yearly_rasters,
  shp_fn = NULL,
  grid_size = 1000,
  survey_dt = NULL,
  survey_fn = NULL,
  survey_lat = NULL,
  survey_lon = NULL,
  extract_fun = "mean",
  buffer_size = NULL,
  survey_crs = 4326,
  name_set = name_set
)



devtools::document()   # Regenerate documentation
devtools::install()    # Reinstall the package
library(geolink)       # Load your package


df_survey <- geolink_CMIP6(start_date = "2019-01-01", end_date = "2019-12-31",
                      scenario = "ssp245", desired_models = "UKESM1-0-LL",
                      shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                       survey_dt = hhgeo_dt[hhgeo_dt$ADM1_EN == "Abia",])



