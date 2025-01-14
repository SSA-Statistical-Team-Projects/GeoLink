# Load required libraries
pacman::p_load(rstac, reticulate, terra, raster, osmdata, sf, geodata, exactextractr, lubridate)

geolink_electaccess <- function(
    start_date = NULL,
    end_date = NULL,
    shp_dt = NULL,
    shp_fn = NULL,
    grid_size = 1000,
    survey_dt = NULL,
    survey_fn = NULL,
    survey_lat = NULL,
    survey_lon = NULL,
    buffer_size = NULL,
    extract_fun = "mean",
    survey_crs = 4326
) {

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  it_obj <- s_obj %>%
    stac_search(
      collections = "hrea",
      bbox = sf::st_bbox(shp_dt),
      datetime = paste(start_date, end_date, sep = "/")
    ) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  # Modify url_list extraction
  url_list <- lapply(1:length(it_obj$features), function(x) {
    urls <- list(
      lightscore = paste0("/vsicurl/", it_obj$features[[x]]$assets$lightscore$href),
      light_composite = paste0("/vsicurl/", it_obj$features[[x]]$assets$`light-composite`$href),
      night_proportion = paste0("/vsicurl/", it_obj$features[[x]]$assets$`night-proportion`$href),
      estimated_brightness = paste0("/vsicurl/", it_obj$features[[x]]$assets$`estimated-brightness`$href)
    )
    return(urls)
  })

  raster_objs <- lapply(url_list, function(urls) {
    # Convert each URL to a SpatRaster
    lapply(urls, function(url) {
      tryCatch({
        terra::rast(url)
      }, error = function(e) {
        warning(paste("Could not convert URL to SpatRaster:", url, "Error:", e$message))
        return(NULL)
      })
    })
  })

  # Flatten the list of lists to a single list of 8 SpatRasters
  raster_objs <- unlist(raster_objs, recursive = FALSE)


  # Create name_set with all 4 indicators for each year
  year_sequence <- seq(lubridate::year(start_date), lubridate::year(end_date))

  # Generate name_set with all indicators
  name_set <- unlist(lapply(year_sequence, function(year) {
    c(
      paste0("lightscore_", year),
      paste0("light_composite_", year),
      paste0("night_proportion_", year),
      paste0("estimated_brightness_", year)
    )
  }))



  print("Electrification Access Raster Downloaded")

  dt <- postdownload_processor(
    shp_dt = shp_dt,
    raster_objs = raster_objs,
    shp_fn = shp_fn,
    grid_size = grid_size,
    survey_dt = survey_dt,
    survey_fn = survey_fn,
    survey_lat = survey_lat,
    survey_lon = survey_lon,
    extract_fun = extract_fun,
    buffer_size = buffer_size,
    survey_crs = survey_crs,
    name_set = name_set
  )

  print("Process Complete!!!")

  return(dt)
}


df = geolink_electaccess(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",], start_date = "2018-12-31", end_date = "2019-12-31")

