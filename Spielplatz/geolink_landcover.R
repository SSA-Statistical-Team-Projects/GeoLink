
geolink_landcover <- function(time_unit = "annual",
                           start_date,
                           end_date,
                           shp_dt,
                           shp_fn = NULL,
                           grid_size = 1000,
                           survey_dt,
                           survey_fn = NULL,
                           survey_lat = NULL,
                           survey_lon = NULL,
                           buffer_size = NULL,
                           extract_fun = "mean",
                           survey_crs = 4326){


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
                     function(x){

                       url <- paste0("/vsicurl/", it_obj$features[[x]]$assets$data$href)

                       return(url)

                     })

  raster_objs <- lapply(url_list,
                     terra::rast)

  name_count <- lubridate::year(end_date) - lubridate::year(start_date) + 1

  name_set <- paste0("landcover_", "annual", 1:name_count)

  dt <- postdownload_processor(shp_dt = shp_dt,
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
                               name_set = name_set)

  return(dt)}


#df <- geolink_landcover(time_unit,
                      #start_date = "2020-01-01",
                      #end_date = "2020-03-01",
                      #shp_dt = shp_dt,
                      #grid_size = 1000,
                      #survey_dt = hhgeo_dt,
                      #extract_fun = "mean")



