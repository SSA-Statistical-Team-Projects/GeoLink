pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf, geodata)


geolink_electaccess <- function(time_unit = "annual",
                               start_date = NULL,
                               end_date = NULL,
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



  #start_date <- "2018-12-31"
  #end_date <- "2019-12-31"

  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")


  #year_start <- "2018-01-01T00:00:00Z"
  #year_end <- "2019-01-01T00:00:00Z"


  it_obj <- s_obj %>%
    stac_search(collections = "hrea",
                bbox = sf::st_bbox(shp_dt)) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  url_list <- lapply(1:length(it_obj$features),
                     function(x) {
                       url <- paste0("/vsicurl/", it_obj$features[[x]]$assets$lightscore$href)
                       return(url)
                     })

  raster_objs <- lapply(url_list, terra::rast)

  raster_list <- lapply(raster_objs, raster)

  name_set <- paste0("elect_")

  print("Electrification Access Raster Downloaded")

  dt <- postdownload_processor(shp_dt = shp_dt,
                               raster_objs = raster_list,
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

  print("Process Complete!!!")

  return(dt)}


df <- geolink_electaccess(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])




