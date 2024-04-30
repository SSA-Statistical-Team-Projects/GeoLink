
geolink_CMIP6 <- function(var,
                              res,
                              model,
                              ssp,
                              time,
                              shp_dt,
                              shp_fn = NULL,
                              grid_size = 1000,
                              survey_dt = NULL,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              extract_fun = "mean",
                              survey_crs = 4326){


  if (!is.null(shp_dt)) {
    coords <- st_coordinates(shp_dt)
    midpoint <- ceiling(nrow(coords) / 2)
    lon <- coords[midpoint, "X"]
    lat <- coords[midpoint, "Y"]
  } else if (!is.null(shp_fn)) {
    shp_dt <- st_read(shp_fn)
    coords <- st_coordinates(shp_dt)
    midpoint <- ceiling(nrow(coords) / 2)
    lon <- coords[midpoint, "X"]
    lat <- coords[midpoint, "Y"]
  } else {
    stop("Provide either shp_dt or shp_fn.")
  }


  data <- cmip6_tile(var=var, res=res, lon=lon, lat=lat, model = model, ssp = ssp, time = time, path = tempdir())

  tif_files <- list.files(tempdir(), pattern = "\\.tif$", full.names = TRUE)

  raster_objs <- lapply(tif_files, terra::rast)

  raster_list <- lapply(raster_objs, raster)

  for (i in seq_along(raster_list)) {
    projection(raster_list[[i]]) <- epsg_4326
    if (is.null(projection(raster_list[[i]]))) {
      print(paste("Projection failed for raster", i))
    } else {
      print(paste("Raster", i, "projected successfully."))
    }
  }

  name_set <- paste0("elevation_")


  print("CMIP6  Raster Downloaded")

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

  return(df)}


bio10 <- geolink_CMIP6(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],"CNRM-CM6-1", "585", "2061-2080", var="bioc", res=10)

