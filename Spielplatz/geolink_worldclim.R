pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf, geodata)


geolink_worldclim <- function(var,
                              res,
                              lon = NULL,
                              lat = NULL,
                        location = NULL,
                        shp_dt = NULL,
                        shp_fn = NULL,
                        grid_size = 1000,
                        survey_dt = NULL,
                        survey_fn = NULL,
                        survey_lat = NULL,
                        survey_lon = NULL,
                        buffer_size = NULL,
                        extract_fun = "mean",
                        survey_crs = 4326){


  data <- worldclim_tile(var=var, res=res, lon=lon, lat=lat, version="2.1", path = tempdir())

  xy <- xyFromCell(data[[1]], 1:ncell(data[[1]]))

  df <- data.frame(lon = xy[,1], lat = xy[,2])

  layer_values <- extract(data, xy)

  df$value <- layer_values

  rows_to_keep <- complete.cases(df[, -(1:2)])


  print("WorldClim Raster Downloaded")


  print("Process Complete!!!")

  return(df)}


df <- geolink_worldclim(var='tmax', res=2.5, lon = 45, lat = 5)

