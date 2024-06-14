pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf, geodata)

geolink_elevation <- function(shp_dt,
                             shp_fn = NULL,
                             grid_size = 1000,
                             survey_dt,
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

  unlink(tempdir(), recursive = TRUE)

  data <- geodata::elevation_3s(lon=lon, lat=lat, path=tempdir())

  tif_files <- list.files(tempdir(), pattern = "\\.tif$", full.names = TRUE)

  name_set <- c()

  for (file in tif_files) {
    base_name <- basename(file)

    extracted_string <- sub("\\.tif$", "", base_name)

    name_set <- c(name_set, extracted_string)
  }

  raster_objs <- lapply(tif_files, terra::rast)

  raster_list <- lapply(raster_objs, raster)

  epsg_4326 <- "+init=EPSG:4326"

  for (i in seq_along(raster_list)) {
    projection(raster_list[[i]]) <- epsg_4326
    if (is.null(projection(raster_list[[i]]))) {
      print(paste("Projection failed for raster", i))
    } else {
      print(paste("Raster", i, "projected successfully."))
    }
  }

  print("Elevation Raster Downloaded")


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


df <- geolink_elevation(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])


