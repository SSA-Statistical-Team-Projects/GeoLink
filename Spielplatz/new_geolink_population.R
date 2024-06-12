pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf, geodata)


geolink_population <- function(start_year,
                               end_year,
                               iso_code,
                               UN_adjst,
                               constrained = NULL,
                               bespoke = NULL,
                               version = NULL,
                               shp_dt = NULL,
                               shp_fn = NULL,
                               grid_size = 1000,
                               survey_dt,
                               survey_fn = NULL,
                               survey_lat = NULL,
                               survey_lon = NULL,
                               buffer_size = NULL,
                               extract_fun = "mean",
                               survey_crs = 4326) {
  temp_dir <- tempdir()

  years <- seq(start_year, end_year)

  result_list <- paste0("ppp_", years)

  if (!is.null(constrained) && constrained == "Y") {
    url2 <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/BSGM/", iso_code)
    url1 <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/maxar_v1/", iso_code)

    tryCatch({
      download.file(url1, file.path(temp_dir, basename(url1)))
    }, error = function(e) {
      tryCatch({
        download.file(url2, file.path(temp_dir, basename(url2)))
      }, error = function(e) {
        message("No file found")
      })
    })

  } else {
    for (year in years) {
      url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020", year, "/",
                    iso_code, "/")
      download.file(url, file.path(temp_dir, basename(url)))
    }
  }

  if (!is.null(bespoke) && bespoke == "Y") {
    url <- paste0("https://data.worldpop.org/repo/wopr/", iso_code,
                  "/population/", version, "_population_",
                  gsub("\\.", "_", version), "_mastergrid.tif")
    download.file(url, file.path(temp_dir, basename(url)))
  }

  if (!is.null(UN_adjst) && UN_adjst == "Y") {
    tif_files <- list.files(temp_dir, pattern = paste0(iso_code, "_ppp_\\d{4}_UNadj\\.tif$"), full.names = TRUE)
  } else {
    tif_files <- list.files(temp_dir, pattern = "\\.tif$", full.names = TRUE)
  }

  raster_objs <- lapply(tif_files, terra::rast)

  raster_list <- lapply(raster_objs, raster)

  year_sequence <- seq(lubridate::year(start_date), lubridate::year(end_date))

  name_set <- paste0("population_", year_sequence)

  dir_contents <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
  print(dir_contents)

  print(length(raster_objs))

  print("Population Raster Downloaded")

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

  print("Process Complete!!!")

  return(dt)
}




df <- geolink_population(start_year = 2018,
                         end_year = 2019,
                         iso_code = "NGA",
                         UN_adjst = "Y",
                         constrained = "Y",
                         shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                         grid_size = 1000,
                         extract_fun = "mean")
