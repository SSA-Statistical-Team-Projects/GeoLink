pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf, geodata, rvest, httr)

geolink_population <- function(time_unit = "annual",
                               start_year = NULL,
                               end_year = NULL,
                               iso_code,
                               UN_adjst = NULL,
                               constrained = NULL,
                               bespoke = NULL,
                               version = NULL,
                               shp_dt,
                               shp_fn = NULL,
                               grid_size = 1000,
                               survey_dt = NULL,
                               survey_fn = NULL,
                               survey_lat = NULL,
                               survey_lon = NULL,
                               buffer_size = NULL,
                               extract_fun = "mean",
                               survey_crs = 4326) {

  if (!is.null(start_year) && !is.null(end_year)) {
    years <- seq(start_year, end_year)
    result_list <- paste0("ppp_", years)
  }


  if (!is.null(constrained) && constrained == "Y") {
    url1 <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/BSGM/", iso_code, "/")
    url2 <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/maxar_v1/", iso_code, "/")

    # Try url1 first, then url2 if url1 fails
    file_urls <- try_download(url1)
    if (is.null(file_urls)) {
      file_urls <- try_download(url2)
    }

    # If we have a list of file URLs, proceed to download
    if (!is.null(file_urls)) {
      download_files(file_urls, UN_adjst)
    } else {
      warning("No files found at both URLs.")
    }
  } else {
    if (!is.null(bespoke) && bespoke == "Y") {
      url <- paste0("https://data.worldpop.org/repo/wopr/", iso_code,
                    "/population/v", version, "/", iso_code, "_population_v",
                    gsub("\\.", "_", version), "_mastergrid.tif")
      download.file(url, file.path(tempdir(), basename(url)))
    } else {
      for (year in years) {
        url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020/", year, "/", iso_code, "/")

        file_urls <- try_download(url)

        # If we have a list of file URLs, proceed to download
        if (!is.null(file_urls)) {
          download_files(file_urls, UN_adjst)
        } else {
          warning(paste("No files found for year", year, "at URL", url))
        }
      }
    }
  }


  tif_files <- list.files(tempdir(), pattern = "\\.tif$", full.names = TRUE)

  raster_objs <- lapply(tif_files, terra::rast)

  raster_list <- lapply(raster_objs, raster)

  if (!is.null(start_year) && !is.null(end_year)) {
    name_count <- lubridate::year(start_year) - lubridate::year(end_year) + 1
  } else {
    name_count <- 1
  }

  name_set <- paste0("population_", "annual_", 1:name_count)

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

# Example usage
df <- geolink_population(time_unit = "annual",
                         iso_code = "NGA",
                         UN_adjst = "N",
                         constrained = "Y",
                         shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                         grid_size = 1000,
                         extract_fun = "mean")











