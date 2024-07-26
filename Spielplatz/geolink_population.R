pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf, geodata, rvest, httr)

geolink_population <- function(start_year = NULL,
                               end_year = NULL,
                               iso_code,
                               UN_adjst = NULL,
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
                               survey_crs = 4326,
                               file_location = tempdir()) {

  if (!dir.exists(file_location)) {
    dir.create(file_location, recursive = TRUE)
  }

  if (!is.null(start_year) && !is.null(end_year)) {
    years <- seq(start_year, end_year)
    result_list <- paste0("ppp_", years)
  }

  if (!is.null(constrained) && constrained == "Y") {
    url1 <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/BSGM/", iso_code, "/")
    url2 <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/maxar_v1/", iso_code, "/")

    file_urls <- try_download(url1)
    if (is.null(file_urls)) {
      file_urls <- try_download(url2)
    }

    if (!is.null(file_urls)) {
      download_files_worldpop(file_urls, UN_adjst, file_location)
    } else {
      warning("No files found at both URLs.")
    }
  } else {
    if (!is.null(bespoke) && bespoke == "Y") {
      url <- paste0("https://data.worldpop.org/repo/wopr/", iso_code,
                    "/population/v", version, "/", iso_code, "_population_v",
                    gsub("\\.", "_", version), "_mastergrid.tif")
      download.file(url, file.path(file_location, basename(url)))
    } else {
      for (year in years) {
        url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020/", year, "/", iso_code, "/")

        file_urls <- try_download(url)

        if (!is.null(file_urls)) {
          download_files_worldpop(file_urls, UN_adjst, file_location)
        } else {
          warning(paste("No files found for year", year, "at URL", url))
        }
      }
    }
  }

  tif_files <- list.files(file_location, pattern = "\\.tif$", full.names = TRUE)

  raster_objs <- lapply(tif_files, function(x) {
    tryCatch({
      terra::rast(x)
    }, error = function(e) {
      warning(paste("Failed to read:", x, "with error:", e))
      return(NULL)
    })
  })

  raster_objs <- raster_objs[!sapply(raster_objs, is.null)]

  if (length(raster_objs) == 0) {
    stop("No valid raster files found.")
  }

  if (!is.null(start_year) && !is.null(end_year)) {
    year_sequence <- seq(start_year, end_year)
  } else {
    year_sequence <- start_year
  }

  name_set <- paste0("population_", year_sequence)

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
df <- geolink_population(iso_code = "NGA",
                         UN_adjst = "N",
                         constrained = "Y",
                         shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                         grid_size = 1000,
                         extract_fun = "mean",
                         file_location = "C:/Users/Diana Jaganjac/Documents/New folder")
