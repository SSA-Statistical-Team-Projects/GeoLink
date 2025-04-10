#' Download and Merge monthly rainfall chirp data into geocoded surveys
#'
#' Download rainfall data from the CHIRPS data at monthly/annual intervals for a specified period
#' The data is downloaded in raster format and combined with shapefile and/or survey data provided
#' by the user
#'
#' @param time_unit A character, either "month" or "annual" monthly or annual rainfall aggregates
#' are to be estimated
#' @param start_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param end_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only)
#' @param grid_size A numeric, the grid size to be used in meters
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey i.e.
#' a household survey with latitude and longitude values.
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only &
#' if use_survey is TRUE)
#' @param survey_lat A character, latitude variable from survey (for STATA users only &
#' if use_survey is TRUE)
#' @param survey_lon A character, longitude variable from survey (for STATA users only &
#' if use survey is TRUE)
#' @param buffer_size A numeric, the size of the buffer for `survey_dt` or `survey_fn` in meters.
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is mean. Other options are "sum", "min", "max", "sd", "skew" and "rms".
#' @param survey_crs A numeric, the default is 4326
#'
#'
#' @details Rainfall data is sourced from the Climate Hazards Group InfraRed Precipitation
#' with Station data (CHIRPS). This a 35+ year quasi-global rainfall dataset. Spanning the
#' entire world from 1981 to date. See https://www.chc.ucsb.edu/data/chirps for more details.
#' The data is extracted into a shapefile provided by user. An added service for tesselating/gridding
#' the shapefile is also provided for users that need this data further analytics that require
#' equal area zonal statistics. Shapefile estimates at the grid or native polygon level is a
#' permitted final output. However, a geocoded survey with rainfall estimates are the end goal
#' if the user also chooses. The function will merge shapefile polygons (either gridded or
#' native polygons) with the location of the survey units i.e. rainfall estimates for the
#' locations of the units within the survey will be returned. The function is also set up for
#' stata users and allows the user to pass file paths for the household survey `survey_fn`
#' (with the lat and long variable names `survey_lon` and `survey_lat`) as well. This is requires
#' a .dta file which is read in with `haven::read_dta()` package. Likewise, the user is permitted
#' to pass a filepath for the location of the shapefile `shp_fn` which is read in with the
#' `sf::read_sf()` function.
#'
#'
#' @examples
#'
#' \dontrun{
#' \donttest{
#'
#'
#' #examples
#' df <- geolink_chirps(time_unit = "month",
#'                      start_date = "2020-01-01",
#'                      end_date = "2020-03-01",
#'                      shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
#'                      grid_size = 1000,
#'                      survey_dt = hhgeo_dt,
#'                      extract_fun = "mean")
#'
#'
#' df <- geolink_chirps(time_unit = "month",
#'                      start_date = "2020-01-01",
#'                      end_date = "2020-02-01",
#'                      survey_fn = "testdata/xy_hhgeo_dt.dta",
#'                      survey_lat = "y",
#'                      survey_lon = "x",
#'                      buffer_size = 1000,
#'                      extract_fun = "mean")
#'
#' }}
#'
#' @export
#' @importFrom raster raster stack brick crop projectRaster
#' @importFrom haven read_dta
#' @importFrom terra rast crs project ext




geolink_chirps <- function(time_unit = NULL,
                           start_date,
                           end_date,
                           shp_dt = NULL,
                           shp_fn = NULL,
                           grid_size = NULL,
                           survey_dt = NULL,
                           survey_fn = NULL,
                           survey_lat = NULL,
                           survey_lon = NULL,
                           buffer_size = NULL,
                           extract_fun = "mean",
                           survey_crs = 4326) {

  # # Only apply ensure_crs_4326 if the spatial inputs are not NULL
  # if (!is.null(shp_dt)) {
  #   shp_dt <- ensure_crs_4326(shp_dt)
  # } else if (!is.null(shp_fn)) {
  #   # If shp_dt is NULL but shp_fn exists, read the file and ensure CRS
  #   shp_dt <- ensure_crs_4326(sf::st_read(shp_fn))
  # }
  #
  # if (!is.null(survey_dt)) {
  #   survey_dt <- ensure_crs_4326(survey_dt)
  # } else if (!is.null(survey_fn)) {
  #   # If survey_dt is NULL but survey_fn exists, read the file and ensure CRS
  #   survey_dt <- ensure_crs_4326(haven::read_dta(survey_fn))
  # }


  ## download the data
  if (time_unit == "month") {

    raster_objs <- get_month_chirps(start_date = start_date,
                                    end_date = end_date)

    name_count <- lubridate::interval(as.Date(start_date),
                                      as.Date(end_date)) %/% months(1) + 1

  } else if (time_unit == "annual") {

    raster_objs <- get_annual_chirps(start_year = lubridate::year(start_date),
                                     end_year = lubridate::year(end_date))

    name_count <- lubridate::year(end_date) - lubridate::year(start_date) + 1

  } else {

    stop("Time unit should either be month or annual")

  }

  print("Global Rainfall Raster Downloaded")

  # Replace -9999.9999 values with NA in downloaded rasters
  raster_objs <- lapply(raster_objs, function(raster_obj) {
    raster_obj[raster_obj == -9999.9999] <- NA
    return(raster_obj)
  })

  name_set <- paste0("rainfall_", time_unit, 1:length(raster_objs))


  ## create the name for the variables

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


#' Download and Merge Monthly and Annual Night Time Light data into geocoded surveys
#'
#' Download night lights luminosity at monthly/annual intervals for a specified period
#' The data is downloaded in raster format and combined with shapefile and/or survey data provided
#' by the user
#'
#' @param time_unit A character, either "month" or "annual" monthly or annual rainfall aggregates
#' are to be estimated
#' @param start_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param end_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only)
#' @param grid_size A numeric, the grid size to be used in meters
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey i.e.
#' a household survey with latitude and longitude values.
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only)
#' @param survey_lat A character, latitude variable from survey (for STATA users only &
#' if use_survey is TRUE)
#' @param survey_lon A character, longitude variable from survey (for STATA users only &
#' if use survey is TRUE)
#' @param buffer_size A numeric, the size of the buffer for `survey_dt` or `survey_fn` in meters.
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' @param survey_crs A numeric, the default is 4326
#' Default is mean. Other options are "sum", "min", "max", "sd", "skew" and "rms".
#' @param month_version A character, the version of month EOG data to use. default set to "v10".
#' @param annual_version A character, the version of annual EOG data for download, default set to "v21"
#' @param indicator A character, specifying the specific indicator of interest. Options are
#' "average", "average_masked", "cf_cvg", "cvg", "lit_mask", "maximum", "median",
#' "median_masked" and "minimum" for annual data and "avg_rade9h", "avg_rade9h.masked", "cf_cvg" or "cvg"
#' for monthly data
#'
#' @inheritParams get_annual_ntl
#' @inheritParams get_month_ntl
#'
#'
#' @details NTL data is sourced from the NASA's Earth Observation Group database.
#' The data is extracted into `shp_dt` shapefile object. An added service for tesselating/gridding
#' the `shp_dt` is also provided if `grid_size` is specified for further analytics that require
#' equal specified area zonal statistics. Shapefile estimates at the grid or native polygon level is a
#' permitted final output. Also, if `survey_dt` is not NULL, the user may compute luminosity within
#' neighborhood of a household given a specified `buffer_size`. The function draw polygons around
#' household locations of `buffer_size` with zonal statistics computed as formerly described.
#' The function is also set up for stata users and allows the user to pass file paths for the
#' household survey `survey_fn` (with the lat and long variable names `survey_lon` and `survey_lat`)
#' as well. This is requires a .dta file which is read in with `haven::read_dta()` package.
#' Likewise, the user is permitted to pass a filepath for the location of the shapefile `shp_fn`
#' which is read in with the `sf::read_sf()` function.
#'
#' @examples
#'
#' \dontrun{
#' \donttest{
#'
#' #loading the survey data and shapefile
#'
#' data("hhgeo_dt")
#' data("shp_dt")
#'
#' #pull monthly night lights and combine with household survey based on
#' #grid tesselation of shapefile at 1000m
#'
#'df <- geolink_ntl(time_unit = "month",
#'            start_date = "2020-01-01",
#'            end_date = "2020-03-01",
#'            shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
#'            indicator = "avg_rade9h",
#'            grid_size = 1000,
#'            extract_fun = "mean")
#'
#' #estimate annual night time luminosity for each household within a 100 meters
#' #of it's location
#'
#' df <- geolink_ntl(time_unit = "annual",
#'                   start_date = "2020-01-01",
#'                   end_date = "2021-12-01",
#'                  survey_dt =  sf::st_as_sf(hhgeo_dt[1:10],
#'                      crs = 4326),
#'                  buffer_size = 100,
#'                  indicator = c("average_masked","cf_cvg"),
#'                  extract_fun = "mean")
#'
#' }}
#
#' @export
#' @import rstac
#' @importFrom rstac stac items_sign
#' @importFrom reticulate py_eval source_python
#' @importFrom terra rast crs project ext
#' @importFrom sf st_bbox st_transform st_as_sf st_geometry st_drop_geometry
#' @importFrom raster projectRaster stack



geolink_ntl <- function(time_unit = "annual",
                        start_date,
                        end_date,
                        annual_version = "v21",
                        month_version = "v10",
                        indicator,
                        slc_type = "vcmslcfg",
                        shp_dt = NULL,
                        shp_fn = NULL,
                        grid_size = NULL,
                        survey_dt = NULL,
                        survey_fn = NULL,
                        survey_lat = NULL,
                        survey_lon = NULL,
                        extract_fun = "mean",
                        buffer_size = NULL,
                        survey_crs = 4326) {

  # shp_dt <- ensure_crs_4326(shp_dt)
  # survey_dt <- ensure_crs_4326(survey_dt)

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  ## download the data
  if (time_unit == "month") {

    raster_objs <- get_month_ntl(start_date = as.Date(start_date),
                                 end_date = as.Date(end_date),
                                 version = month_version,
                                 slc_type = slc_type,
                                 indicator = indicator,
                                 no_tile = TRUE)

    name_count <- lubridate::interval(as.Date(start_date),
                                      as.Date(end_date)) %/% months(1) + 1

  } else if (time_unit == "annual") {

    raster_objs <- lapply(X = year(seq(start_date,
                                       end_date,
                                       "years")),
                          FUN = get_annual_ntl,
                          version = annual_version,
                          indicator = indicator )

    raster_objs <- unlist(raster_objs)

    name_count <- lubridate::year(end_date) - lubridate::year(start_date) + 1

  } else {

    stop("Time unit should either be month or annual")

  }

  print("Global NTL Raster Downloaded")

  name_set <- paste0("ntl_", time_unit, 1:length(raster_objs), indicator)

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

  unlink(tempdir(), recursive = TRUE)
}


#' Download and Merge Annual Population data into geocoded surveys
#'
#' Download Population data from the World Pop dataset at annual intervals for a specified period
#' The data is downloaded in raster format and combined with shapefile and/or survey data provided
#' by the user. Source data: https://www.worldpop.org/
#'
#' The individual country datasets use modelling methods found it Stevens et al. The 'Global per country 2000-2020'
#' The datasets provided are the outputs of a project utilising various modelling methods to develop consistent 100m
#' resolution population count datasets for all countries of the World for each year 2000-2020.
#'
#'
#' @param start_year A numeric specifying the start year
#' @param end_year A numeric specifying the end year, if only one year is required then enter start year into
#' @param iso_code A character, specifying the iso code for country to download data from
#' @param UN_adjst A character 'Y' if want 2020 UN adjusted dataset returned, if not leave blank.
#' @param constrained A character 'Y' if want constrained dataset returned, if not leave blank.
#' @param bespoke A character 'Y' if want bespoke dataset returned, if not leave blank. Version also should be specified.
#' @param version A character such as v2.0 or v2.1 which correlates to the iso_code provided for bespoke datasets.
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only)
#' @param grid_size A numeric, the grid size to be used in meters
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey i.e.
#' a household survey with latitude and longitude values.
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only &
#' if use_survey is TRUE)
#' @param survey_lat A character, latitude variable from survey (for STATA users only &
#' if use_survey is TRUE)
#' @param survey_lon A character, longitude variable from survey (for STATA users only &
#' if use survey is TRUE)
#' @param buffer_size A numeric, the size of the buffer for `survey_dt` or `survey_fn` in meters.
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is mean. Other options are "sum", "min", "max", "sd", "skew" and "rms".
#' @param survey_crs A numeric, the default is 4326
#' @param file_location A path to the folder where the downloaded data should be stored.
#'
#' @details Population data is sourced from WorldPop.
#' The data is extracted into a shapefile provided by user. An added service for tesselating/gridding
#' the shapefile is also provided for users that need this data further analytics that require
#' equal area zonal statistics. Shapefile estimates at the grid or native polygon level is a
#' permitted final output. However, a geocoded survey with population estimates are the end goal
#' if the user also chooses. The function will merge shapefile polygons (either gridded or
#' native polygons) with the location of the survey units i.e. population estimates for the
#' locations of the units within the survey will be returned. The function is also set up for
#' stata users and allows the user to pass file paths for the household survey `survey_fn`
#' (with the lat and long variable names `survey_lon` and `survey_lat`) as well. This is requires
#' a .dta file which is read in with `haven::read_dta()` package. Likewise, the user is permitted
#' to pass a filepath for the location of the shapefile `shp_fn` which is read in with the
#' `sf::read_sf()` function.
#'
#'
#' @examples
#'
#' \dontrun{
#' \donttest{
#'
#'
#'
#'df <- geolink_population(start_year = 2018,
#'                            end_year = 2019,
#'                           iso_code = "NGA",
#'                            UN_adjst = "N",
#'                            constrained = "N",
#'                            shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'                            grid_size = 1000,
#'                            extract_fun = "mean",
#'                            file_location = tempdir())
#'
#' }}
#' @export
#' @importFrom terra rast crs project ext
#' @importFrom raster stack brick projectRaster
#' @importFrom sf st_bbox st_transform st_as_sf st_geometry
#' @importFrom geodata elevation_30s
#' @importFrom httr GET write_disk http_type
#'
#' @import terra
#' @import raster
#' @import sf
#' @import geodata
#' @import httr

geolink_population <- function(start_year = NULL,
                               end_year = NULL,
                               iso_code,
                               UN_adjst = NULL,
                               constrained = NULL,
                               bespoke = NULL,
                               version = NULL,
                               shp_dt = NULL,
                               shp_fn = NULL,
                               grid_size = NULL,
                               survey_dt = NULL,
                               survey_fn = NULL,
                               survey_lat = NULL,
                               survey_lon = NULL,
                               buffer_size = NULL,
                               extract_fun = "mean",
                               survey_crs = 4326,
                               file_location = tempdir()) {

  clear_temp = TRUE

  # Clear temporary directory if requested and if it's the default tempdir()
  if (clear_temp && file_location == tempdir()) {
    message("Clearing temporary directory...")
    temp_files <- list.files(tempdir(), full.names = TRUE)
    unlink(temp_files, recursive = TRUE, force = TRUE)
  }

  if (!dir.exists(file_location)) {
    dir.create(file_location, recursive = TRUE)
  }

  if (!is.null(start_year) && !is.null(end_year)) {
    years <- seq(start_year, end_year)
  }

  # Check for existing .tif files
  year_pattern <- paste(years, collapse = "|")
  tif_files <- list.files(file_location,
                          pattern = ".tif$",
                          full.names = TRUE)
  if (constrained == "Y" && UN_adjst == "Y" ){
    tif_files <- tif_files[grepl(iso_code, tif_files, ignore.case = TRUE) &
                             grepl("ppp", tif_files, ignore.case = TRUE) &
                             grepl("UNadj_constrained", tif_files, ignore.case = TRUE) &
                             grepl(year_pattern, tif_files)]
  } else if (constrained == "N" && UN_adjst == "Y"){
    tif_files <- tif_files[grepl(iso_code, tif_files, ignore.case = TRUE) &
                             grepl("ppp", tif_files, ignore.case = TRUE) &
                             grepl("constrained", tif_files, ignore.case = TRUE) &
                             grepl(year_pattern, tif_files)]
  }else if (constrained == "Y" && UN_adjst == "N"){
    tif_files <- tif_files[grepl(iso_code, tif_files, ignore.case = TRUE) &
                             grepl("ppp", tif_files, ignore.case = TRUE) &
                             grepl("UNadj", tif_files, ignore.case = TRUE) &
                             grepl(year_pattern, tif_files)]
  }else {
    tif_files <- tif_files[grepl(iso_code, tif_files, ignore.case = TRUE) &
                             grepl("ppp", tif_files, ignore.case = TRUE) &
                             grepl(year_pattern, tif_files)]
  }

  # If .tif files are already present, skip downloading
  if (length(tif_files) == 0) {
    if (!is.null(constrained) && constrained == "Y") {
      url1 <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/BSGM/", iso_code, "/")
      url2 <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/maxar_v1/", iso_code, "/")

      file_urls <- try_download(url1, UN_adjst)
      if (is.null(file_urls)) {
        file_urls <- try_download(url2, UN_adjst)
      }

      file_urls <- file_urls[grepl(iso_code, file_urls, ignore.case = TRUE) &
                               grepl("ppp", file_urls, ignore.case = TRUE) &
                               grepl(year_pattern, file_urls)]
      if (!is.null(file_urls)) {
        download_files_worldpop(file_urls, file_location)
      } else {
        warning("No files found at both URLs.")
      }
    } else {
      if (!is.null(bespoke) && bespoke == "Y") {
        url <- paste0("https://data.worldpop.org/repo/wopr/", iso_code,
                      "/population/v", version, "/", iso_code, "_population_v",
                      gsub("\\.", "_", version), "_mastergrid.tif")

        # Use httr_download instead of download.file
        dest_file <- file.path(file_location, basename(url))
        download_status <- httr_download(url, dest_file)

        if (!download_status) {
          warning(paste("Failed to download file from URL:", url))
        }
      } else {
        if (!is.null(start_year) && !is.null(end_year)) {
          for (year in years) {
            url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020/", year, "/", iso_code, "/")

            # Pass UN_adjst parameter to try_download to filter files appropriately
            file_urls <- try_download(url, UN_adjst)

            if (!is.null(file_urls)) {
              # We've already filtered based on UN_adjst, so no need to pass it again
              download_files_worldpop(file_urls, file_location)
            } else {
              # Try direct URL construction as a fallback
              if (!is.null(UN_adjst)) {
                direct_url <- if(UN_adjst == "Y") {
                  paste0(url, tolower(iso_code), "_ppp_", year, "_UNadj.tif")
                } else {
                  paste0(url, tolower(iso_code), "_ppp_", year, ".tif")
                }

                if(check_url_status(direct_url)) {
                  dest_file <- file.path(file_location, basename(direct_url))
                  download_status <- httr_download(direct_url, dest_file)
                  if (!download_status) {
                    warning(paste("Failed to download file from direct URL:", direct_url))
                  }
                } else {
                  warning(paste("No files found for year", year, "at URL", url, "with UN_adjst =", UN_adjst))
                }
              } else {
                warning(paste("No files found for year", year, "at URL", url))
              }
            }
          }
        }
      }
    }

    # Update tif_files after download
    year_pattern <- paste(years, collapse = "|")
    tif_files <- list.files(file_location,
                            pattern = ".tif$",
                            full.names = TRUE)
    if (constrained == "Y" && UN_adjst == "Y" ){
      tif_files <- tif_files[grepl(iso_code, tif_files, ignore.case = TRUE) &
                               grepl("ppp", tif_files, ignore.case = TRUE) &
                               grepl("UNadj_constrained", tif_files, ignore.case = TRUE) &
                               grepl(year_pattern, tif_files)]
    } else if (constrained == "N" && UN_adjst == "Y"){
      tif_files <- tif_files[grepl(iso_code, tif_files, ignore.case = TRUE) &
                               grepl("ppp", tif_files, ignore.case = TRUE) &
                               grepl("constrained", tif_files, ignore.case = TRUE) &
                               grepl(year_pattern, tif_files)]
    }else if (constrained == "Y" && UN_adjst == "N"){
      tif_files <- tif_files[grepl(iso_code, tif_files, ignore.case = TRUE) &
                               grepl("ppp", tif_files, ignore.case = TRUE) &
                               grepl("UNadj", tif_files, ignore.case = TRUE) &
                               grepl(year_pattern, tif_files)]
    }else {
      tif_files <- tif_files[grepl(iso_code, tif_files, ignore.case = TRUE) &
                               grepl("ppp", tif_files, ignore.case = TRUE) &
                               grepl(year_pattern, tif_files)]
    }
  } else {
    print("Using existing .tif files in file_location.")
  }

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

  # Define the name_set at the end
  if (!is.null(bespoke) && bespoke == "Y") {
    name_set <- paste0("population_", version)
  } else if (!is.null(start_year) && !is.null(end_year)) {
    name_set <- paste0("population_", years)
  } else {
    name_set <- "population_2020"
  }

  print("Population Raster Processed")

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


#' Download high resolution elevation data based on shapefile coordinates
#'
#' @details This function downloads high-resolution elevation data based on the coordinates
#' provided by either a shapefile or a file path to a shapefile. It can also incorporate
#' survey data for further analysis. The elevation data is downloaded using the `elevation_3s` function
#' and post-processed using the `postdownload_processor` function. The data is extracted into a shapefile
#' provided by user. An added service for tesselating/gridding
#' the shapefile is also provided for users that need this data further analytics that require
#' equal area zonal statistics. Shapefile estimates at the grid or native polygon level is a
#' permitted final output. However, a geocoded survey with population estimates are the end goal
#' if the user also chooses. The function will merge shapefile polygons (either gridded or
#' native polygons) with the location of the survey units i.e. population estimates for the
#' locations of the units within the survey will be returned. The function is also set up for
#' stata users and allows the user to pass file paths for the household survey `survey_fn`
#' (with the lat and long variable names `survey_lon` and `survey_lat`) as well. This is requires
#' a .dta file which is read in with `haven::read_dta()` package. Likewise, the user is permitted
#' to pass a filepath for the location of the shapefile `shp_fn` which is read in with the
#' `sf::read_sf()` function.
#'
#' @param iso_code A character, specifying the iso code for country to download data from
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons representing the study area.
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only).
#' @param grid_size A numeric, the grid size to be used in meters for analyzing the elevation data.
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey with latitude and longitude values (optional).
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lat A character, latitude variable from survey (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lon A character, longitude variable from survey (for STATA users only & if use survey is TRUE) (optional).
#' @param buffer_size A numeric, the buffer size to be used around each point in the survey data, in meters (optional).
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is "mean". Other options are "sum", "min", "max", "sd", "skew" and "rms" (optional).
#' @param survey_crs An integer, the Coordinate Reference System (CRS) for the survey data. Default is 4326 (WGS84) (optional).
#'
#' @return A processed data frame or object based on the input parameters and downloaded data.
#'
#'
#' @examples
#'
#' \dontrun{
#' \donttest{
#'
#'  #example usage with shapefile
#'test_dt <- geolink_elevation(iso_code = "NGA",
#'                             shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'                             grid_size = 1000,
#'                             extract_fun = "mean")
#' }}
#' @export
#' @importFrom terra rast crs project ext
#' @importFrom sf st_bbox st_transform st_as_sf
#' @importFrom geodata elevation_30s

geolink_elevation <- function(iso_code,
                              shp_dt = NULL,
                              shp_fn = NULL,
                              grid_size = NULL,
                              survey_dt = NULL,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              extract_fun = "mean",
                              survey_crs = 4326){

  # shp_dt <- ensure_crs_4326(shp_dt)
  # survey_dt <- ensure_crs_4326(survey_dt)



  if(!is.null(iso_code)){
    print(paste("Checking data for", iso_code))
  } else{
    stop("Please input a valid country Name or ISO3 Code")
  }

  unlink(tempdir(), recursive = TRUE)

  data <- geodata::elevation_30s(country = iso_code, path=tempdir())

  tif_files <- list.files(tempdir(), pattern = "\\.tif$", full.names = TRUE,
                          recursive = TRUE)

  name_set <- c()

  for (file in tif_files) {
    base_name <- basename(file)

    extracted_string <- sub("\\.tif$", "", base_name)

    name_set <- c(name_set, extracted_string)
  }

  raster_list <- lapply(tif_files, terra::rast)

  # epsg_4326 <- "+init=EPSG:4326"
  #
  # for (i in seq_along(raster_list)) {
  #   terra::crs(raster_list[[i]]) <- epsg_4326
  #   if (is.null(terra::crs(raster_list[[i]]))) {
  #     print(paste("Projection failed for raster", st_crs(raster_list[[i]])$input))
  #   } else {
  #     print(paste("Raster", i, "projected successfully."))
  #   }
  # }

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

  return(dt)
  unlink(tempdir(), recursive = TRUE)
}


#' Download high resolution building data from WorldPop
#'
#' @details This function downloads high-resolution building data from WorldPop based
#' on the specified version and ISO country code. It can incorporate survey data
#' for further analysis. The building data is downloaded as raster files and can
#' be processed and analyzed using the `postdownload_processor` function.The data is extracted into
#' a shapefile provided by user. An added service for tesselating/gridding
#' the shapefile is also provided for users that need this data further analytics that require
#' equal area zonal statistics. Shapefile estimates at the grid or native polygon level is a
#' permitted final output. However, a geocoded survey with population estimates are the end goal
#' if the user also chooses. The function will merge shapefile polygons (either gridded or
#' native polygons) with the location of the survey units i.e. population estimates for the
#' locations of the units within the survey will be returned. The function is also set up for
#' stata users and allows the user to pass file paths for the household survey `survey_fn`
#' (with the lat and long variable names `survey_lon` and `survey_lat`) as well. This is requires
#' a .dta file which is read in with `haven::read_dta()` package. Likewise, the user is permitted
#' to pass a filepath for the location of the shapefile `shp_fn` which is read in with the
#' `sf::read_sf()` function.
#'
#' @param version A character, the version of the building data to download. Options are "v1.1" and "v2.0".
#' @param iso_code A character, the ISO country code for the country of interest.
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons representing the study area.
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only).
#' @param grid_size A numeric, the grid size to be used in meters for analyzing the building data.
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey with latitude and longitude values (optional).
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lat A character, latitude variable from survey (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lon A character, longitude variable from survey (for STATA users only & if use survey is TRUE) (optional).
#' @param buffer_size A numeric, the buffer size to be used around each point in the survey data, in meters (optional).
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is "mean". Other options are "sum", "min", "max", "sd", "skew" and "rms" (optional).
#' @param survey_crs An integer, the Coordinate Reference System (CRS) for the survey data. Default is 4326 (WGS84) (optional).
#' @param indicators character, default = "ALL", the set of indicators of interest
#'
#' @return A processed data frame or object based on the input parameters and downloaded data.
#'
#'
#' @examples
#'
#' \dontrun{
#' \donttest{
#'
#' #example usage with version 1.1
#'test_dt <- geolink_buildings(version = "v1.1",
#'                             iso_code = "NGA",
#'                             shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
#'                             indicators = "ALL",
#'                             grid_size = 1000)
#'
#' }}
#' @export
#' @importFrom httr GET http_type write_disk
#' @importFrom terra rast crs project
#' @importFrom raster projection projectRaster
#' @importFrom sf st_transform st_as_sf st_bbox


geolink_buildings <- function(version,
                              iso_code,
                              shp_dt = NULL,
                              shp_fn = NULL,
                              grid_size = NULL,
                              survey_dt = NULL,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              extract_fun = "mean",
                              survey_crs = 4326,
                              indicators = "ALL"){

  temp_dir <- tempdir()

  if (version == "v1.1") {
    url <- paste0("https://data.worldpop.org/repo/wopr/_MULT/buildings/v1.1/", iso_code, "_buildings_v1_1.zip")
    tryCatch({
      # Download the ZIP file
      response <- GET(url, write_disk(file.path(tempdir(), basename(url)), overwrite = TRUE))
      if (http_type(response) == "application/zip") {
        message("File downloaded successfully.")

        # Unzip the downloaded file
        unzip(file.path(tempdir(), basename(url)), exdir = tempdir())
        message("File unzipped successfully.")
      } else {
        warning("Downloaded file may not be a ZIP file.")
      }
    }, error = function(e) {
      print(e)
    })
  }

  if (version == "v2.0") {
    url <- paste0("https://data.worldpop.org/repo/wopr/_MULT/buildings/v2.0/", iso_code, "_buildings_v2_0.zip")
    tryCatch({
      # Download the ZIP file
      response <- GET(url, write_disk(file.path(tempdir(), basename(url)), overwrite = TRUE))
      if (http_type(response) == "application/zip") {
        message("File downloaded successfully.")

        # Unzip the downloaded file
        unzip(file.path(tempdir(), basename(url)), exdir = tempdir())
        message("File unzipped successfully.")
      } else {
        warning("Downloaded file may not be a ZIP file.")
      }
    }, error = function(e) {
      print(e)
    })
  }


  tif_files <- list.files(path = temp_dir, pattern = "\\.tif$", full.names = TRUE)


  if (!all(indicators== "ALL")) {
    indicators <- paste(indicators, collapse = "|")
    tif_files <- tif_files[grepl(indicators, basename(tif_files))]
  }

  name_set <- c()

  for (file in tif_files) {
    base_name <- basename(file)

    extracted_string <- sub(".*1_([^\\.]+)\\.tif$", "\\1", base_name)

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


  print("Building Raster Downloaded")


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

  unlink(tempdir(), recursive = TRUE)

  return(dt)

}

#' Download CMIP6 climate model data
#'
#' @details This function downloads and processes CMIP6 (Coupled Model Intercomparison Project Phase 6)
#' climate model data for a specific scenario and desired model.
#' It allows for further analysis of the data in conjunction with geographic data.
#' Please see x for the full list of possible models and scenarios which can be downloaded.
#' The data is extracted into a shapefile provided by user. An added service for tesselating/gridding
#' the shapefile is also provided for users that need this data further analytics that require
#' equal area zonal statistics. Shapefile estimates at the grid or native polygon level is a
#' permitted final output. However, a geocoded survey with population estimates are the end goal
#' if the user also chooses. The function will merge shapefile polygons (either gridded or
#' native polygons) with the location of the survey units i.e. population estimates for the
#' locations of the units within the survey will be returned. The function is also set up for
#' stata users and allows the user to pass file paths for the household survey `survey_fn`
#' (with the lat and long variable names `survey_lon` and `survey_lat`) as well. This is requires
#' a .dta file which is read in with `haven::read_dta()` package. Likewise, the user is permitted
#' to pass a filepath for the location of the shapefile `shp_fn` which is read in with the
#' `sf::read_sf()` function.
#'
#' @param start_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param end_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param scenario A scenario has to be selected and can be one of "historical", "ssp245" or "ssp585"
#' @param desired_models The name or names in a list, of the desired model(s) required for the analysis, for example
#'  ("ACCESS-CM-2" or ["ACCESS-CM-2","UKESM1-0-LL" ]). See the cmip6:model summary in the STAC collection for a full list of models.
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons representing the study area.
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only).
#' @param grid_size A numeric, the grid size to be used in meters for analyzing the climate model data.
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey with latitude and longitude values (optional).
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lat A character, latitude variable from survey (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lon A character, longitude variable from survey (for STATA users only & if use survey is TRUE) (optional).
#' @param buffer_size A numeric, the buffer size to be used around each point in the survey data, in meters (optional).
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is "mean". Other options are "sum", "min", "max", "sd", "skew" and "rms" (optional).
#' @param survey_crs An integer, the Coordinate Reference System (CRS) for the survey data. Default is 4326 (WGS84) (optional).
#'
#' @return A processed data frame based on the input parameters and downloaded data.
#'
#'
#' @examples
#'
#' \dontrun{
#' \donttest{
#'
#'  #example usage
#' df <- geolink_CMIP6(start_date = "2019-01-01",
#'                     end_date = "2019-12-31",
#'                     scenario = "ssp245",
#'                    desired_models = "UKESM1-0-LL",
#'                    shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'                    grid_size = 1000)
#' }}
#'
#' @export
#' @import rstac
#' @importFrom httr GET http_type write_disk
#' @importFrom rstac stac items_sign
#' @importFrom terra rast crs project ext nlyr time tapp
#' @importFrom sf st_bbox st_transform st_as_sf
#' @importFrom progress progress_bar

geolink_CMIP6 <- function(start_date,
                          end_date,
                          scenario,
                          desired_models,
                          shp_dt = NULL,
                          shp_fn = NULL,
                          grid_size = NULL,
                          survey_dt = NULL,
                          survey_fn = NULL,
                          survey_lat = NULL,
                          survey_lon = NULL,
                          buffer_size = NULL,
                          extract_fun = "mean",
                          survey_crs = 4326) {

  # # Ensure shapefile and survey are in the correct CRS
  # if (!is.null(shp_dt)) {
  #   sf_obj <- ensure_crs_4326(shp_dt)
  #
  # } else if (!is.null(survey_dt)) {
  #   sf_obj <- ensure_crs_4326(survey_dt)
  #
  # } else if (!is.null(shp_fn)) {
  #   sf_obj <- sf::read_sf(shp_fn)
  #   sf_obj <- ensure_crs_4326(sf_obj)
  #
  # } else if (!is.null(survey_fn)) { # Changed condition to `survey_fn`
  #   sf_obj <- zonalstats_prepsurvey(
  #     survey_dt = survey_dt,
  #     survey_fn = survey_fn,
  #     survey_lat = survey_lat,
  #     survey_lon = survey_lon,
  #     buffer_size = NULL,
  #     survey_crs = survey_crs)
  #   sf_obj <- ensure_crs_4326(sf_obj)
  #
  # } else {
  #   print("Input a valid sf object or geosurvey")
  #   sf_obj <- NULL  # Optional: Define a default value to avoid potential errors
  # }

  sf_obj <- prep_sf_obj_predownload(shp_dt = shp_dt,
                                    shp_fn = shp_fn,
                                    survey_dt = survey_dt,
                                    survey_fn = survey_fn)

  # Set date range
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Create STAC connection
  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  # Retrieve URLs for the specified scenario
  it_obj <- s_obj %>%
    stac_search(
      collections = "nasa-nex-gddp-cmip6",

      bbox = sf::st_bbox(sf_obj),

      datetime = paste(start_date, end_date, sep = "/")
    ) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  # Filter features based on the scenario and desired models
  filtered_features <- Filter(function(feature) {
    feature$properties$`cmip6:scenario` == scenario &&
      feature$properties$`cmip6:model` %in% desired_models
  }, it_obj$features)

  # Extract URLs for each feature
  urls <- lapply(seq_along(filtered_features), function(x) {
    list(
      scenario = scenario,
      pr = filtered_features[[x]]$assets$pr$href,
      tas = filtered_features[[x]]$assets$tas$href,
      hurs = filtered_features[[x]]$assets$hurs$href,
      huss = filtered_features[[x]]$assets$huss$href,
      rlds = filtered_features[[x]]$assets$rlds$href,
      rsds = filtered_features[[x]]$assets$rsds$href,
      tasmax = filtered_features[[x]]$assets$tasmax$href,
      tasmin = filtered_features[[x]]$assets$tasmin$href,
      sfcWind = filtered_features[[x]]$assets$sfcWind$href
    )
  })

  # Function to process rasters and calculate yearly averages
  process_rasters_and_aggregate <- function(urls, filtered_features) {
    temp_dir <- tempdir()  # Temporary directory
    variables <- c("pr", "tas", "hurs", "huss", "rlds", "rsds", "tasmax", "tasmin", "sfcWind")
    raster_list <- list()  # Initialize raster storage

    pb <- progress_bar$new(
      total = length(urls) * length(variables),
      format = "  Downloading and Processing [:bar] :percent (:current/:total)"
    )

    for (i in seq_along(urls)) {
      model <- filtered_features[[i]]$properties$`cmip6:model`
      year <- filtered_features[[i]]$properties$`cmip6:year`
      scenario <- urls[[i]]$scenario

      current_rasters <- list()

      for (var in variables) {
        pb$tick()

        url <- urls[[i]][[var]]
        if (is.null(url)) next

        temp_file <- file.path(temp_dir, paste0(model, "_", var, "_", year, ".nc"))
        tryCatch({
          GET(url, write_disk(temp_file, overwrite = TRUE))
          raster <- rast(temp_file)

          if (nlyr(raster) == 360) {
            time_indices <- as.numeric(format(time(raster), "%Y"))
            yearly_raster <- tapp(raster, index = time_indices, fun = "mean", na.rm = TRUE)
            current_rasters[[var]] <- yearly_raster
          }
        }, error = function(e) {
          warning(paste("Error processing", var, "for model", model, "year", year, ":", e$message))
        })
      }

      if (length(current_rasters) > 0) {
        label <- paste0(model, "_", year, "_", scenario)
        raster_list[[label]] <- current_rasters
      }
    }

    return(raster_list)
  }

  # Process the raster data
  raster_list <- process_rasters_and_aggregate(urls, filtered_features)
  raster_objs <- unlist(raster_list, recursive = FALSE)

  # Generate name_set for variables and years
  year_sequence <- seq(lubridate::year(start_date), lubridate::year(end_date))

  name_set <- unlist(lapply(year_sequence, function(year) {
    paste0(c("pr_", "tas_", "hurs_", "huss_", "rlds_", "rsds_", "tasmax_", "tasmin_", "sfcWind_"), year)
  }))

  # Create the final dataframe using postdownload_processor
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

  # Save the dataframe in the global environment
  #assign("geolink_CMIP6_output", dt, envir = .GlobalEnv)

  #print("Process Complete! DataFrame saved as 'geolink_CMIP6_output' in the environment.")

  return(dt)
  unlink(tempdir(), recursive = TRUE)
}

#' Download cropland data
#'
#' @details This function downloads cropland data from a specified source, such as WorldCover.
#' It allows for further analysis of cropland distribution in a given area.
#' The source of the dataset is X.
#' The data is extracted into a shapefile provided by user. An added service for tesselating/gridding
#' the shapefile is also provided for users that need this data further analytics that require
#' equal area zonal statistics. Shapefile estimates at the grid or native polygon level is a
#' permitted final output. However, a geocoded survey with population estimates are the end goal
#' if the user also chooses. The function will merge shapefile polygons (either gridded or
#' native polygons) with the location of the survey units i.e. population estimates for the
#' locations of the units within the survey will be returned. The function is also set up for
#' stata users and allows the user to pass file paths for the household survey `survey_fn`
#' (with the lat and long variable names `survey_lon` and `survey_lat`) as well. This is requires
#' a .dta file which is read in with `haven::read_dta()` package. Likewise, the user is permitted
#' to pass a filepath for the location of the shapefile `shp_fn` which is read in with the
#' `sf::read_sf()` function.
#'
#' @param source A character, the source of cropland data. Default is "WorldCover".
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons representing the study area.
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only).
#' @param grid_size A numeric, the grid size to be used in meters for analyzing the cropland data.
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey with latitude and longitude values (optional).
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lat A character, latitude variable from survey (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lon A character, longitude variable from survey (for STATA users only & if use survey is TRUE) (optional).
#' @param buffer_size A numeric, the buffer size to be used around each point in the survey data, in meters (optional).
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is "mean". Other options are "sum", "min", "max", "sd", "skew" and "rms" (optional).
#' @param survey_crs An integer, the Coordinate Reference System (CRS) for the survey data. Default is 4326 (WGS84) (optional).
#'
#' @return A processed data frame or object based on the input parameters and downloaded data.
#'
#'
#' @examples
#'
#' \dontrun{
#' \donttest{
#'
#'  #example usage
#'df <- geolink_cropland(shp_dt = shp_dt[shp_dt$ADM1_EN ==  "Abia",],
#'                 grid_size = 1000,
#'                 extract_fun = "mean")
#' }}
#' @export
#' @importFrom terra rast crs
#' @importFrom sf st_transform
#' @importFrom geodata cropland
#' @importFrom httr GET write_disk


geolink_cropland <- function(source = "WorldCover",
                             shp_dt = NULL,
                             shp_fn = NULL,
                             grid_size = NULL,
                             survey_dt = NULL,
                             survey_fn = NULL,
                             survey_lat = NULL,
                             survey_lon = NULL,
                             buffer_size = NULL,
                             extract_fun = "mean",
                             survey_crs = 4326){

  raster_objs <- geodata::cropland(source = source, path = tempdir())

  name_set <- "cropland"

  epsg_4326 <- "+init=EPSG:4326"

  terra::crs(raster_objs) <- epsg_4326
  if (is.null(crs(raster_objs))) {
    print("Projection failed for raster")
  } else {
    print(paste("Raster projected successfully."))
  }

  raster_list <- as.list(raster_objs)

  print("WorldCover Raster Downloaded")

  df <- postdownload_processor(shp_dt = shp_dt,
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

  return(df)
  unlink(tempdir(), recursive = TRUE)
  }

#' Download WorldClim climate data
#'
#' @details This function downloads WorldClim climate data for a specific variable and resolution.
#' It allows for further analysis of climate patterns in a given area.
#' The data is extracted into a shapefile provided by user. An added service for tesselating/gridding
#' the shapefile is also provided for users that need this data further analytics that require
#' equal area zonal statistics. Shapefile estimates at the grid or native polygon level is a
#' permitted final output. However, a geocoded survey with population estimates are the end goal
#' if the user also chooses. The function will merge shapefile polygons (either gridded or
#' native polygons) with the location of the survey units i.e. population estimates for the
#' locations of the units within the survey will be returned. The function is also set up for
#' stata users and allows the user to pass file paths for the household survey `survey_fn`
#' (with the lat and long variable names `survey_lon` and `survey_lat`) as well. This is requires
#' a .dta file which is read in with `haven::read_dta()` package. Likewise, the user is permitted
#' to pass a filepath for the location of the shapefile `shp_fn` which is read in with the
#' `sf::read_sf()` function.
#'
#' @param iso_code A character, specifying the iso code for country to download data from
#' @param var A character, the variable of interest (e.g., "temperature", "precipitation").
#' @param res A character, the resolution of the data (e.g., "2.5m", "5m").
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons representing the study area.
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only).
#' @param grid_size A numeric, the grid size to be used in meters for analyzing the climate data.
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey with latitude and longitude values (optional).
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lat A character, latitude variable from survey (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lon A character, longitude variable from survey (for STATA users only & if use survey is TRUE) (optional).
#' @param buffer_size A numeric, the buffer size to be used around each point in the survey data, in meters (optional).
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is "mean". Other options are "sum", "min", "max", "sd", "skew" and "rms" (optional).
#' @param survey_crs An integer, the Coordinate Reference System (CRS) for the survey data. Default is 4326 (WGS84) (optional).
#'
#' @return A processed data frame or object based on the input parameters and downloaded data.
#'
#'
#' @examples
#'
#' \dontrun{
#' \donttest{
#'
#'  #example usage
#'df <- geolink_worldclim(iso_code ="NGA",
#'                  var='tmax',
#'                  res=2.5,
#'                  shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'                  grid_size = 1000,
#'                  extract_fun = "mean")
#' }}
#'
#' @export
#' @importFrom terra rast crs nlyr
#' @importFrom sf st_transform
#' @importFrom geodata worldclim_country


geolink_worldclim <- function(iso_code,
                              var,
                              res,
                              shp_dt = NULL,
                              shp_fn = NULL,
                              grid_size = NULL,
                              survey_dt = NULL,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              extract_fun = "mean",
                              survey_crs = 4326){

  # shp_dt <- ensure_crs_4326(shp_dt)
  # survey_dt <- ensure_crs_4326(survey_dt)


  if(!is.null(iso_code)){
    print(paste("Checking data for", iso_code))
  } else{
    stop("Please input a valid country Name or ISO3 Code")
  }

  unlink(tempdir(), recursive = TRUE)

  destination_wc <- tempdir()

  raster_file <- geodata::worldclim_country(country = iso_code, version = "2.1",
                                     var = var, res = res, path = destination_wc)

   tif_files <- list.files(destination_wc, pattern = "\\.tif$",
                           full.names = TRUE,
                           recursive = TRUE)

   rasters_combined <- terra::rast(tif_files)

   raster_list <- lapply(1:terra::nlyr(rasters_combined),
                         function(i) rasters_combined[[i]])

  name_set <- c()

  num_layers <- terra::nlyr(rasters_combined)

  months <- month.abb

  name_set <- paste0(iso_code,"_WC_", var, "_", months)

  print("WorldClim Raster Downloaded")

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

  return(dt)
  unlink(tempdir(), recursive = TRUE)
  }



#' Download Terraclimate data
#'
#' @details This function downloads Terraclimate data for a specific variable and year.
#' It allows for further analysis of climate patterns in a given area.
#' The data is extracted into a shapefile provided by user. An added service for tesselating/gridding
#' the shapefile is also provided for users that need this data further analytics that require
#' equal area zonal statistics. Shapefile estimates at the grid or native polygon level is a
#' permitted final output. However, a geocoded survey with population estimates are the end goal
#' if the user also chooses. The function will merge shapefile polygons (either gridded or
#' native polygons) with the location of the survey units i.e. population estimates for the
#' locations of the units within the survey will be returned. The function is also set up for
#' stata users and allows the user to pass file paths for the household survey `survey_fn`
#' (with the lat and long variable names `survey_lon` and `survey_lat`) as well. This is requires
#' a .dta file which is read in with `haven::read_dta()` package. Likewise, the user is permitted
#' to pass a filepath for the location of the shapefile `shp_fn` which is read in with the
#' `sf::read_sf()` function.
#'
#' @param var A character, the variable of interest (e.g., "temperature", "precipitation").
#' @param year A numeric, the year for which data is to be downloaded.
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons representing the study area.
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only).
#' @param grid_size A numeric, the grid size to be used in meters for analyzing the climate data.
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey with latitude and longitude values (optional).
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lat A character, latitude variable from survey (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lon A character, longitude variable from survey (for STATA users only & if use survey is TRUE) (optional).
#' @param buffer_size A numeric, the buffer size to be used around each point in the survey data, in meters (optional).
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is "mean". Other options are "sum", "min", "max", "sd", "skew" and "rms" (optional).
#' @param survey_crs An integer, the Coordinate Reference System (CRS) for the survey data. Default is 4326 (WGS84) (optional).
#'
#' @return A processed data frame or object based on the input parameters and downloaded data.
#'
#'
#' @examples
#'
#' \dontrun{
#' \donttest{
#'
#'  #example usage
#'df <- geolink_terraclimate( var='tmax',
#'                                year = 2017,
#'                                 shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'                                 grid_size = 1000,
#'                                 extract_fun = "mean")
#' }}
#'
#' @export
#' @importFrom terra rast crs nlyr
#' @importFrom sf st_bbox st_transform
#' @importFrom httr GET timeout http_status content
#' @importFrom ncdf4 nc_open nc_close


geolink_terraclimate <- function(var,
                                 year,
                                 shp_dt = NULL,
                                 shp_fn = NULL,
                                 grid_size = NULL,
                                 survey_dt = NULL,
                                 survey_fn = NULL,
                                 survey_lat = NULL,
                                 survey_lon = NULL,
                                 buffer_size = NULL,
                                 extract_fun = "mean",
                                 survey_crs = 4326) {
  # Add httr package
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is needed for this function to work. Please install it.")
  }

  # Ensure CRS is 4326 for both shapefile and survey data if they exist
  if (!is.null(shp_dt)) {
    shp_dt <- ensure_crs_4326(shp_dt)
  }

  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
  }

  # Generate URL
  url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_", var, "_", year, ".nc")
  # Extract the filename from the URL
  filename <- basename(url)
  # Create the destination path
  destination_dir <- tempdir()
  destination <- file.path(destination_dir, filename)
  # Ensure the temporary directory exists
  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
  }
  # Set the timeout
  timeout_seconds <- 240
  # Print the URL for debugging purposes
  print(paste("URL:", url))
  # Perform the GET request
  response <- try(GET(url, timeout(timeout_seconds)), silent = TRUE)
  # Check if the GET request was successful
  if (inherits(response, "try-error")) {
    print("Error performing the GET request.")
    return(NULL)  # Added return to prevent further processing on failure
  } else if (http_status(response)$category == "Success") {
    # Write the content to a file if the status code is 200
    tryCatch({
      writeBin(content(response, "raw"), destination)
      print("File downloaded successfully.")
      print(paste("File saved to:", destination))
    }, error = function(e) {
      print(paste("Error writing the file:", e$message))
      return(NULL)  # Added return to prevent further processing on file write error
    })
    # raster_stack <- stack(destination)
    rasters_combined <- terra::rast(destination)
    # Check CRS and set it if necessary
    if (is.na(terra::crs(rasters_combined))) {
      terra::crs(rasters_combined) <- "+proj=longlat +datum=WGS84 +no_defs"
    }
    #raster_list <- lapply(1:nlayers(raster_stack), function(i) raster_stack[[i]])
    raster_list <- lapply(1:terra::nlyr(rasters_combined), function(i) rasters_combined[[i]])
    #num_layers <- nlayers(raster_stack)
    num_layers <- terra::nlyr(rasters_combined)
    months <- month.abb
    name_set <- paste0(var, "_", months)
    # Set names for the raster layers
    names(raster_list) <- name_set
    print(paste("Names set for raster layers:", paste(names(raster_list), collapse = ", ")))
    print("Terraclimate Raster Downloaded")
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
    # Clean up temp directory after successful processing
    unlink(destination, force = TRUE)
    return(dt)
  } else {
    # Print the error status
    print(paste("Error downloading the file. Status code:", http_status(response)$status_code))
    return(NULL)
  }
}

#' Download points of interest from OSM data using open street maps API.
#'
#' @param osm_key A character, refering to the osm key wiki page, please see details below
#' @param osm_value A character, refering to the osm key wiki page, please see details below
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey i.e. a household survey with latitude and longitude values.
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only & if use_survey is TRUE)
#' @param shp_fn A link to location of shapefile for stat users
#' @param buffer_size buffer area around shapefile or survey points.
#' @param survey_lat A character, latitude variable from survey (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lon A character, longitude variable from survey (for STATA users only & if use survey is TRUE) (optional).
#' @param survey_crs An integer, the Coordinate Reference System (CRS) for the survey data. Default is 4326 (WGS84) (optional).
#' @param grid_size A numeric, the grid size to be used as a buffer around survey points.
#'
#' @details This function downloads open street maps (osm) datapoints based on osn_key and osm_value
#' arguments passed to the function. The full details for osm_key and osm_value arguments
#' can be found here: https://wiki.openstreetmap.org/wiki/Map_features. Either a shapefile, shapefile path,
#' household survey or household survey path can be passed in as an area for analysis.
#' The function works for shapefiles, however is also set up for stata users and allows the user to
#' pass file paths for the household survey `survey_fn`
#' (with the lat and long variable names `survey_lon` and `survey_lat`) as well. This is requires
#' a .dta file which is read in with `haven::read_dta()` package. Likewise, the user is permitted
#' to pass a filepath for the location of the shapefile `shp_fn` which is read in with the
#' `sf::read_sf()` function.
#'
#'
#' @examples
#'
#' \dontrun{
#' \donttest{
#'
#'
#' poi_survey_df <- geolink_get_poi(osm_key = "amenity",
#'                                 buffer_size = 2000,
#'                                 survey_dt = hhgeo_dt[hhgeo_dt$ADM1_EN == "Abia",])
#'
#'
#' poi_survey_fn <- geolink_get_poi(
#'  osm_key = "amenity",
#'  buffer_size = 2000,
#'  survey_fn = "tests/testthat/testdata/xy_hhgeo_dt.dta",
#'  survey_lon = "x",
#'  survey_lat = "y",
#'  survey_crs = 4326)
#'
#'
#' poi_shp = geolink_get_poi(osm_key = "amenity",
#'                          shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#'
#' poi_shp_fn = geolink_get_poi(osm_key = "amenity",
#'                              shp_fn = "tests/testthat/testdata/shp_dt.shp")
#'
#'
#' }}
#' @export
#' @importFrom osmdata available_features available_tags opq add_osm_feature osmdata_sf
#' @importFrom sf st_bbox st_transform st_as_sf st_join st_geometry

# Main function
geolink_get_poi <- function(osm_key,
                            osm_value = NULL,
                            shp_dt = NULL,
                            shp_fn = NULL,
                            survey_dt = NULL,
                            survey_fn = NULL,
                            survey_lat = NULL,
                            survey_lon = NULL,
                            buffer_size = NULL,
                            survey_crs = NULL,
                            grid_size = NULL) {

  max_retries = 3
  timeout = 300
  area_threshold = 1

  # Validate OSM key-value pairs
  if (!osm_key %in% available_features()) {
    stop(sprintf("'%s' is not a valid OSM key", osm_key))
  }
  if (!is.null(osm_value)) {
    available_tags <- available_tags(osm_key)
    if (!osm_value %in% available_tags) {
      warning(sprintf("'%s' may not be a valid value for key '%s'", osm_value, osm_key))
    }
  }

  # Set CRS to 4326 if data is provided as survey_dt
  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
  }


  # Handle survey file input and projection
  if (!is.null(survey_fn)) {
    if (is.null(survey_lat) || is.null(survey_lon)) {
      stop("Both survey_lat and survey_lon must be provided when using survey_fn")
    }

    # Read the survey file
    tryCatch({
      if (grepl("\\.dta$", survey_fn)) {
        survey_dt <- haven::read_dta(survey_fn)
      } else {
        stop("Unsupported file format. Please provide .dta file")
      }
    }, error = function(e) {
      stop(sprintf("Error reading survey file: %s", e$message))
    })

    # Convert to sf object with specified projection
    survey_dt <- st_as_sf(survey_dt,
                          coords = c(survey_lon, survey_lat),
                          crs = survey_crs)

    # Transform to EPSG:4326 if needed
    if (st_crs(survey_dt)$epsg != 4326) {
      survey_dt <- st_transform(survey_dt, 4326)
    }
  }

  # Process input data using helper functions
  if (!is.null(shp_dt)) {
    sf_obj <- zonalstats_prepshp(shp_dt = shp_dt, grid_size = grid_size) %>%
      ensure_crs_4326()
  } else if (!is.null(shp_fn)) {
    sf_obj <- zonalstats_prepshp(shp_fn = shp_fn, grid_size = grid_size) %>%
      ensure_crs_4326()
  } else if (!is.null(survey_dt) || !is.null(survey_fn)) {
    sf_obj <- zonalstats_prepsurvey(
      survey_dt = survey_dt,
      survey_fn = survey_fn,
      survey_lat = survey_lat,
      survey_lon = survey_lon,
      buffer_size = buffer_size,
      survey_crs = survey_crs) %>%
      ensure_crs_4326()
  } else {
    stop("Please provide either a shapefile (shp_dt/shp_fn) or survey data (survey_dt/survey_fn)")
  }

  # Validate input data
  if (nrow(sf_obj) == 0) {
    stop("Input data is empty after filtering")
  }
  if (any(is.na(st_geometry(sf_obj)))) {
    stop("Input contains invalid geometries")
  }

  # Suppress warnings
  oldw <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = oldw))

  # Get bounding box
  bbox <- st_bbox(sf_obj)
  bbox_area <- (bbox["xmax"] - bbox["xmin"]) * (bbox["ymax"] - bbox["ymin"])

  # Process based on area size
  if (bbox_area > area_threshold) {
    message("Large area detected. Splitting into quadrants...")

    # Split into quadrants
    mid_x <- (bbox["xmax"] + bbox["xmin"]) / 2
    mid_y <- (bbox["ymax"] + bbox["ymin"]) / 2

    quadrants <- list(
      q1 = c(xmin = bbox["xmin"], ymin = mid_y, xmax = mid_x, ymax = bbox["ymax"]),
      q2 = c(xmin = mid_x, ymin = mid_y, xmax = bbox["xmax"], ymax = bbox["ymax"]),
      q3 = c(xmin = bbox["xmin"], ymin = bbox["ymin"], xmax = mid_x, ymax = mid_y),
      q4 = c(xmin = mid_x, ymin = bbox["ymin"], xmax = bbox["xmax"], ymax = mid_y)
    )

    # Process each quadrant
    results_list <- lapply(quadrants, function(quad_bbox) {
      process_bbox_quadrant(quad_bbox, osm_key, osm_value)
    })

    # Remove NULL results
    results_list <- results_list[!sapply(results_list, is.null)]

    if (length(results_list) == 0) {
      message("No results found in any quadrant")
      return(NULL)
    }

    # Get union of all column names
    all_cols <- unique(unlist(lapply(results_list, names)))

    # Ensure all data frames have the same columns
    results_list <- lapply(results_list, function(df) {
      missing_cols <- setdiff(all_cols, names(df))
      if (length(missing_cols) > 0) {
        for (col in missing_cols) {
          df[[col]] <- NA
        }
      }
      return(df[, all_cols])
    })

    # Combine results
    results <- do.call(rbind, results_list)

  } else {
    message("Processing area as single unit...")
    features <- get_osm_data(bbox, osm_key, osm_value, max_retries, timeout)
    results <- features$osm_points %>%
      filter(if_any(-c(osm_id, geometry), ~ !is.na(.x)))
  }

  # Check if results exist
  if (is.null(results)) {
    return(NULL)
  }

  # Join results with input geometries
  query_dt <- st_join(results, sf_obj, left = FALSE)

  # Check results
  if (nrow(query_dt) == 0) {
    message("No points of interest found in the specified area")
  } else {
    message(sprintf("Found %d points of interest", nrow(query_dt)))
  }

  message("OpenStreetMap data download complete!")

  return(query_dt)
}


#' Download high resolution electrification access data from HREA
#'
#' @param start_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param end_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only)
#' @param grid_size A numeric, the grid size to be used in meters
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey i.e. a household survey with latitude and longitude values.
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only & if use_survey is TRUE)
#' @param survey_lat A character, latitude variable from survey (for STATA users only & if use_survey is TRUE)
#' @param survey_lon A character, longitude variable from survey (for STATA users only & if use survey is TRUE)
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is mean. Other options are "sum", "min", "max", "sd", "skew" and "rms".
#' @param survey_crs A numeric, the default is 4326
#' @param buffer_size A numeric, the size of the buffer for `survey_dt` or `survey_fn` in meters.
#'
#' @details This function downloads and processes the following electrification access indicators, lightscore,
#' light-composite, nightime proportion and estimated brightness.
#' Details for the dataset can be found here: https://hrea.isr.umich.edu/.
#' The data is extracted into a shapefile provided by user. An added service for tesselating/gridding
#' the shapefile is also provided for users that need this data further analytics that require
#' equal area zonal statistics. Shapefile estimates at the grid or native polygon level is a
#' permitted final output. However, a geocoded survey with population estimates are the end goal
#' if the user also chooses. The function will merge shapefile polygons (either gridded or
#' native polygons) with the location of the survey units i.e. population estimates for the
#' locations of the units within the survey will be returned. The function is also set up for
#' stata users and allows the user to pass file paths for the household survey `survey_fn`
#' (with the lat and long variable names `survey_lon` and `survey_lat`) as well. This is requires
#' a .dta file which is read in with `haven::read_dta()` package. Likewise, the user is permitted
#' to pass a filepath for the location of the shapefile `shp_fn` which is read in with the
#' `sf::read_sf()` function.
#'
#'
#' @examples
#' \dontrun{
#' \donttest{
#'
#'
#' df = geolink_electaccess(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'   start_date = "2018-12-31", end_date = "2019-12-31")
#'
#' elect_shp_fn = geolink_electaccess(shp_fn = "tests/testthat/testdata/shp_dt.shp",
#' start_date = "2018-12-31",
#' end_date = "2019-12-31")
#'
#' elect_survey <- geolink_electaccess(
#'  start_date = "2019-01-01",
#'  end_date = "2019-12-31",
#'  survey_dt = hhgeo_dt[hhgeo_dt$ADM1_EN == "Abia",],
#'  buffer_size = 1000)
#'
#' elect_survey_fn <- geolink_electaccess(start_date = "2019-01-01",
#'                                       end_date = "2019-12-31",
#'                                       survey_fn = "tests/testthat/testdata/xy_hhgeo_dt.dta",
#'                                       survey_lon = "x",
#'                                       survey_lat = "y",
#'                                       buffer_size = 1000)
#' }}
#'
#' @export
#' @import rstac
#' @importFrom rstac stac items_sign
#' @importFrom terra rast crs project ext
#' @importFrom sf st_bbox st_transform st_as_sf st_geometry
#' @importFrom httr GET write_disk
#' @importFrom lubridate year

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

  mosaic_and_crop = TRUE

  # Convert dates to proper format
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Process input data and create sf_obj
  if (!is.null(shp_dt)) {
    sf_obj <- zonalstats_prepshp(shp_dt = shp_dt, grid_size = grid_size) %>%
      ensure_crs_4326()
  } else if (!is.null(shp_fn)) {
    sf_obj <- zonalstats_prepshp(shp_fn = shp_fn, grid_size = grid_size) %>%
      ensure_crs_4326()
  } else if (!is.null(survey_dt) || !is.null(survey_fn)) {
    # Handle survey data input
    if (!is.null(survey_fn)) {
      if (is.null(survey_lat) || is.null(survey_lon)) {
        stop("Both survey_lat and survey_lon must be provided when using survey_fn")
      }

      # Read the survey file
      tryCatch({
        if (grepl("\\.dta$", survey_fn)) {
          survey_dt <- haven::read_dta(survey_fn)
        } else if (grepl("\\.csv$", survey_fn)) {
          survey_dt <- read.csv(survey_fn)
        } else {
          stop("Unsupported file format. Please provide .dta or .csv file")
        }
      }, error = function(e) {
        stop(sprintf("Error reading survey file: %s", e$message))
      })

      # Convert to sf object with specified projection
      survey_dt <- st_as_sf(survey_dt,
                            coords = c(survey_lon, survey_lat),
                            crs = survey_crs)
      # Transform to EPSG:4326 if needed
      if (st_crs(survey_dt)$epsg != 4326) {
        survey_dt <- st_transform(survey_dt, 4326)
      }
    } else if (!is.null(survey_dt)) {
      survey_dt <- ensure_crs_4326(survey_dt)
    }

    sf_obj <- zonalstats_prepsurvey(
      survey_dt = survey_dt,
      buffer_size = buffer_size,
      survey_crs = survey_crs
    ) %>% ensure_crs_4326()
  } else {
    stop("Please provide either a shapefile (shp_dt/shp_fn) or survey data (survey_dt/survey_fn)")
  }

  # Use sf_obj for STAC search
  stac_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1", force_version = "1.0.0")
  bbox <- sf::st_bbox(sf_obj)
  print(paste("Searching with bbox:", paste(bbox, collapse=", ")))

  it_obj <- stac_obj %>%
    stac_search(
      collections = "hrea",
      bbox = bbox,
      datetime = paste(start_date, end_date, sep = "/")
    ) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  print(paste("Found", length(it_obj$features), "features from STAC API"))
  if (length(it_obj$features) > 0) {
    print("First feature properties:")
    print(it_obj$features[[1]]$properties)
  }

  if (length(it_obj$features) == 0) {
    stop("No data found for the specified date range and location")
  }

  # Extract URLs for required assets
  required_assets <- c("lightscore", "light-composite", "night-proportion", "estimated-brightness")
  url_list <- lapply(it_obj$features, function(feature) {
    missing_assets <- required_assets[!required_assets %in% names(feature$assets)]
    if (length(missing_assets) > 0) {
      warning(sprintf("Missing assets: %s", paste(missing_assets, collapse = ", ")))
      return(NULL)
    }

    # Get year from the feature properties
    year <- as.integer(format(as.Date(feature$properties$datetime), "%Y"))

    list(
      lightscore = feature$assets$lightscore$href,
      light_composite = feature$assets$`light-composite`$href,
      night_proportion = feature$assets$`night-proportion`$href,
      estimated_brightness = feature$assets$`estimated-brightness`$href,
      year = year
    )
  })

  # Group URLs by year
  years <- unique(sapply(url_list, function(x) x$year))
  print(paste("Found data for years:", paste(years, collapse=", ")))

  # Create temporary directory for downloaded rasters
  temp_dir <- tempdir()
  dir.create(file.path(temp_dir, "rasters"), showWarnings = FALSE, recursive = TRUE)

  download_raster <- function(url, asset_name, year) {
    tryCatch({
      file_ext <- ".tif"
      temp_file <- file.path(temp_dir, "rasters", paste0(asset_name, "_", year, file_ext))

      result <- httr::GET(url,
                          httr::write_disk(temp_file, overwrite = TRUE),
                          httr::progress())

      if (httr::status_code(result) == 200) {
        return(list(
          path = temp_file,
          asset = asset_name,
          year = year
        ))
      } else {
        warning(sprintf("Failed to download %s for year %s: HTTP status code %d",
                        asset_name, year, httr::status_code(result)))
        return(NULL)
      }
    }, error = function(e) {
      warning(sprintf("Failed to download raster for %s, year %s: %s",
                      asset_name, year, e$message))
      return(NULL)
    })
  }

  # Download all rasters
  print("Downloading rasters...")
  downloaded_files <- list()

  for (i in seq_along(url_list)) {
    item <- url_list[[i]]
    year <- item$year

    # Download each asset
    for (asset_name in names(item)[names(item) != "year"]) {
      url <- item[[asset_name]]
      # Download the raster and add to list if successful
      result <- download_raster(url, asset_name, year)
      if (!is.null(result)) {
        downloaded_files <- c(downloaded_files, list(result))
      }
    }
  }

  if (length(downloaded_files) == 0) {
    stop("No rasters could be successfully downloaded")
  }

  print(paste("Successfully downloaded", length(downloaded_files), "raster files"))

  # Group downloaded files by year and asset type
  download_by_year_asset <- list()

  for (file_info in downloaded_files) {
    year <- file_info$year
    asset <- file_info$asset
    key <- paste(year, asset, sep="_")

    if (is.null(download_by_year_asset[[key]])) {
      download_by_year_asset[[key]] <- list()
    }

    download_by_year_asset[[key]] <- c(download_by_year_asset[[key]], file_info$path)
  }

  # Prepare file for shapefile to use in cropping
  if (mosaic_and_crop && !is.null(shp_dt)) {
    # Save the shapefile to a temporary location for use by the Python script
    temp_shp <- file.path(temp_dir, "shape.gpkg")
    sf::st_write(sf_obj, temp_shp, delete_layer = TRUE)
    print(paste("Saved shapefile to temporary location:", temp_shp))
  }

  # Mosaic and crop for each year/asset combination if requested
  raster_objs <- list()

  if (mosaic_and_crop) {
    print("Performing mosaicking and cropping...")

    # Get path to the python script in the package
    python_script <- system.file("python_scripts/mosaic_crop.py", package = "geolink")

    if (!file.exists(python_script)) {
      warning("Python script not found at ", python_script, ". Falling back to direct raster loading.")
      mosaic_and_crop <- FALSE
    } else {
      # Process each year/asset group with the external Python script
      for (key in names(download_by_year_asset)) {
        file_paths <- download_by_year_asset[[key]]
        parts <- strsplit(key, "_")[[1]]
        year <- parts[1]
        asset <- parts[2]

        print(paste("Processing", asset, "for year", year))

        # Output path for the mosaicked and cropped raster
        processed_path <- file.path(temp_dir, paste0("processed_", key, ".tif"))

        # Construct the Python command
        python_cmd <- paste(
          "python",
          shQuote(python_script),
          shQuote(temp_shp),
          shQuote(processed_path),
          paste(sapply(file_paths, shQuote), collapse = " ")
        )

        print(paste("Executing:", python_cmd))

        # Execute the Python script
        result <- system(python_cmd, intern = TRUE)

        # Check if the mosaicking and cropping was successful
        success_line <- grep("^SUCCESS:", result, value = TRUE)

        if (length(success_line) > 0) {
          # Extract the processed file path
          final_path <- sub("^SUCCESS:", "", success_line)
          print(paste("Successfully processed", asset, "for year", year))

          # Load the processed raster
          tryCatch({
            rast_obj <- terra::rast(final_path)
            if (!is.null(rast_obj)) {
              raster_objs[[length(raster_objs) + 1]] <- rast_obj
              # Store name with year and asset for later identification
              names(raster_objs)[length(raster_objs)] <- paste0(asset, "_", year)
            }
          }, error = function(e) {
            warning(sprintf("Failed to load processed raster for %s, year %s: %s",
                            asset, year, e$message))
          })
        } else {
          warning(paste("Failed to process", asset, "for year", year))
          print(result)  # Print the full output for debugging
        }
      }
    }
  }

  # If mosaicking and cropping was disabled or failed, load the individual rasters
  if (!mosaic_and_crop || length(raster_objs) == 0) {
    print("Loading individual rasters...")

    for (file_info in downloaded_files) {
      tryCatch({
        rast_obj <- terra::rast(file_info$path)
        if (!is.null(rast_obj)) {
          raster_objs[[length(raster_objs) + 1]] <- rast_obj
          # Store name with year and asset for later identification
          names(raster_objs)[length(raster_objs)] <- paste0(file_info$asset, "_", file_info$year)
        }
      }, error = function(e) {
        warning(sprintf("Failed to load raster for %s, year %s: %s",
                        file_info$asset, file_info$year, e$message))
      })
    }
  }

  if (length(raster_objs) == 0) {
    stop("No rasters could be successfully loaded")
  }

  # Use names from the raster objects
  name_set <- names(raster_objs)

  print("Electrification Access Raster Downloaded and Processed")
  print(sprintf("Processing %d rasters", length(raster_objs)))

  # Process downloaded rasters
  dt <- postdownload_processor(
    shp_dt = sf_obj,
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


#' Download OpenCellID data
#'
#' @details This function processes downloaded OpenCellID data,
#' which provides information about cell towers and their coverage areas.
#' The return dataframe gives a count of cell towers within the shapefile area.
#' For the function to run, the user will need to set up an account on www.opencellid.com and download the
#' required csv.gz file for the country for which they wish to run the analysis. The file location of this
#' dataset (csv.gz file) should then be passed into the cell_tower_file parameter. Please see example usage below/
#'
#' @param cell_tower_file A csv.gz file path downloaded from OpencellID.
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only)
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey i.e. a household survey with latitude and longitude values.
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only & if use_survey is TRUE)
#' @param survey_lat A character, latitude variable from survey (for STATA users only & if use_survey is TRUE)
#' @param survey_lon A character, longitude variable from survey (for STATA users only & if use survey is TRUE)
#' @param survey_crs A numeric, the default is 4326
#' @param buffer_size A numeric, the size of the buffer for `survey_dt` or `survey_fn` in meters.
#' @param grid_size A numeric, the grid size to be used in meters
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is mean. Other options are "sum", "min", "max", "sd", "skew" and "rms".
#'
#' @return A processed data frame or object based on the input parameters and downloaded data.
#'
#'
#' @examples
#'
#' \dontrun{
#' \donttest{
#'
#' # Example usage
#' opencellid_shp <- geolink_opencellid(cell_tower_file = "C:/username/Downloads/621.csv.gz",
#'                                      shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#'
#'
#' opencellid_shp_fn <- geolink_opencellid(cell_tower_file = "C:/username/Downloads/621.csv.gz",
#'                                         shp_fn = "tests/testthat/testdata/shp_dt.shp")
#'
#'
#' opencellid_survey <- geolink_opencellid(cell_tower_file = "C:/username/Downloads/621.csv.gz",
#'                                         buffer_size = 2000,
#'                                         survey_dt = hhgeo_dt[hhgeo_dt$ADM1_EN == "Abia",])
#'
#'
#' opencellid_survey_fn <- geolink_opencellid(cell_tower_file = "C:/username/Downloads/621.csv.gz",
#'                                            buffer_size = 1000,
#'                                            survey_fn = "tests/testthat/testdata/xy_hhgeo_dt.dta",
#'                                            survey_lon = "x",
#'                                            survey_lat = "y")
#' }}
#'
#' @export
#' @importFrom terra rast
#' @importFrom httr GET timeout
#' @importFrom sf st_as_sf st_transform st_bbox st_intersects st_as_sfc
#' @importFrom data.table as.data.table
#' @importFrom memoise memoise
#' @importFrom haven read_dta

geolink_opencellid <- function(cell_tower_file,
                               shp_dt = NULL,
                               shp_fn = NULL,
                               survey_dt = NULL,
                               survey_fn = NULL,
                               survey_lat = NULL,
                               survey_lon = NULL,
                               buffer_size = NULL,
                               survey_crs = 4326,
                               extract_fun = "mean",
                               grid_size = 1000) {

  resolution = 1000
  name_set = "cell_towers"

  # Define read_opencellid_data function if not already defined
  read_opencellid_data <- function(file_path) {
    # For gzipped CSV files
    if (grepl("\\.gz$", file_path)) {
      message("Reading gzipped file...")
      data <- data.table::fread(file_path)
    } else {
      data <- data.table::fread(file_path)
    }

    # Check if required columns exist
    if (!all(c("lat", "lon") %in% colnames(data))) {
      # Try to identify lat/lon columns if they exist
      if (ncol(data) >= 8) {
        # Assuming columns 7 and 8 are lon and lat as mentioned
        names(data)[7] <- "lon"
        names(data)[8] <- "lat"
        message("Renamed columns 7 and 8 to 'lon' and 'lat'")
      } else {
        stop("Cell tower data must contain 'lat' and 'lon' columns")
      }
    }

    return(data)
  }

  read_opencellid_data_cached <- memoise::memoise(read_opencellid_data)

  if (!is.null(survey_dt)) {
    if (!inherits(survey_dt, "sf")) {
      if ("geometry" %in% names(survey_dt)) {
        sf_obj <- sf::st_as_sf(survey_dt)
      } else if (!is.null(survey_lat) && !is.null(survey_lon)) {
        if (!(survey_lat %in% names(survey_dt)) || !(survey_lon %in% names(survey_dt))) {
          stop(sprintf("Coordinate columns '%s' and '%s' not found in survey data",
                       survey_lat, survey_lon))
        }

        sf_obj <- sf::st_as_sf(survey_dt,
                               coords = c(survey_lon, survey_lat),
                               crs = survey_crs,
                               agr = "constant")
      } else {
        stop("Survey data must either have a geometry column or survey_lat/survey_lon must be provided")
      }
      original_data <- data.table::as.data.table(sf::st_drop_geometry(survey_dt))
    } else {
      sf_obj <- survey_dt
      original_data <- data.table::as.data.table(sf::st_drop_geometry(survey_dt))
    }
  } else if (!is.null(survey_fn)) {
    if (is.null(survey_lat) || is.null(survey_lon)) {
      stop("Both survey_lat and survey_lon column names must be provided when using survey_fn")
    }

    original_data <- read_survey_data(survey_fn)

    if (!(survey_lat %in% names(original_data)) || !(survey_lon %in% names(original_data))) {
      stop(sprintf("Coordinate columns '%s' and '%s' not found in survey file",
                   survey_lat, survey_lon))
    }

    sf_obj <- sf::st_as_sf(original_data,
                           coords = c(survey_lon, survey_lat),
                           crs = survey_crs,
                           agr = "constant")
  } else if (!is.null(shp_dt)) {
    if (!inherits(shp_dt, "sf")) {
      stop("Input shp_dt must be an sf object")
    }
    sf_obj <- shp_dt
    original_data <- data.table::as.data.table(sf::st_drop_geometry(shp_dt))
  } else if (!is.null(shp_fn)) {
    sf_obj <- sf::st_read(shp_fn, quiet = TRUE)
    original_data <- data.table::as.data.table(sf::st_drop_geometry(sf_obj))
  } else {
    stop("Please provide either shapefile data or survey data with coordinate columns")
  }

  # Keep a copy of original geometry without any transformation
  original_sf_obj <- sf_obj

  # Use 3857 for buffering (metric) then convert to 4326 (degrees)
  if (!is.null(buffer_size)) {
    message(sprintf("Creating buffer of %s meters aroundpoints...", buffer_size))
    sf_obj <- sf::st_transform(sf_obj, 3857) %>%
      sf::st_buffer(buffer_size) %>%
      sf::st_transform(4326)
  }

  if (sf::st_crs(sf_obj)$epsg != 4326) {
    sf_obj <- sf::st_transform(sf_obj, 4326)
  }

  message("Reading cell tower data...")
  cell_towers <- read_opencellid_data_cached(cell_tower_file)
  message(sprintf("Read %d cell towers", nrow(cell_towers)))

  cell_towers_sf <- sf::st_as_sf(cell_towers,
                                 coords = c("lon", "lat"),
                                 crs = 4326,
                                 agr = "constant")

  # Create spatial index for efficiency
  cell_towers_sf <- sf::st_sf(cell_towers_sf)

  bbox <- sf::st_bbox(sf_obj)

  message("Filtering cell towers within bounding box...")
  cell_towers_filtered <- cell_towers_sf[sf::st_intersects(
    cell_towers_sf,
    sf::st_as_sfc(bbox),
    sparse = FALSE
  )[,1], ]

  if (nrow(cell_towers_filtered) == 0) {
    warning("No cell towers found in the area enclosed by the bounding box.")
    num_towers <- rep(0, nrow(original_sf_obj))
  } else {
    message(sprintf("Found %d cell towers within bounding box", nrow(cell_towers_filtered)))

    # HERE'S THE KEY CHANGE: We're using sf_obj (buffered) instead of original_sf_obj
    message("Counting cell towers within buffered areas...")
    num_towers <- lengths(sf::st_intersects(sf_obj, cell_towers_filtered, sparse = TRUE))

    message(sprintf("Total towers found in all buffers: %d", sum(num_towers)))
  }

  original_data[[name_set]] <- num_towers

  return(original_data)
}


#' Download and Merge Annual Land Use Land Cover data into geocoded surveys
#'
#' Download Land Use Land Cover data from the LULC dataset at annual intervals for a specified period.
#' The data is downloaded in raster format and combined with shapefile and/or survey data provided
#' by the user. Source data: https://planetarycomputer.microsoft.com/dataset/io-lulc-annual-v02
#'
#' @param start_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param end_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons
#' @param shp_fn Character string. Path to a shapefile if shp_dt is not provided directly
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey
#' @param survey_fn Character string. Path to survey data file (.dta or .csv)
#' @param survey_lat Character string. Column name for latitude in survey data
#' @param survey_lon Character string. Column name for longitude in survey data
#' @param buffer_size Numeric. Buffer size (in meters) to apply around survey points
#' @param survey_crs Numeric. CRS code for survey coordinates (default: 4326)
#' @param grid_size Numeric. Size of grid cells for tessellation (in meters)
#' @param use_resampling Logical. Whether to resample rasters to a common resolution (default: TRUE)
#' @param target_resolution Numeric. Target resolution for resampling in meters (default: 1000)
#'
#' @return An sf object with land cover classifications by year
#'
#' @examples
#' \dontrun{
#' \donttest{
#' # Loading the survey data and shapefile
#' data("hhgeo_dt")
#' data("shp_dt")
#'
#' # Pull annual land use land cover for Abia region
#'
#'  landcover_shp <- geolink_landcover(
#'  start_date = "2019-01-01",
#'  end_date = "2019-12-31",
#'  shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'  use_resampling = TRUE,
#'  target_resolution = 1000)
#'
#'
#'
#'  landcover_survey <- geolink_landcover(
#'  start_date = "2020-01-01",
#'  end_date = "2020-12-31",
#'  survey_dt = hhgeo_dt[hhgeo_dt$ADM1_EN == "Abia",],
#'  buffer_size = 1000,
#'  use_resampling = TRUE,
#'  target_resolution = 1000)
#'
#'
#' }
#'}
#' @export
#' @import sf rstac terra
#' @importFrom haven read_dta
#' @importFrom httr GET write_disk config timeout status_code
#' @importFrom exactextractr exact_extract
#' @importFrom reticulate use_condaenv use_python py_run_string source_python
#'
#'


geolink_landcover <- function(start_date,
                              end_date,
                              shp_dt = NULL,
                              shp_fn = NULL,
                              survey_dt = NULL,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              survey_crs = 4326,
                              grid_size = NULL,
                              use_resampling = TRUE,
                              target_resolution = 1000) {

  # Convert dates
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # SECTION 1: PROCESS INPUT DATA ---------------------------------------------

  # Handle survey data if provided
  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
  } else if (!is.null(survey_fn)) {
    if (is.null(survey_lat) || is.null(survey_lon)) {
      stop("Both survey_lat and survey_lon must be provided when using survey_fn")
    }

    # Read survey file
    survey_dt <- try({
      if (grepl("\\.dta$", survey_fn)) {
        haven::read_dta(survey_fn)
      } else if (grepl("\\.csv$", survey_fn)) {
        read.csv(survey_fn)
      } else {
        stop("Unsupported file format. Please provide .dta or .csv file")
      }
    }, silent = TRUE)

    if (inherits(survey_dt, "try-error")) {
      stop("Error reading survey file")
    }

    # Convert to sf object
    survey_dt <- st_as_sf(survey_dt,
                          coords = c(survey_lon, survey_lat),
                          crs = survey_crs)

    # Transform to EPSG:4326 if needed
    if (st_crs(survey_dt)$epsg != 4326) {
      survey_dt <- st_transform(survey_dt, 4326)
    }
  }

  # Create spatial object from inputs
  if (!is.null(shp_dt)) {
    sf_obj <- zonalstats_prepshp(shp_dt = shp_dt, grid_size = grid_size) %>%
      ensure_crs_4326()
  } else if (!is.null(shp_fn)) {
    sf_obj <- zonalstats_prepshp(shp_fn = shp_fn, grid_size = grid_size) %>%
      ensure_crs_4326()
  } else if (!is.null(survey_dt) || !is.null(survey_fn)) {
    sf_obj <- zonalstats_prepsurvey(
      survey_dt = survey_dt,
      survey_fn = survey_fn,
      survey_lat = survey_lat,
      survey_lon = survey_lon,
      buffer_size = buffer_size,
      survey_crs = survey_crs) %>%
      ensure_crs_4326()
  } else {
    stop("Please provide either a shapefile (shp_dt/shp_fn) or survey data (survey_dt/survey_fn)")
  }

  # Fix invalid geometries
  sf_obj <- sf::st_make_valid(sf_obj)

  # SECTION 2: CONFIGURE PYTHON ENVIRONMENT ----------------------------------

  # Clear existing Python config
  Sys.unsetenv("RETICULATE_PYTHON")

  # Set up Python environment
  geo_env_name <- get("pkg_env", envir = asNamespace("GeoLink"))$conda_env_name

  # Try to use conda environment
  env_setup_success <- try({
    reticulate::use_condaenv(geo_env_name, required = TRUE)
    TRUE
  }, silent = TRUE)

  # If conda fails, try direct Python path
  if (inherits(env_setup_success, "try-error")) {
    python_path <- get("pkg_env", envir = asNamespace("GeoLink"))$python_path
    if (!is.null(python_path) && file.exists(python_path)) {
      reticulate::use_python(python_path, required = TRUE)
    } else {
      stop("Failed to configure Python environment")
    }
  }

  # Configure SSL certificates
  try({
    reticulate::py_run_string("
    import certifi
    import os
    os.environ['SSL_CERT_FILE'] = certifi.where()
    ")
  }, silent = TRUE)

  # Load Python utilities
  python_utils_path <- system.file("python_scripts", "raster_utils.py", package = "GeoLink")
  if (!file.exists(python_utils_path)) {
    stop("Python utilities not found. Check package installation.")
  }
  reticulate::source_python(python_utils_path)

  # SECTION 3: STAC SEARCH FOR LANDCOVER DATA --------------------------------

  # Define feature filter function
  filter_features <- function(feature, start_date, end_date) {
    # Get feature date and check if it's in the time range
    feature_date <- as.Date(feature$properties$start_datetime)
    feature_year <- format(feature_date, "%Y")
    start_year <- format(as.Date(start_date), "%Y")
    end_year <- format(as.Date(end_date), "%Y")

    return(feature_year >= start_year && feature_year <= end_year)
  }

  # Perform STAC search
  stac_result <- try({
    # Connect to Planetary Computer STAC API
    s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

    # Get bounding box with buffer
    bbox <- sf::st_bbox(sf_obj)
    buffered_bbox <- c(
      bbox["xmin"] - 0.1,
      bbox["ymin"] - 0.1,
      bbox["xmax"] + 0.1,
      bbox["ymax"] + 0.1
    )

    # Search for land cover data
    it_obj <- s_obj %>%
      stac_search(
        collections = "io-lulc-annual-v02",
        bbox = buffered_bbox,
        datetime = paste(start_date, end_date, sep = "/")
      ) %>%
      get_request() %>%
      items_sign(sign_fn = sign_planetary_computer())

    # Filter features by date
    it_obj$features <- it_obj$features[sapply(it_obj$features, function(feature) {
      filter_features(feature, start_date, end_date)
    })]

    it_obj
  }, silent = TRUE)

  # Handle failed STAC search
  if (inherits(stac_result, "try-error") || length(stac_result$features) == 0) {
    warning("No features found. Returning empty dataset.")
    return(create_empty_result(sf_obj, start_date))
  }

  # SECTION 4: DOWNLOAD AND PROCESS RASTERS ---------------------------------

  temp_dir <- tempdir()
  raster_year_map <- list()

  # Define land cover classes
  land_cover_classes <- list(
    list(values = 0, summary = "No Data"),
    list(values = 1, summary = "Water"),
    list(values = 2, summary = "Trees"),
    list(values = 4, summary = "Flooded vegetation"),
    list(values = 5, summary = "Crops"),
    list(values = 7, summary = "Built area"),
    list(values = 8, summary = "Bare ground"),
    list(values = 9, summary = "Snow/ice"),
    list(values = 10, summary = "Clouds"),
    list(values = 11, summary = "Rangeland")
  )


  # Download rasters
  for (i in seq_along(stac_result$features)) {
    feature <- stac_result$features[[i]]

    if (is.null(feature$assets$data$href)) {
      print(paste("Feature", i, "has no data URL"))
      next
    }

    year <- format(as.Date(feature$properties$start_datetime), "%Y")
    url <- feature$assets$data$href
    raster_path <- file.path(temp_dir, paste0(year, "_", i, "_raster.tif"))

    print(paste("Downloading raster for year", year))

    # Try to download the file
    download_success <- try({
      response <- httr::GET(
        url,
        httr::write_disk(raster_path, overwrite = TRUE),
        httr::config(ssl_verifypeer = FALSE),
        httr::timeout(300)
      )

      httr::status_code(response) == 200
    }, silent = TRUE)

    if (!inherits(download_success, "try-error") && download_success) {
      file_size <- file.info(raster_path)$size

      if (file_size > 0) {
        if (!year %in% names(raster_year_map)) {
          raster_year_map[[year]] <- list()
        }
        raster_year_map[[year]] <- append(raster_year_map[[year]], raster_path)
      }
    }
  }

  # Check if any rasters were downloaded
  if (length(raster_year_map) == 0) {
    warning("No raster data could be downloaded. Returning empty dataset.")
    return(create_empty_result(sf_obj, start_date))
  }

  # SECTION 5: PROCESS RASTERS AND EXTRACT DATA -----------------------------

  # Process each year's rasters
  results_list <- list()

  for (year in names(raster_year_map)) {
    message("Processing rasters for year:", year)

    raster_paths <- as.character(raster_year_map[[year]])

    # Check file existence
    if (!all(file.exists(raster_paths))) {
      warning(paste("Some raster files for year", year, "do not exist. Skipping."))
      next
    }

    # Apply resampling if requested
    if (use_resampling) {
      processed_paths <- try({
        resample_rasters(
          input_files = raster_paths,
          output_folder = file.path(temp_dir, "resampled", year),
          target_resolution = target_resolution
        )
      }, silent = TRUE)

      if (!inherits(processed_paths, "try-error") && length(processed_paths) > 0) {
        raster_paths <- processed_paths
      }
    }

    # Mosaic rasters if needed
    if (length(raster_paths) > 1) {
      mosaic_path <- try({
        mosaic_rasters(input_files = raster_paths)
      }, silent = TRUE)

      if (!inherits(mosaic_path, "try-error")) {
        raster_path <- mosaic_path
      } else {
        raster_path <- raster_paths[1]
      }
    } else {
      raster_path <- raster_paths[1]
    }

    # Load the raster
    raster <- try({
      terra::rast(raster_path)
    }, silent = TRUE)

    if (inherits(raster, "try-error")) {
      warning(paste("Failed to load raster for year", year))
      next
    }

    # Ensure CRS is set correctly
    if (is.na(terra::crs(raster))) {
      terra::crs(raster) <- "EPSG:4326"
    }

    # Extract class values and names
    class_values <- unlist(lapply(land_cover_classes, function(x) x$values))
    class_names <- tolower(gsub(" ", "_", unlist(lapply(land_cover_classes, function(x) x$summary))))
    all_column_names <- c(class_names, "no_data")

    # Extract land cover proportions
    message(paste("Extracting land cover proportions for year:", year))

    # Create results dataframe
    year_results <- sf::st_drop_geometry(sf_obj)

    # Initialize all land cover columns to 0
    for (col in all_column_names) {
      year_results[[col]] <- 0
    }

    # Extract values using exactextractr
    extracted_values <- try({
      exactextractr::exact_extract(raster, sf::st_make_valid(sf_obj),
                                   coverage_area = TRUE)
    }, silent = TRUE)

    if (!inherits(extracted_values, "try-error")) {
      # Process extracted values
      for (i in seq_along(extracted_values)) {
        ev <- extracted_values[[i]]

        if (is.null(ev) || nrow(ev) == 0) {
          next
        }

        # Calculate total area
        total_area <- sum(ev$coverage_area, na.rm = TRUE)

        if (total_area <= 0) {
          next
        }

        # Calculate proportion for each class
        for (class_idx in seq_along(class_values)) {
          class_val <- class_values[class_idx]
          class_name <- class_names[class_idx]

          class_rows <- ev$value == class_val

          if (any(class_rows, na.rm = TRUE)) {
            class_area <- sum(ev$coverage_area[class_rows], na.rm = TRUE)
            year_results[i, class_name] <- round((class_area / total_area) * 100, 2)
          }
        }

        # Calculate no_data percentage
        na_rows <- is.na(ev$value)
        if (any(na_rows)) {
          na_area <- sum(ev$coverage_area[na_rows], na.rm = TRUE)
          year_results[i, "no_data"] <- round((na_area / total_area) * 100, 2)
        }
      }
    }

    year_results$year <- year
    results_list[[year]] <- year_results
  }

  # SECTION 6: COMBINE RESULTS AND RETURN ------------------------------------

  # Combine all years of results
  if (length(results_list) == 0) {
    warning("No results generated. Returning empty dataset.")
    return(create_empty_result(sf_obj, start_date))
  }

  # Combine results and add geometries
  result_df <- do.call(rbind, results_list)
  final_result <- sf::st_sf(result_df, geometry = sf::st_geometry(sf_obj)[rep(1:nrow(sf_obj), length(results_list))])

  return(final_result)
}

# Helper function to create empty result
create_empty_result <- function(sf_obj, start_date) {
  empty_result <- sf::st_drop_geometry(sf_obj)

  land_cover_classes <- c("No Data", "Water", "Trees", "Flooded vegetation", "Crops",
                          "Built area", "Bare ground", "Snow/ice", "Clouds", "Rangeland")


  for (col in land_cover_classes) {
    empty_result[[col]] <- NA
  }

  empty_result$year <- format(start_date, "%Y")
  return(sf::st_sf(empty_result, geometry = sf::st_geometry(sf_obj)))
}
