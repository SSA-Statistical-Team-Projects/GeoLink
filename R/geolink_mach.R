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
#' \donttest{
#'
#'
#' #quick example
#' df <- geolink_chirps(time_unit = "month",
#'                      start_date = "2020-01-01",
#'                      end_date = "2020-03-01",
#'                      shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
#'                      grid_size = 1000,
#'                      survey_dt = hhgeo_dt,
#'                      extract_fun = "mean")
#'
#' }
#'
#' @export
#' @import data.table parallel raster
#' @importFrom haven read_dta
#' @importFrom crsuggest suggest_crs



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

  shp_dt <- ensure_crs_4326(shp_dt)
  survey_dt <- ensure_crs_4326(survey_dt)


  # start_date <- as.Date(start_date)
  # end_date <- as.Date(end_date)

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
#' @param ego_username username for your eogdata account
#' @param ego_password password for your egodata account
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
#' }
#
#' @export


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

  shp_dt <- ensure_crs_4326(shp_dt)
  survey_dt <- ensure_crs_4326(survey_dt)

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
#''@import rstac reticulate terra raster osmdata sf  geodata rvest httr
#'
#' @examples
#'\donttest{
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
#'                            file_location = "/Users/nikos/Documents/temp/nga_pop")
#'
#'}
#'@export

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

      file_urls <- try_download(url1)
      if (is.null(file_urls)) {
        file_urls <- try_download(url2)
      }

      file_urls <- file_urls[grepl(iso_code, file_urls, ignore.case = TRUE) &
                               grepl("ppp", file_urls, ignore.case = TRUE) &
                               grepl(year_pattern, file_urls)]
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
        if (!is.null(start_year) && !is.null(end_year)) {
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
  unlink(tempdir(), recursive = TRUE)
}



#' Download high resolution elevation data based on shapefile coordinates
#'
#' This function downloads high-resolution elevation data based on the coordinates provided by either a shapefile or a file path to a shapefile. It can also incorporate survey data for further analysis. The elevation data is downloaded using the `elevation_3s` function and post-processed using the `postdownload_processor` function.
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
#' @import rstac terra raster osmdata sf httr geodata
#' @examples
#' \donttest{
#'
#'  #example usage with shapefile
#'test_dt <- geolink_elevation(iso_code = "NGA",
#'                             shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'                             grid_size = 1000,
#'                             extract_fun = "mean")
#' }
#'@export

geolink_elevation <- function(iso_code,
                              shp_dt=NULL,
                              shp_fn = NULL,
                              grid_size = NULL,
                              survey_dt = NULL,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              extract_fun = "mean",
                              survey_crs = 4326){

  shp_dt <- ensure_crs_4326(shp_dt)
  survey_dt <- ensure_crs_4326(survey_dt)

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

  epsg_4326 <- "+init=EPSG:4326"

  for (i in seq_along(raster_list)) {
    terra::crs(raster_list[[i]]) <- epsg_4326
    if (is.null(terra::crs(raster_list[[i]]))) {
      print(paste("Projection failed for raster", st_crs(raster_list[[i]])$input))
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

  return(dt)
  unlink(tempdir(), recursive = TRUE)
}


#' Download high resolution building data from WorldPop
#'
#' This function downloads high-resolution building data from WorldPop based on the specified version and ISO country code. It can incorporate survey data for further analysis. The building data is downloaded as raster files and can be processed and analyzed using the `postdownload_processor` function.
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
#'
#' @return A processed data frame or object based on the input parameters and downloaded data.
#'
#' @importFrom httr GET http_type write_disk
#' @import rstac terra raster osmdata  sf httr geodata
#'
#' @examples
#' \donttest{
#'
#' #example usage with version 1.1
#'test_dt <- geolink_buildings(version = "v1.1",
#'                             iso_code = "NGA",
#'                             shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
#'                             indicators = "ALL",
#'                             grid_size = 1000)
#'
#' }
#' @export
#'


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

  return(dt)
  unlink(tempdir(), recursive = TRUE)
}

#' Download CMIP6 climate model data
#'
#' This function downloads and processes CMIP6 (Coupled Model Intercomparison Project Phase 6)
#' climate model data for a specific scenario and desired model.
#' It allows for further analysis of the data in conjunction with geographic data.
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
#' @importFrom httr GET http_type write_disk
#' @import rstac terra raster osmdata sf httr geodata progress
#'
#' @examples
#' \donttest{
#'
#'  #example usage
#' df <- geolink_CMIP6(start_date = "2019-01-01",
#'                     end_date = "2019-12-31",
#'                     scenario = "ssp245",
#'                    desired_models = "UKESM1-0-LL",
#'                    shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'                    grid_size = 1000)
#'
#'
#' }
#'@export

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

  # Ensure shapefile and survey are in the correct CRS
  if (!is.null(shp_dt)) {
    sf_obj <- ensure_crs_4326(shp_dt)

  } else if (!is.null(survey_dt)) {
    sf_obj <- ensure_crs_4326(survey_dt)

  } else if (!is.null(shp_fn)) {
    sf_obj <- sf::read_sf(shp_fn)
    sf_obj <- ensure_crs_4326(sf_obj)

  } else if (!is.null(survey_fn)) { # Changed condition to `survey_fn`
    sf_obj <- zonalstats_prepsurvey(
      survey_dt = survey_dt,
      survey_fn = survey_fn,
      survey_lat = survey_lat,
      survey_lon = survey_lon,
      buffer_size = NULL,
      survey_crs = survey_crs)
    sf_obj <- ensure_crs_4326(sf_obj)

  } else {
    print("Input a valid sf object or geosurvey")
    sf_obj <- NULL  # Optional: Define a default value to avoid potential errors
  }



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
#' This function downloads cropland data from a specified source, such as WorldCover. It allows for further analysis of cropland distribution in a given area.
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
#' @importFrom terra rast
#' @importFrom httr GET http_type write_disk
#' @import rstac terra raster osmdata  sf httr geodata
#'
#' @examples
#' \donttest{
#'
#'  #example usage
#'df <- geolink_cropland(shp_dt = shp_dt[shp_dt$ADM1_EN
#'                                 ==  "Abia",],
#'                 grid_size = 1000,
#'                 extract_fun = "mean")
#' }
#'@export


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
#' This function downloads WorldClim climate data for a specific variable and resolution. It allows for further analysis of climate patterns in a given area.
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
#' @importFrom terra rast
#' @importFrom httr GET http_type write_disk
#' @import rstac terra  sf httr geodata
#'
#' @examples
#' \donttest{
#'
#'  #example usage
#'df <- geolink_worldclim(iso_code ="NGA",
#'                  var='tmax',
#'                  res=2.5,
#'                  shp_dt = shp_dt[shp_dt$ADM1_EN
#'                                  == "Abia",],
#'                  grid_size = 1000,
#'                  extract_fun = "mean")
#' }
#'@export
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

  shp_dt <- ensure_crs_4326(shp_dt)
  survey_dt <- ensure_crs_4326(survey_dt)


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
  print(raster_list)

  months <- month.abb
  print(months)


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
#' This function downloads Terraclimate data for a specific variable and year. It allows for further analysis of climate patterns in a given area.
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
#' @importFrom terra rast
#' @importFrom httr GET http_type write_disk
#' @import rstac reticulate terra raster osmdata sf geodata httr ncdf4
#'
#' @examples
#' \donttest{
#'
#'  #example usage
#'df <- geolink_terraclimate( var='tmax',
#'                                year = 2017,
#'                                 shp_dt = shp_dt[shp_dt$ADM1_EN
#'                                                 == "Abia",],
#'                                 grid_size = 1000,
#'                                 extract_fun = "mean")
#' }
#'@export

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

  shp_dt <- ensure_crs_4326(shp_dt)
  survey_dt <- ensure_crs_4326(survey_dt)

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
  } else if (http_status(response)$category == "Success") {
    # Write the content to a file if the status code is 200
    tryCatch({
      writeBin(content(response, "raw"), destination)
      print("File downloaded successfully.")
      print(paste("File saved to:", destination))
    }, error = function(e) {
      print(paste("Error writing the file:", e$message))
    })


   # raster_stack <- stack(destination)
    rasters_combined <- terra::rast(destination)

    # Check CRS and set it if necessary
    if (is.na(crs(rasters_combined))) {
      crs(rasters_combined) <- crs("+proj=longlat +datum=WGS84 +no_defs")
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

    return(dt)
    unlink(tempdir(), recursive = TRUE)
  } else {
    # Print the error status
    print(paste("Error downloading the file. Status code:", http_status(response)$status_code))
    return(NULL)
  }
  unlink(tempdir(), recursive = TRUE)
}

#' Download and Merge Annual Land Use Land Cover data into geocoded surveys
#'
#' Download Land Use Land Cover data from the LULC dataset at annual intervals for a specified period
#' The data is downloaded in raster format and combined with shapefile and/or survey data provided
#' by the user. Source data: https://planetarycomputer.microsoft.com/dataset/io-lulc-annual-v02
#'
#' @param start_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param end_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey i.e.
#' @param use_resampling An option to resample data to 1km squared if the resolution of the data is higher than this,
#' this will speed up the processing of the code
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only).
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only & if use_survey is TRUE) (optional).
#' @param grid_size A numeric, the grid size to be used as a buffer around survey points.
#' @param survey_lat A character, latitude variable from survey (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lon A character, longitude variable from survey (for STATA users only & if use survey is TRUE) (optional).
#' @param survey_crs An integer, the Coordinate Reference System (CRS) for the survey data. Default is 4326 (WGS84) (optional).
#' @param buffer_size A numeric, the buffer size to be used around each point in the survey data, in meters (optional).
#'
#'
#' @examples
#'
#' \donttest{
#'
#' #loading the survey data and shapefile
#'
#' data("hhgeo_dt")
#' data("shp_dt")
#'
#' #pull annual land use land cover and combine with household survey based on
#' #grid tesselation of shapefile at 1000m
#'
#'df <- geolink_landcover(
#                         start_date = "2020-01-01",
#                         end_date = "2021-01-01",
#                         shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#'
#'
#'
#' surveyfn_df <- geolink_landcover(start_date = "2020-01-01",
#'                              end_date = "2020-12-31",
#'                              survey_fn = "tests/testthat/testdata/xy_hhgeo_dt.dta",
#'                              survey_lon = "x",
#'                              survey_lat = "y",
#'                              buffer_size = 1000,
#'                              use_resampling = TRUE)
#'
#'
#'
#' df_survey <- geolink_landcover(
#'                                start_date = "2020-01-01",
#'                                end_date = "2020-12-31",
#'                                survey_dt = hhgeo_dt[hhgeo_dt$ADM1_EN == "Abia",],
#'                                buffer_size = 1000,
#'                                use_resampling = TRUE)
#' }
#'
#' @import  rstac reticulate terra raster osmdata sp sf geodata httr ncdf4 rgdal exactextractr parallel
#'
#'
#'
#'


geolink_landcover <- function(start_date = NULL,
                              end_date = NULL,
                              shp_dt = NULL,
                              shp_fn = NULL,
                              survey_dt = NULL,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              survey_crs = 4326,
                              grid_size = NULL,
                              use_resampling = FALSE) {

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

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


  # Clear existing Python config
  Sys.unsetenv("RETICULATE_PYTHON")

  # Set up virtual environment
  venv_path <- file.path(system.file(package = "GeoLink"), "python", "virtual_env")
  reticulate::use_virtualenv(venv_path, required = TRUE)

  # Source Python utilities
  python_utils_path <- system.file("python_scripts", "raster_utils.py", package = "GeoLink")
  if (!file.exists(python_utils_path)) {
    stop("Python utilities not found. Check package installation.")
  }
  reticulate::source_python(python_utils_path)

  # Keep one filter_features function that checks both conditions
  filter_features <- function(feature, start_date, end_date) {
    # Check if it's a boundary tile
    bbox <- feature$bbox
    is_valid_bbox <- !any(bbox %in% c(180, -180))

    # Check if dates represent a full calendar year
    start_year <- format(as.Date(start_date), "%Y")
    end_year <- format(as.Date(end_date), "%Y")
    start_md <- format(as.Date(start_date), "%m-%d")
    end_md <- format(as.Date(end_date), "%m-%d")

    is_full_year <- start_year == end_year &&
      start_md == "01-01" &&
      end_md == "12-31"

    if (is_full_year) {
      # For full calendar year, create the pattern dynamically
      year <- start_year
      next_year <- as.character(as.numeric(year) + 1)
      pattern <- paste0(year, "0101-", next_year, "0101")

      # Check URL pattern
      url <- feature$assets$data$href
      is_correct_year <- grepl(pattern, url, fixed = TRUE)
    } else {
      # For other date ranges, check the feature date
      feature_date <- as.Date(feature$properties$start_datetime)
      is_correct_year <- feature_date >= start_date && feature_date <= end_date
    }

    # Return TRUE only if both conditions are met
    return(is_valid_bbox && is_correct_year)
  }

  # STAC search
  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  it_obj <- s_obj %>%
    stac_search(
      collections = "io-lulc-annual-v02",
      bbox = sf::st_bbox(sf_obj),
      datetime = paste(start_date, end_date, sep = "/")
    ) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  # Apply filtering once with both conditions
  it_obj$features <- it_obj$features[sapply(it_obj$features, function(feature) {
    filter_features(feature, start_date, end_date)
  })]

  # Download and process rasters
  temp_dir <- tempdir()
  raster_year_map <- list()

  for (i in seq_along(it_obj$features)) {
    if (is.null(it_obj$features[[i]]$assets$data$href)) next

    year <- format(as.Date(it_obj$features[[i]]$properties$start_datetime), "%Y")
    url <- it_obj$features[[i]]$assets$data$href
    raster_path <- file.path(temp_dir, paste0(year, "_", i, "_raster.tif"))

    tryCatch({
      download.file(url, raster_path, mode = "wb")
      cat("Downloaded raster to:", raster_path, "\n")

      if (!year %in% names(raster_year_map)) {
        raster_year_map[[year]] <- list()
      }
      raster_year_map[[year]] <- append(raster_year_map[[year]], raster_path)
    }, error = function(e) {
      warning(paste("Could not download raster for year", year, ":", e$message))
    })
  }

  # Process rasters
  mosaicked_rasters <- list()

  for (year in names(raster_year_map)) {
    cat("Processing rasters for year:", year, "\n")

    raster_paths <- raster_year_map[[year]]
    raster_paths <- as.character(raster_paths)
    raster_paths <- normalizePath(raster_paths)

    if (!all(file.exists(raster_paths))) {
      stop("Some raster files do not exist")
    }


    if (use_resampling) {
      resampled_rasters <- resample_rasters(
        input_files = raster_paths,
        output_folder = file.path(temp_dir, "resampled", year),
        target_resolution = 1000
      )

      mosaicked_path <- if (length(resampled_rasters) == 1) {
        resampled_rasters[[1]]
      } else {
        mosaic_rasters(input_files = resampled_rasters)
      }
    } else {
      # Skip resampling and directly mosaic if multiple rasters exist
      mosaicked_path <- if (length(raster_paths) == 1) {
        raster_paths[1]
      } else {
        mosaic_rasters(input_files = raster_paths)
      }
    }

    mosaicked_raster <- terra::rast(mosaicked_path)

    if (is.na(terra::crs(mosaicked_raster))) {
      terra::crs(mosaicked_raster) <- "EPSG:4326"
    } else if (terra::crs(mosaicked_raster, describe = TRUE)$code != "4326") {
      mosaicked_raster <- terra::project(mosaicked_raster, "EPSG:4326")
    }

    mosaicked_rasters[[year]] <- mosaicked_raster
  }

  # Crop rasters to extent
  cropped_rasters <- lapply(mosaicked_rasters, function(raster_obj) {
    terra::crop(raster_obj, terra::ext(sf_obj))
  })
  names(cropped_rasters) <- names(mosaicked_rasters)

  # Extract land cover proportions
  file_values <- it_obj$features[[1]]$assets$data$`file:values`
  class_values <- unlist(lapply(file_values, `[[`, "values"))
  class_names <- tolower(gsub(" ", "_", unlist(lapply(file_values, `[[`, "summary"))))
  all_column_names <- c(class_names, "no_data")

  results_list <- list()

  for (i in seq_along(cropped_rasters)) {
    year <- names(cropped_rasters)[i]
    raster <- cropped_rasters[[i]]

    # Extract values using exactextractr
    extracted_values <- exactextractr::exact_extract(raster, sf_obj, coverage_area = TRUE)

    # Calculate proportions
    polygon_proportions <- lapply(seq_along(extracted_values), function(j) {
      ev <- extracted_values[[j]]
      total_area <- sum(ev$coverage_area, na.rm = TRUE)

      if (total_area == 0) {
        proportions <- setNames(rep(NA, length(all_column_names)), all_column_names)
      } else {
        proportions <- setNames(sapply(class_values, function(class_val) {
          class_area <- sum(ev$coverage_area[ev$value == class_val], na.rm = TRUE)
          round((class_area / total_area) * 100, 2)
        }), class_names)

        remainder <- 100 - sum(proportions, na.rm = TRUE)
        proportions["no_data"] <- round(max(0, remainder), 2)
      }

      return(proportions)
    })

    proportions_matrix <- do.call(rbind, polygon_proportions)

    # Create results dataframe
    year_results <- sf::st_drop_geometry(sf_obj)

    for (col in all_column_names) {
      year_results[[col]] <- proportions_matrix[, col]
    }

    year_results$year <- year
    results_list[[year]] <- year_results
  }

  # Combine results and reattach geometry
  final_result <- do.call(rbind, results_list)
  final_result <- sf::st_sf(final_result, geometry = sf::st_geometry(sf_obj))

  return(final_result)
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
#' @details
#'
#' Details for feature category and sub-category can be found here: https://wiki.openstreetmap.org/wiki/Map_features
#'
#' @import rstac terra raster osmdata sp sf httr geodata
#'
#' @examples
#'\donttest{
#'
#'
#' poi_survey_df <- geolink_get_poi(osm_key = "amenity",
#'                                 buffer_size = 2000,
#'                                 survey_dt = hhgeo_dt[hhgeo_dt$ADM1_EN == "Abia",],)
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
#'
#'}
#'

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
                            grid_size = NULL,
                            max_retries = 3,
                            timeout = 300,
                            area_threshold = 1) {

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
#' @details
#'
#' Details for the dataset can be found here: https://hrea.isr.umich.edu/
#'
#' @import rstac terra raster osmdata sp sf httr geodata
#'
#' @examples
#'\donttest{
#'
#'
#' df = geolink_electaccess(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'   start_date = "2018-12-31", end_date = "2019-12-31")
#'
#' }
#'


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

  # Process input data first to get sf_obj
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
  }

  # Create sf_obj based on input type
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

  # Now use sf_obj for STAC search
  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1", force_version = "1.0.0")
  it_obj <- s_obj %>%
    stac_search(
      collections = "hrea",
      bbox = sf::st_bbox(sf_obj),
      datetime = paste(start_date, end_date, sep = "/")
    ) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  if (length(it_obj$features) == 0) {
    stop("No data found for the specified date range and location")
  }

  # Modify url_list extraction with error checking
  url_list <- lapply(1:length(it_obj$features), function(x) {
    feature <- it_obj$features[[x]]
    required_assets <- c("lightscore", "light-composite", "night-proportion", "estimated-brightness")

    # Check if all required assets exist
    missing_assets <- required_assets[!required_assets %in% names(feature$assets)]
    if (length(missing_assets) > 0) {
      warning(sprintf("Missing assets for feature %d: %s", x, paste(missing_assets, collapse = ", ")))
      return(NULL)
    }

    urls <- list(
      lightscore = paste0("/vsicurl/", feature$assets$lightscore$href),
      light_composite = paste0("/vsicurl/", feature$assets$`light-composite`$href),
      night_proportion = paste0("/vsicurl/", feature$assets$`night-proportion`$href),
      estimated_brightness = paste0("/vsicurl/", feature$assets$`estimated-brightness`$href)
    )
    return(urls)
  })

  # Remove NULL entries from url_list
  url_list <- Filter(Negate(is.null), url_list)

  if (length(url_list) == 0) {
    stop("No valid data URLs found")
  }

  # Convert URLs to rasters with progress tracking
  raster_objs <- list()
  for (i in seq_along(url_list)) {
    for (asset_name in names(url_list[[i]])) {
      url <- url_list[[i]][[asset_name]]
      tryCatch({
        rast_obj <- terra::rast(url)
        if (!is.null(rast_obj)) {
          raster_objs[[length(raster_objs) + 1]] <- rast_obj
        }
      }, error = function(e) {
        warning(sprintf("Failed to load raster for %s: %s", asset_name, e$message))
      })
    }
  }

  if (length(raster_objs) == 0) {
    stop("No rasters could be successfully loaded")
  }

  # Create name_set based on actual successful raster downloads
  name_set <- character(length(raster_objs))

  # Generate names based on actual raster count
  indicators <- c("lightscore", "light_composite", "night_proportion", "estimated_brightness")
  year_sequence <- seq(lubridate::year(start_date), lubridate::year(end_date))

  for (i in seq_along(raster_objs)) {
    indicator_idx <- ((i - 1) %% length(indicators)) + 1
    year_idx <- ((i - 1) %/% length(indicators)) + 1
    if (year_idx <= length(year_sequence)) {
      name_set[i] <- paste0(indicators[indicator_idx], "_", year_sequence[year_idx])
    }
  }

  print("Electrification Access Raster Downloaded")
  print(sprintf("Processing %d rasters with %d names", length(raster_objs), length(name_set)))

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
#' This function processes downloaded OpenCellID data, which provides information about cell towers and their coverage areas.
#' The return dataframe gives data of the nearest cell tower to the shapefile polygon centroid.
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
#'
#' @return A processed data frame or object based on the input parameters and downloaded data.
#'
#' @importFrom terra rast
#' @importFrom httr GET timeout
#' @import rstac terra raster osmdata sf httr geodata data.table geosphere memorise haven
#'
#' @examples
#' \donttest{
#'
#' # Example usage
#' results <- geolink_opencellid(cell_tower_file = "C:/Users/username/Downloads/621.csv.gz",
#'                              shp_dt = shp_dt)
#' }
#'


geolink_opencellid <- function(cell_tower_file,
                               shp_dt = NULL,
                               shp_fn = NULL,
                               survey_dt = NULL,
                               survey_fn = NULL,
                               survey_lat = NULL,
                               survey_lon = NULL,
                               buffer_size = NULL,
                               survey_crs = 4326) {

  # Create memoised version of the function
  read_opencellid_data_cached <- memoise(read_opencellid_data)

  # Process input geometry
  if (!is.null(survey_dt)) {
    log_debug("Processing survey_dt")
    if (!inherits(survey_dt, "sf")) {
      # Check if there's a geometry column
      if ("geometry" %in% names(survey_dt)) {
        # Convert to sf object using existing geometry column
        sf_obj <- st_as_sf(survey_dt)
      } else if (!is.null(survey_lat) && !is.null(survey_lon)) {
        # Convert using coordinate columns if provided
        if (!(survey_lat %in% names(survey_dt)) || !(survey_lon %in% names(survey_dt))) {
          stop(sprintf("Coordinate columns '%s' and '%s' not found in survey data",
                       survey_lat, survey_lon))
        }

        # Convert to sf object efficiently
        sf_obj <- st_as_sf(survey_dt,
                           coords = c(survey_lon, survey_lat),
                           crs = survey_crs,
                           agr = "constant")
      } else {
        stop("Survey data must either have a geometry column or survey_lat/survey_lon must be provided")
      }
      original_data <- as.data.table(st_drop_geometry(survey_dt))
    } else {
      sf_obj <- survey_dt
      original_data <- as.data.table(st_drop_geometry(survey_dt))
    }
  } else if (!is.null(survey_fn)) {
    if (is.null(survey_lat) || is.null(survey_lon)) {
      stop("Both survey_lat and survey_lon column names must be provided when using survey_fn")
    }

    # Read survey file using appropriate method
    original_data <- read_survey_data(survey_fn)

    # Check if coordinate columns exist
    if (!(survey_lat %in% names(original_data)) || !(survey_lon %in% names(original_data))) {
      stop(sprintf("Coordinate columns '%s' and '%s' not found in survey file",
                   survey_lat, survey_lon))
    }

    # Convert to sf object efficiently
    sf_obj <- st_as_sf(original_data,
                       coords = c(survey_lon, survey_lat),
                       crs = survey_crs,
                       agr = "constant")
  } else if (!is.null(shp_dt)) {
    if (!inherits(shp_dt, "sf")) {
      stop("Input shp_dt must be an sf object")
    }
    sf_obj <- shp_dt
    original_data <- as.data.table(st_drop_geometry(shp_dt))
  } else if (!is.null(shp_fn)) {
    # Read shapefile
    sf_obj <- st_read(shp_fn, quiet = TRUE)
    original_data <- as.data.table(st_drop_geometry(sf_obj))
  } else {
    stop("Please provide either shapefile data or survey data with coordinate columns")
  }

  # Handle buffering if needed
  if (!is.null(buffer_size)) {
    sf_obj <- st_transform(sf_obj, 3857) %>%
      st_buffer(buffer_size) %>%
      st_transform(4326)
  }

  # Ensure CRS is 4326
  if (st_crs(sf_obj)$epsg != 4326) {
    sf_obj <- st_transform(sf_obj, 4326)
  }

  # Load cell tower data efficiently
  cell_towers <- read_opencellid_data_cached(cell_tower_file)
  sprintf("Read %d cell towers", nrow(cell_towers))

  # Convert cell towers to sf object efficiently
  cell_towers_sf <- st_as_sf(cell_towers,
                             coords = c("lon", "lat"),
                             crs = 4326,
                             agr = "constant")

  # Create spatial index for efficiency
  cell_towers_sf <- st_sf(cell_towers_sf)

  # Get bounding box of input geometries
  bbox <- st_bbox(sf_obj)

  # Filter cell towers by bounding box first
  cell_towers_filtered <- cell_towers_sf[st_intersects(
    cell_towers_sf,
    st_as_sfc(bbox),
    sparse = FALSE
  )[,1], ]

  # Count towers per geometry using filtered dataset
  num_towers <- lengths(st_intersects(sf_obj, cell_towers_filtered, sparse = TRUE))

  # Add tower counts to original data
  original_data[, num_towers := num_towers]

  return(original_data)
}


