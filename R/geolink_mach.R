# globals
utils::globalVariables(c(
  "osm_id",
  "input_id"

))

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
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
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
#' In addition, raster download is now possible by setting `return_raster` argument to `TRUE`. This
#' will only return a raster however, to carry out zonal statistics estimation, set argument to `FALSE`
#' and specify other arguments as need. See examples below.
#'
#'
#' @examples
#'
#' \dontrun{
#' \donttest{
#'
#' ## raster download to the extent of a shapefile
#' df <- geolink_chirps(time_unit = "month",
#'                      start_date = "2020-01-01",
#'                      end_date = "2020-03-01",
#'                      shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
#'                      return_raster = TRUE)
#'
#' #examples of zonal statistics computation for a tesselated shapefile
#' df <- geolink_chirps(time_unit = "month",
#'                      start_date = "2020-01-01",
#'                      end_date = "2020-03-01",
#'                      shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
#'                      grid_size = 1000,
#'                      extract_fun = "mean")
#'
#'
#' ### estimating how much rainfall occurred within a certain distance of a household
#' ### assuming the user specifies the file location of a .dta file i.e. for STATA
#' ### users who do not normally use R
#'
#' temp_dir <- tempfile(fileext = ".dta")
#'
#' haven::write_dta(hhgeo_dt, temp_dir) ## assuming has stored .dta file locally
#'
#' df <- geolink_chirps(time_unit = "month",
#'                      start_date = "2020-01-01",
#'                      end_date = "2020-02-01",
#'                      survey_fn = temp_dir,
#'                      survey_lat = "y",
#'                      survey_lon = "x",
#'                      buffer_size = 1000,
#'                      extract_fun = "mean")
#'
#' ## examples of zonal statistics computation with weights
#'
#' fpath <- system.file("extdata", "pop.tif", package = "GeoLink")
#' pop_raster <- terra::rast(fpath)
#'
#'
#' df <- geolink_chirps(time_unit = "month",
#'                      start_date = "2020-01-01",
#'                      end_date = "2020-03-01",
#'                      shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
#'                      grid_size = 1000,
#'                      extract_fun = "weighted_mean",
#'                      weight_raster = pop_raster)
#'
#'
#' }}
#'
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
                           survey_crs = 4326,
                           return_raster = FALSE,
                           weight_raster = NULL) {

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

  raster_objs <- lapply(raster_objs, function(raster_obj) {
    raster_obj[raster_obj <= -9999] <- NA
    return(raster_obj)
  })

  name_set <- paste0("rainfall_", time_unit, 1:length(raster_objs))

  if (!is.null(weight_raster)) {
    if (is.list(weight_raster) && length(weight_raster) > 0) {
      weight_raster <- weight_raster[[1]]
      message("Using first raster from weight_raster list")
    }

    if (inherits(weight_raster, "SpatRaster")) {
      weight_raster <- raster::raster(weight_raster)
    }

    if (!is.null(raster_objs) && length(raster_objs) > 0) {
      rainfall_crs <- raster::crs(raster_objs[[1]])
      weight_crs <- raster::crs(weight_raster)

      if (!is.na(rainfall_crs) && !is.na(weight_crs)) {
        if (as.character(rainfall_crs) != as.character(weight_crs)) {
          message("Reprojecting weight raster to match rainfall raster CRS...")
          weight_raster <- raster::projectRaster(weight_raster, raster_objs[[1]])
        }
      }

      if (!raster::compareRaster(raster_objs[[1]], weight_raster,
                                 extent = TRUE, rowcol = TRUE,
                                 crs = TRUE, res = TRUE,
                                 stopiffalse = FALSE)) {
        message("Resampling weight raster to match rainfall raster extent and resolution...")
        weight_raster <- raster::resample(weight_raster, raster_objs[[1]], method = "bilinear")
      }
    }
  }

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
                               name_set = name_set,
                               return_raster = return_raster,
                               weight_raster = weight_raster)
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
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
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
                        survey_crs = 4326,
                        return_raster = FALSE,
                        weight_raster = NULL) {

  if (!is.null(shp_dt)) {
    shp_dt <- ensure_crs_4326(shp_dt)
  } else if (!is.null(shp_fn)) {
    shp_dt <- ensure_crs_4326(sf::st_read(shp_fn))
  }
  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
  } else if (!is.null(survey_fn)) {
    survey_dt <- ensure_crs_4326(sf::st_read(survey_fn))
  }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

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

  if (return_raster == TRUE && (!is.null(shp_dt) || !is.null(shp_fn))) {
    if (!is.null(shp_fn) && is.null(shp_dt)) {
      shp_for_crop <- sf::st_read(shp_fn)
    } else {
      shp_for_crop <- shp_dt
    }

    tryCatch({
      raster_crs <- raster::crs(raster_objs[[1]])
      shp_crs <- sf::st_crs(shp_for_crop)

      if (!is.na(raster_crs) && !is.na(shp_crs)) {
        if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
          message("Transforming shapefile to match raster CRS...")
          shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
        }
      }

      raster_ext <- raster::extent(raster_objs[[1]])
      shp_bbox <- sf::st_bbox(shp_for_crop)

      shp_ext <- raster::extent(shp_bbox[c("xmin", "xmax", "ymin", "ymax")])

      if (raster_ext@xmin < shp_ext@xmax &&
          raster_ext@xmax > shp_ext@xmin &&
          raster_ext@ymin < shp_ext@ymax &&
          raster_ext@ymax > shp_ext@ymin) {

        raster_objs <- lapply(raster_objs, function(r) {
          tryCatch({
            cropped <- raster::crop(r, shp_for_crop)
            raster::mask(cropped, shp_for_crop)
          }, error = function(e) {
            warning(paste("Error cropping raster:", e$message))
            return(r)
          })
        })
        message("Rasters successfully cropped to shapefile extent")
      } else {
        warning("Raster and shapefile extents do not overlap. Returning uncropped rasters.")
      }
    }, error = function(e) {
      warning(paste("Error in spatial processing:", e$message))
    })

    print("Process Complete!!!")
    unlink(paste0(tempdir(), "/file*"), recursive = TRUE)
    return(raster_objs)
  }

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
                               name_set = name_set,
                               return_raster = return_raster,
                               weight_raster = weight_raster)
  print("Process Complete!!!")
  unlink(paste0(tempdir(), "/file*"), recursive = TRUE)
  return(dt)
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
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
#'
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
                               file_location = tempdir(),
                               return_raster = FALSE,
                               weight_raster = NULL) {

  clear_temp = TRUE

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
                             !grepl("constrained", tif_files, ignore.case = TRUE) &
                             grepl("UNadj", tif_files, ignore.case = TRUE) &
                             grepl(year_pattern, tif_files)]
  }else if (constrained == "Y" && UN_adjst == "N"){
    tif_files <- tif_files[grepl(iso_code, tif_files, ignore.case = TRUE) &
                             grepl("ppp", tif_files, ignore.case = TRUE) &
                             grepl("constrained", tif_files, ignore.case = TRUE) &
                             !grepl("UNadj", tif_files, ignore.case = TRUE) &
                             grepl(year_pattern, tif_files)]
  }else {
    tif_files <- tif_files[grepl(iso_code, tif_files, ignore.case = TRUE) &
                             grepl("ppp", tif_files, ignore.case = TRUE) &
                             !grepl("constrained", tif_files, ignore.case = TRUE) &
                             !grepl("UNadj", tif_files, ignore.case = TRUE) &
                             grepl(year_pattern, tif_files)]
  }

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
              download_files_worldpop(file_urls, file_location)
            } else {
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
                               !grepl("constrained", tif_files, ignore.case = TRUE) &
                               grepl("UNadj", tif_files, ignore.case = TRUE) &
                               grepl(year_pattern, tif_files)]
    }else if (constrained == "Y" && UN_adjst == "N"){
      tif_files <- tif_files[grepl(iso_code, tif_files, ignore.case = TRUE) &
                               grepl("ppp", tif_files, ignore.case = TRUE) &
                               grepl("constrained", tif_files, ignore.case = TRUE) &
                               !grepl("UNadj", tif_files, ignore.case = TRUE) &
                               grepl(year_pattern, tif_files)]
    }else {
      tif_files <- tif_files[grepl(iso_code, tif_files, ignore.case = TRUE) &
                               grepl("ppp", tif_files, ignore.case = TRUE) &
                               !grepl("constrained", tif_files, ignore.case = TRUE) &
                               !grepl("UNadj", tif_files, ignore.case = TRUE) &
                               grepl(year_pattern, tif_files)]
    }
  } else {
    print("Using existing .tif files in file_location.")
  }

  raster_objs <- lapply(tif_files, function(x) {
    tryCatch({
      raster::raster(x)  # Changed from terra::rast to raster::raster
    }, error = function(e) {
      warning(paste("Failed to read:", x, "with error:", e))
      return(NULL)
    })
  })

  raster_objs <- raster_objs[!sapply(raster_objs, is.null)]

  if (length(raster_objs) == 0) {
    stop("No valid raster files found.")
  }

  if (!is.null(bespoke) && bespoke == "Y") {
    name_set <- paste0("population_", version)
  } else if (!is.null(start_year) && !is.null(end_year)) {
    name_set <- paste0("population_", years)
  } else {
    name_set <- "population_2020"
  }

  print("Population Raster Processed")

  if (return_raster == TRUE && (!is.null(shp_dt) || !is.null(shp_fn))) {
    # Prepare shapefile
    if (!is.null(shp_fn)) {
      shp_for_crop <- sf::st_read(shp_fn)
    } else {
      shp_for_crop <- shp_dt
    }

    tryCatch({
      # Check and transform CRS if needed
      raster_crs <- raster::crs(raster_objs[[1]])
      shp_crs <- sf::st_crs(shp_for_crop)

      if (!is.na(raster_crs) && !is.na(shp_crs)) {
        if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
          message("Transforming shapefile to match raster CRS...")
          shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
        }
      }

      raster_ext <- raster::extent(raster_objs[[1]])
      shp_bbox <- sf::st_bbox(shp_for_crop)

      shp_ext <- raster::extent(shp_bbox[c("xmin", "xmax", "ymin", "ymax")])

      if (raster_ext@xmin < shp_ext@xmax &&
          raster_ext@xmax > shp_ext@xmin &&
          raster_ext@ymin < shp_ext@ymax &&
          raster_ext@ymax > shp_ext@ymin) {

        raster_objs <- lapply(raster_objs, function(r) {
          tryCatch({
            cropped <- raster::crop(r, shp_for_crop)
            # Optionally mask to exact boundaries
            raster::mask(cropped, shp_for_crop)
          }, error = function(e) {
            warning(paste("Error cropping raster:", e$message))
            return(r)  # Return original if crop fails
          })
        })
        message("Rasters successfully cropped to shapefile extent")
      } else {
        warning("Raster and shapefile extents do not overlap. Returning uncropped rasters.")
      }
    }, error = function(e) {
      warning(paste("Error in spatial processing:", e$message))
    })

    print("Process Complete!!!")
    return(raster_objs)
  }

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
                               name_set = name_set,
                               return_raster = return_raster,
                               weight_raster = weight_raster)

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
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
#'
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
                              survey_crs = 4326,
                              return_raster = FALSE,
                              weight_raster = NULL){

  if (!is.null(shp_dt)) {
    shp_dt <- ensure_crs_4326(shp_dt)
  } else if (!is.null(shp_fn)) {
    shp_dt <- ensure_crs_4326(sf::st_read(shp_fn))
  }
  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
  } else if (!is.null(survey_fn)) {
    survey_dt <- ensure_crs_4326(sf::st_read(survey_fn))
  }

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

  raster_objs <- lapply(tif_files, function(f) {
    raster::raster(f)
  })

  print("Elevation Raster Downloaded")

  if (return_raster == TRUE && (!is.null(shp_dt) || !is.null(shp_fn))) {
    if (!is.null(shp_fn) && is.null(shp_dt)) {
      shp_for_crop <- sf::st_read(shp_fn)
    } else {
      shp_for_crop <- shp_dt
    }

    tryCatch({
      raster_crs <- raster::crs(raster_objs[[1]])
      shp_crs <- sf::st_crs(shp_for_crop)

      if (!is.na(raster_crs) && !is.na(shp_crs)) {
        if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
          message("Transforming shapefile to match raster CRS...")
          shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
        }
      }

      raster_ext <- raster::extent(raster_objs[[1]])
      shp_bbox <- sf::st_bbox(shp_for_crop)

      shp_ext <- raster::extent(shp_bbox[c("xmin", "xmax", "ymin", "ymax")])

      if (raster_ext@xmin < shp_ext@xmax &&
          raster_ext@xmax > shp_ext@xmin &&
          raster_ext@ymin < shp_ext@ymax &&
          raster_ext@ymax > shp_ext@ymin) {

        raster_objs <- lapply(raster_objs, function(r) {
          tryCatch({
            cropped <- raster::crop(r, shp_for_crop)
            raster::mask(cropped, shp_for_crop)
          }, error = function(e) {
            warning(paste("Error cropping raster:", e$message))
            return(r)
          })
        })
        message("Rasters successfully cropped to shapefile extent")
      } else {
        warning("Raster and shapefile extents do not overlap. Returning uncropped rasters.")
      }
    }, error = function(e) {
      warning(paste("Error in spatial processing:", e$message))
    })

    print("Process Complete!!!")
    unlink(paste0(tempdir(), "/elevation"), recursive = TRUE)
    return(raster_objs)
  }

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
                               name_set = name_set,
                               return_raster = return_raster,
                               weight_raster = weight_raster)
  print("Process Complete!!!")
  unlink(paste0(tempdir(), "/elevation"), recursive = TRUE)
  return(dt)
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
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
#'
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
                              indicators = "ALL",
                              return_raster = FALSE,
                              weight_raster = NULL){

  temp_dir <- tempdir()

  if (version == "v1.1") {
    url <- paste0("https://data.worldpop.org/repo/wopr/_MULT/buildings/v1.1/", iso_code, "_buildings_v1_1.zip")
    tryCatch({
      response <- httr::GET(url, httr::write_disk(file.path(tempdir(), basename(url)), overwrite = TRUE))
      if (httr::http_type(response) == "application/zip") {
        message("File downloaded successfully.")
        utils::unzip(file.path(tempdir(), basename(url)), exdir = tempdir())
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
      response <- httr::GET(url, httr::write_disk(file.path(tempdir(), basename(url)), overwrite = TRUE))
      if (httr::http_type(response) == "application/zip") {
        message("File downloaded successfully.")
        utils::unzip(file.path(tempdir(), basename(url)), exdir = tempdir())
        message("File unzipped successfully.")
      } else {
        warning("Downloaded file may not be a ZIP file.")
      }
    }, error = function(e) {
      print(e)
    })
  }

  tif_files <- list.files(path = temp_dir, pattern = "\\.tif$", full.names = TRUE)

  if (!all(indicators == "ALL")) {
    indicators <- paste(indicators, collapse = "|")
    tif_files <- tif_files[grepl(indicators, basename(tif_files))]
  }

  name_set <- c()
  for (file in tif_files) {
    base_name <- basename(file)
    extracted_string <- sub(".*1_([^\\.]+)\\.tif$", "\\1", base_name)
    name_set <- c(name_set, extracted_string)
  }

  raster_objs <- lapply(tif_files, function(f) {
    r <- raster::raster(f)

    if (is.na(raster::crs(r))) {
      raster::crs(r) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
      message(paste("Set CRS to WGS84 for", basename(f)))
    }

    return(r)
  })

  print("Building Raster Downloaded")

  if (return_raster == TRUE && (!is.null(shp_dt) || !is.null(shp_fn))) {
    if (!is.null(shp_fn)) {
      shp_for_crop <- sf::st_read(shp_fn)
    } else {
      shp_for_crop <- shp_dt
    }

    tryCatch({
      raster_crs <- raster::crs(raster_objs[[1]])
      shp_crs <- sf::st_crs(shp_for_crop)

      if (!is.na(raster_crs) && !is.na(shp_crs)) {
        if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
          message("Transforming shapefile to match raster CRS...")
          shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
        }
      }

      raster_ext <- raster::extent(raster_objs[[1]])
      shp_bbox <- sf::st_bbox(shp_for_crop)

      shp_ext <- raster::extent(shp_bbox[c("xmin", "xmax", "ymin", "ymax")])

      print("Raster extent:")
      print(raster_ext)
      print("Shapefile extent:")
      print(shp_ext)

      if (raster_ext@xmin < shp_ext@xmax &&
          raster_ext@xmax > shp_ext@xmin &&
          raster_ext@ymin < shp_ext@ymax &&
          raster_ext@ymax > shp_ext@ymin) {

        raster_objs <- lapply(raster_objs, function(r) {
          tryCatch({
            cropped <- raster::crop(r, shp_for_crop)
            raster::mask(cropped, shp_for_crop)
          }, error = function(e) {
            warning(paste("Error cropping raster:", e$message))
            return(r)
          })
        })
        message("Rasters successfully cropped to shapefile extent")
      } else {
        warning("Raster and shapefile extents do not overlap. Returning uncropped rasters.")
      }
    }, error = function(e) {
      warning(paste("Error in spatial processing:", e$message))
    })

    print("Process Complete!!!")
    unlink(paste0(tempdir(), "/", toupper(iso_code), "*"), recursive = TRUE)
    return(raster_objs)
  }

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
                               name_set = name_set,
                               return_raster = return_raster,
                               weight_raster = weight_raster)

  print("Process Complete!!!")
  unlink(paste0(tempdir(), "/", toupper(iso_code), "*"), recursive = TRUE)
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
#'  ("ACCESS-CM-2" or c("ACCESS-CM-2","UKESM1-0-LL")). See the cmip6:model summary in the STAC collection for a full list of models.
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
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
#'
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
#' @importFrom rstac stac items_sign get_request
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
                          survey_crs = 4326,
                          return_raster = FALSE,
                          weight_raster = NULL) {

  sf_obj <- prep_sf_obj_predownload(shp_dt = shp_dt,
                                    shp_fn = shp_fn,
                                    survey_dt = survey_dt,
                                    survey_fn = survey_fn)

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  it_obj <- s_obj %>%
    stac_search(
      collections = "nasa-nex-gddp-cmip6",
      bbox = sf::st_bbox(sf_obj),
      datetime = paste(start_date, end_date, sep = "/")
    ) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  filtered_features <- Filter(function(feature) {
    feature$properties$`cmip6:scenario` == scenario &&
      feature$properties$`cmip6:model` %in% desired_models
  }, it_obj$features)

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

  process_rasters_and_aggregate <- function(urls, filtered_features) {
    temp_dir <- tempdir()
    variables <- c("pr", "tas", "hurs", "huss", "rlds", "rsds", "tasmax", "tasmin", "sfcWind")
    raster_list <- list()

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

          if (ext(raster)[1] == 0 & ext(raster)[2] == 360) {
            raster <- rotate(raster)
          }

          if (nlyr(raster) >= 355 & nlyr(raster) <= 370) {
            time_indices <- as.numeric(format(time(raster), "%Y"))
            if(length(unique(time_indices)) == 1){
              yearly_raster <- app(raster, index = time_indices, fun = "mean", na.rm = TRUE)
            } else {
              yearly_raster <- tapp(raster, index = time_indices, fun = "mean", na.rm = TRUE)
            }

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

  raster_list <- process_rasters_and_aggregate(urls, filtered_features)
  raster_objs <- unlist(raster_list, recursive = FALSE)

  raster_objs <- lapply(raster_objs, function(r) {
    if (inherits(r, "SpatRaster")) {
      raster::raster(r)
    } else {
      r
    }
  })

  year_sequence <- seq(lubridate::year(start_date), lubridate::year(end_date))

  name_set <- unlist(lapply(year_sequence, function(year) {
    paste0(c("pr_", "tas_", "hurs_", "huss_", "rlds_", "rsds_", "tasmax_", "tasmin_", "sfcWind_"), year)
  }))

  if (return_raster == TRUE && (!is.null(shp_dt) || !is.null(shp_fn))) {
    if (!is.null(shp_fn) && is.null(shp_dt)) {
      shp_for_crop <- sf::st_read(shp_fn)
    } else {
      shp_for_crop <- shp_dt
    }

    tryCatch({
      raster_crs <- raster::crs(raster_objs[[1]])
      shp_crs <- sf::st_crs(shp_for_crop)

      if (!is.na(raster_crs) && !is.na(shp_crs)) {
        if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
          message("Transforming shapefile to match raster CRS...")
          shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
        }
      }

      raster_ext <- raster::extent(raster_objs[[1]])
      shp_bbox <- sf::st_bbox(shp_for_crop)

      shp_ext <- raster::extent(shp_bbox[c("xmin", "xmax", "ymin", "ymax")])

      if (raster_ext@xmin < shp_ext@xmax &&
          raster_ext@xmax > shp_ext@xmin &&
          raster_ext@ymin < shp_ext@ymax &&
          raster_ext@ymax > shp_ext@ymin) {

        raster_objs <- lapply(raster_objs, function(r) {
          tryCatch({
            cropped <- raster::crop(r, shp_for_crop)
            raster::mask(cropped, shp_for_crop)
          }, error = function(e) {
            warning(paste("Error cropping raster:", e$message))
            return(r)
          })
        })
        message("Rasters successfully cropped to shapefile extent")
      } else {
        warning("Raster and shapefile extents do not overlap. Returning uncropped rasters.")
      }
    }, error = function(e) {
      warning(paste("Error in spatial processing:", e$message))
    })

    for(model in desired_models){
      unlink(paste0(tempdir(), "/", model, "*"), recursive = TRUE)
    }

    return(raster_objs)
  }

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
    name_set = name_set,
    return_raster = return_raster,
    weight_raster = weight_raster
  )

  for(model in desired_models){
    unlink(paste0(tempdir(), "/", model, "*"), recursive = TRUE)
  }

  return(dt)
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
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
#'
#'
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
                             survey_crs = 4326,
                             return_raster = FALSE,
                             weight_raster = NULL){

  raster_objs <- geodata::cropland(source = source, path = tempdir())
  name_set <- "cropland"
  epsg_4326 <- "+init=EPSG:4326"
  terra::crs(raster_objs) <- epsg_4326
  if (is.null(crs(raster_objs))) {
    print("Projection failed for raster")
  } else {
    print(paste("Raster projected successfully."))
  }

  raster_list <- list(raster::raster(raster_objs))

  print("WorldCover Raster Downloaded")

  if (return_raster == TRUE && (!is.null(shp_dt) || !is.null(shp_fn))) {
    if (!is.null(shp_fn)) {
      shp_for_crop <- sf::st_read(shp_fn)
    } else {
      shp_for_crop <- shp_dt
    }

    tryCatch({
      raster_crs <- raster::crs(raster_list[[1]])
      shp_crs <- sf::st_crs(shp_for_crop)

      if (!is.na(raster_crs) && !is.na(shp_crs)) {
        if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
          message("Transforming shapefile to match raster CRS...")
          shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
        }
      }

      raster_ext <- raster::extent(raster_list[[1]])
      shp_bbox <- sf::st_bbox(shp_for_crop)

      shp_ext <- raster::extent(shp_bbox[c("xmin", "xmax", "ymin", "ymax")])

      if (raster_ext@xmin < shp_ext@xmax &&
          raster_ext@xmax > shp_ext@xmin &&
          raster_ext@ymin < shp_ext@ymax &&
          raster_ext@ymax > shp_ext@ymin) {

        raster_list <- lapply(raster_list, function(r) {
          tryCatch({
            cropped <- raster::crop(r, shp_for_crop)
            raster::mask(cropped, shp_for_crop)
          }, error = function(e) {
            warning(paste("Error cropping raster:", e$message))
            return(r)
          })
        })
        message("Raster successfully cropped to shapefile extent")
      } else {
        warning("Raster and shapefile extents do not overlap. Returning uncropped raster.")
      }
    }, error = function(e) {
      warning(paste("Error in spatial processing:", e$message))
    })

    print("Process Complete!!!")
    unlink(tempdir(), recursive = TRUE)
    return(raster_list)
  }

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
                               name_set = name_set,
                               return_raster = return_raster,
                               weight_raster = weight_raster)
  print("Process Complete!!!")
  unlink(tempdir(), recursive = TRUE)
  return(df)
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
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
#'
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
                              survey_crs = 4326,
                              return_raster = FALSE,
                              weight_raster = NULL){

  if (!is.null(shp_dt)) {
    shp_dt <- ensure_crs_4326(shp_dt)
  } else if (!is.null(shp_fn)) {
    shp_dt <- ensure_crs_4326(sf::st_read(shp_fn))
  }
  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
  } else if (!is.null(survey_fn)) {
    survey_dt <- ensure_crs_4326(sf::st_read(survey_fn))
  }

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
                        function(i) raster::raster(rasters_combined[[i]]))

  name_set <- c()
  num_layers <- terra::nlyr(rasters_combined)
  months <- month.abb
  name_set <- paste0(iso_code,"_WC_", var, "_", months)

  print("WorldClim Raster Downloaded")

  if (return_raster == TRUE && (!is.null(shp_dt) || !is.null(shp_fn))) {
    if (!is.null(shp_fn) && is.null(shp_dt)) {
      shp_for_crop <- sf::st_read(shp_fn)
    } else {
      shp_for_crop <- shp_dt
    }

    tryCatch({
      raster_crs <- raster::crs(raster_list[[1]])
      shp_crs <- sf::st_crs(shp_for_crop)

      if (!is.na(raster_crs) && !is.na(shp_crs)) {
        if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
          message("Transforming shapefile to match raster CRS...")
          shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
        }
      }

      raster_ext <- raster::extent(raster_list[[1]])
      shp_bbox <- sf::st_bbox(shp_for_crop)

      shp_ext <- raster::extent(shp_bbox[c("xmin", "xmax", "ymin", "ymax")])

      if (raster_ext@xmin < shp_ext@xmax &&
          raster_ext@xmax > shp_ext@xmin &&
          raster_ext@ymin < shp_ext@ymax &&
          raster_ext@ymax > shp_ext@ymin) {

        raster_list <- lapply(raster_list, function(r) {
          tryCatch({
            cropped <- raster::crop(r, shp_for_crop)
            raster::mask(cropped, shp_for_crop)
          }, error = function(e) {
            warning(paste("Error cropping raster:", e$message))
            return(r)
          })
        })
        message("Rasters successfully cropped to shapefile extent")
      } else {
        warning("Raster and shapefile extents do not overlap. Returning uncropped rasters.")
      }
    }, error = function(e) {
      warning(paste("Error in spatial processing:", e$message))
    })

    print("Process Complete!!!")
    unlink(tempdir(), recursive = TRUE)
    return(raster_list)
  }

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
                               name_set = name_set,
                               return_raster = return_raster,
                               weight_raster = weight_raster)
  print("Process Complete!!!")
  unlink(tempdir(), recursive = TRUE)
  return(dt)
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
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
#'
#'
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
                                 survey_crs = 4326,
                                 return_raster = FALSE,
                                 weight_raster = NULL) {

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is needed for this function to work. Please install it.")
  }

  if (!is.null(shp_dt)) {
    shp_dt <- ensure_crs_4326(shp_dt)
  }

  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
  }

  url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_", var, "_", year, ".nc")
  filename <- basename(url)
  destination_dir <- tempdir()
  destination <- file.path(destination_dir, filename)

  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
  }

  timeout_seconds <- 240
  print(paste("URL:", url))
  response <- try(GET(url, timeout(timeout_seconds)), silent = TRUE)

  if (inherits(response, "try-error")) {
    print("Error performing the GET request.")
    return(NULL)
  } else if (http_status(response)$category == "Success") {
    tryCatch({
      writeBin(content(response, "raw"), destination)
      print("File downloaded successfully.")
      print(paste("File saved to:", destination))
    }, error = function(e) {
      print(paste("Error writing the file:", e$message))
      return(NULL)
    })

    rasters_combined <- raster::brick(destination)

    if (is.na(raster::crs(rasters_combined))) {
      raster::crs(rasters_combined) <- "+proj=longlat +datum=WGS84 +no_defs"
    }

    raster_list <- lapply(1:raster::nlayers(rasters_combined), function(i) raster::raster(rasters_combined[[i]]))

    num_layers <- raster::nlayers(rasters_combined)
    months <- month.abb
    name_set <- paste0(var, "_", months)

    names(raster_list) <- name_set
    print(paste("Names set for raster layers:", paste(names(raster_list), collapse = ", ")))
    print("Terraclimate Raster Downloaded")

    if (return_raster == TRUE && (!is.null(shp_dt) || !is.null(shp_fn))) {
      if (!is.null(shp_fn) && is.null(shp_dt)) {
        shp_for_crop <- sf::st_read(shp_fn)
      } else {
        shp_for_crop <- shp_dt
      }

      tryCatch({
        raster_crs <- raster::crs(raster_list[[1]])
        shp_crs <- sf::st_crs(shp_for_crop)

        if (!is.na(raster_crs) && !is.na(shp_crs)) {
          if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
            message("Transforming shapefile to match raster CRS...")
            shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
          }
        }

        raster_ext <- raster::extent(raster_list[[1]])
        shp_bbox <- sf::st_bbox(shp_for_crop)

        shp_ext <- raster::extent(shp_bbox[c("xmin", "xmax", "ymin", "ymax")])

        if (raster_ext@xmin < shp_ext@xmax &&
            raster_ext@xmax > shp_ext@xmin &&
            raster_ext@ymin < shp_ext@ymax &&
            raster_ext@ymax > shp_ext@ymin) {

          raster_list <- lapply(raster_list, function(r) {
            tryCatch({
              cropped <- raster::crop(r, shp_for_crop)
              raster::mask(cropped, shp_for_crop)
            }, error = function(e) {
              warning(paste("Error cropping raster:", e$message))
              return(r)
            })
          })
          message("Rasters successfully cropped to shapefile extent")
        } else {
          warning("Raster and shapefile extents do not overlap. Returning uncropped rasters.")
        }
      }, error = function(e) {
        warning(paste("Error in spatial processing:", e$message))
      })

      print("Process Complete!!!")
      unlink(destination, force = TRUE)
      return(raster_list)
    }

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
                                 name_set = name_set,
                                 return_raster = return_raster,
                                 weight_raster = weight_raster)
    print("Process Complete!!!")
    unlink(destination, force = TRUE)
    return(dt)
  } else {
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
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
#'
#'
#'
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

geolink_get_poi <- function(osm_key,
                            osm_value = NULL,
                            shp_dt = NULL,
                            shp_fn = NULL,
                            survey_dt = NULL,
                            survey_fn = NULL,
                            survey_lat = NULL,
                            survey_lon = NULL,
                            buffer_size = NULL,
                            survey_crs = 4326,
                            grid_size = NULL,
                            return_raster = FALSE,
                            weight_raster = NULL) {

  max_retries = 3
  timeout = 300
  area_threshold = 1

  if (!osm_key %in% available_features()) {
    stop(sprintf("'%s' is not a valid OSM key", osm_key))
  }
  if (!is.null(osm_value)) {
    available_tags <- available_tags(osm_key)
    if (!osm_value %in% available_tags) {
      warning(sprintf("'%s' may not be a valid value for key '%s'", osm_value, osm_key))
    }
  }

  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
  }

  if (!is.null(survey_fn)) {
    if (is.null(survey_lat) || is.null(survey_lon)) {
      stop("Both survey_lat and survey_lon must be provided when using survey_fn")
    }

    tryCatch({
      if (grepl("\\.dta$", survey_fn)) {
        survey_dt <- haven::read_dta(survey_fn)
      } else {
        stop("Unsupported file format. Please provide .dta file")
      }
    }, error = function(e) {
      stop(sprintf("Error reading survey file: %s", e$message))
    })

    survey_dt <- st_as_sf(survey_dt,
                          coords = c(survey_lon, survey_lat),
                          crs = survey_crs)

    if (st_crs(survey_dt)$epsg != 4326) {
      survey_dt <- st_transform(survey_dt, 4326)
    }
  }

  if (!is.null(shp_dt)) {
    sf_obj <- zonalstats_prepshp(shp_dt = shp_dt, grid_size = grid_size) %>%
      ensure_crs_4326()
    input_source <- "shapefile"
  } else if (!is.null(shp_fn)) {
    sf_obj <- zonalstats_prepshp(shp_fn = shp_fn, grid_size = grid_size) %>%
      ensure_crs_4326()
    input_source <- "shapefile"
  } else if (!is.null(survey_dt) || !is.null(survey_fn)) {
    sf_obj <- zonalstats_prepsurvey(
      survey_dt = survey_dt,
      survey_fn = survey_fn,
      survey_lat = survey_lat,
      survey_lon = survey_lon,
      buffer_size = buffer_size,
      survey_crs = survey_crs) %>%
      ensure_crs_4326()
    input_source <- "survey"
  } else {
    stop("Please provide either a shapefile (shp_dt/shp_fn) or survey data (survey_dt/survey_fn)")
  }

  if (nrow(sf_obj) == 0) {
    stop("Input data is empty after filtering")
  }
  if (any(is.na(st_geometry(sf_obj)))) {
    stop("Input contains invalid geometries")
  }

  oldw <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = oldw))

  bbox <- st_bbox(sf_obj)
  bbox_area <- (bbox["xmax"] - bbox["xmin"]) * (bbox["ymax"] - bbox["ymin"])

  if (bbox_area > area_threshold) {
    message("Large area detected. Splitting into quadrants...")

    mid_x <- (bbox["xmax"] + bbox["xmin"]) / 2
    mid_y <- (bbox["ymax"] + bbox["ymin"]) / 2

    quadrants <- list(
      q1 = c(xmin = bbox["xmin"], ymin = mid_y, xmax = mid_x, ymax = bbox["ymax"]),
      q2 = c(xmin = mid_x, ymin = mid_y, xmax = bbox["xmax"], ymax = bbox["ymax"]),
      q3 = c(xmin = bbox["xmin"], ymin = bbox["ymin"], xmax = mid_x, ymax = mid_y),
      q4 = c(xmin = mid_x, ymin = bbox["ymin"], xmax = bbox["xmax"], ymax = mid_y)
    )

    results_list <- lapply(quadrants, function(quad_bbox) {
      process_bbox_quadrant(quad_bbox, osm_key, osm_value)
    })

    results_list <- results_list[!sapply(results_list, is.null)]

    if (length(results_list) == 0) {
      message("No results found in any quadrant")
      return(NULL)
    }

    all_cols <- unique(unlist(lapply(results_list, names)))

    results_list <- lapply(results_list, function(df) {
      missing_cols <- setdiff(all_cols, names(df))
      if (length(missing_cols) > 0) {
        for (col in missing_cols) {
          df[[col]] <- NA
        }
      }
      return(df[, all_cols])
    })

    results <- do.call(rbind, results_list)

  } else {
    message("Processing area as single unit...")
    features <- get_osm_data(bbox, osm_key, osm_value, max_retries, timeout)
    results <- features$osm_points %>%
      filter(if_any(-c(osm_id, geometry), ~ !is.na(.x)))
  }

  if (is.null(results) || nrow(results) == 0) {
    message("No points of interest found in the specified area")
    return(NULL)
  }

  message(sprintf("Found %d points of interest", nrow(results)))

  sf_obj$input_id <- 1:nrow(sf_obj)

  joined_data <- st_join(results, sf_obj, left = TRUE)

  outside_pois <- joined_data %>%
    filter(is.na(input_id))

  joined_data <- joined_data %>%
    filter(!is.na(input_id))

  if (nrow(joined_data) == 0) {
    message("No points of interest found within the specified geometries")
    message("All POIs are outside your areas of interest")

    outside_pois$within_input_geom <- FALSE
    outside_pois$input_source <- input_source

    if (return_raster == TRUE) {
      warning("No POIs within input geometries. Cannot create raster.")
      return(NULL)
    }

    return(outside_pois)
  }

  joined_data$within_input_geom <- TRUE
  joined_data$input_source <- input_source

  final_result <- joined_data

  coords <- st_coordinates(final_result)
  final_result$longitude <- coords[, "X"]
  final_result$latitude <- coords[, "Y"]

  if (!is.null(weight_raster)) {
    message("Extracting weight values at POI locations...")

    if (is.list(weight_raster)) {
      weight_raster_use <- weight_raster[[1]]
    } else {
      weight_raster_use <- weight_raster
    }

    if (inherits(weight_raster_use, "SpatRaster")) {
      weight_raster_use <- raster::raster(weight_raster_use)
    }

    tryCatch({
      poi_crs <- st_crs(final_result)
      weight_crs <- raster::crs(weight_raster_use)

      if (!is.na(weight_crs) && !is.na(poi_crs)) {
        if (as.character(weight_crs) != as.character(poi_crs$proj4string)) {
          message("Transforming POIs to match weight raster CRS...")
          poi_for_extract <- st_transform(final_result, crs = weight_crs)
        } else {
          poi_for_extract <- final_result
        }
      } else {
        poi_for_extract <- final_result
      }

      weight_values <- raster::extract(weight_raster_use, poi_for_extract)

      final_result$weight_value <- weight_values

      message(sprintf("Successfully extracted weight values for %d POIs", sum(!is.na(weight_values))))

      if (sum(!is.na(weight_values)) == 0) {
        warning("All weight values are NA. Check if weight raster covers the POI locations.")
      } else {
        message(sprintf("Weight values range: %.2f to %.2f",
                        min(weight_values, na.rm = TRUE),
                        max(weight_values, na.rm = TRUE)))
      }

    }, error = function(e) {
      warning(sprintf("Could not extract weight values: %s", e$message))
      final_result$weight_value <- NA
    })
  }

  if (return_raster == TRUE) {
    message("Converting POI data to raster...")

    if (is.null(grid_size)) {
      grid_size <- 1000
      message(sprintf("No grid_size specified. Using default resolution of %d meters", grid_size))
    }

    final_result$poi_count <- 1

    if (!is.null(weight_raster) && "weight_value" %in% names(final_result)) {
      poi_subset <- final_result %>%
        select(weight_value, geometry)
      agg_field <- "weight_value"
    } else {
      poi_subset <- final_result %>%
        select(poi_count, geometry)
      agg_field <- "poi_count"
    }

    tryCatch({
      poi_raster <- point_sf_to_raster(
        point_sf = poi_subset,
        crs = "EPSG:4326",
        resolution = grid_size / 111000,
        agg_fun = sum
      )

      poi_raster_r <- raster::raster(poi_raster)

      if (!is.null(shp_dt) || !is.null(shp_fn)) {
        if (!is.null(shp_fn)) {
          shp_for_crop <- sf::st_read(shp_fn)
        } else {
          shp_for_crop <- shp_dt
        }

        raster_crs <- raster::crs(poi_raster_r)
        shp_crs <- sf::st_crs(shp_for_crop)

        if (!is.na(raster_crs) && !is.na(shp_crs)) {
          if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
            shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
          }
        }

        poi_raster_r <- raster::crop(poi_raster_r, shp_for_crop)
        poi_raster_r <- raster::mask(poi_raster_r, shp_for_crop)
      }

      message("POI data successfully converted to raster")
      return(list(poi_raster_r))

    }, error = function(e) {
      warning(sprintf("Could not convert POI data to raster: %s", e$message))
      return(final_result)
    })
  }

  message("OpenStreetMap data download complete!")

  return(final_result)
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
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
#'
#'
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
    grid_size = NULL,
    survey_dt = NULL,
    survey_fn = NULL,
    survey_lat = NULL,
    survey_lon = NULL,
    buffer_size = NULL,
    extract_fun = "mean",
    survey_crs = 4326,
    return_raster = FALSE,
    weight_raster = NULL
) {

  mosaic_and_crop = TRUE

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (!is.null(shp_dt)) {
    sf_obj <- zonalstats_prepshp(shp_dt = shp_dt, grid_size = grid_size) %>%
      ensure_crs_4326()
  } else if (!is.null(shp_fn)) {
    sf_obj <- zonalstats_prepshp(shp_fn = shp_fn, grid_size = grid_size) %>%
      ensure_crs_4326()
  } else if (!is.null(survey_dt) || !is.null(survey_fn)) {
    if (!is.null(survey_fn)) {
      if (is.null(survey_lat) || is.null(survey_lon)) {
        stop("Both survey_lat and survey_lon must be provided when using survey_fn")
      }

      tryCatch({
        if (grepl("\\.dta$", survey_fn)) {
          survey_dt <- haven::read_dta(survey_fn)
        } else if (grepl("\\.csv$", survey_fn)) {
          survey_dt <- utils::read.csv(survey_fn)
        } else {
          stop("Unsupported file format. Please provide .dta or .csv file")
        }
      }, error = function(e) {
        stop(sprintf("Error reading survey file: %s", e$message))
      })

      survey_dt <- st_as_sf(survey_dt,
                            coords = c(survey_lon, survey_lat),
                            crs = survey_crs)
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

  required_assets <- c("lightscore", "light-composite", "night-proportion", "estimated-brightness")
  url_list <- lapply(it_obj$features, function(feature) {
    missing_assets <- required_assets[!required_assets %in% names(feature$assets)]
    if (length(missing_assets) > 0) {
      warning(sprintf("Missing assets: %s", paste(missing_assets, collapse = ", ")))
      return(NULL)
    }

    year <- as.integer(format(as.Date(feature$properties$datetime), "%Y"))

    list(
      lightscore = feature$assets$lightscore$href,
      light_composite = feature$assets$`light-composite`$href,
      night_proportion = feature$assets$`night-proportion`$href,
      estimated_brightness = feature$assets$`estimated-brightness`$href,
      year = year
    )
  })

  years <- unique(sapply(url_list, function(x) x$year))
  print(paste("Found data for years:", paste(years, collapse=", ")))

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

  print("Downloading rasters...")
  downloaded_files <- list()

  for (i in seq_along(url_list)) {
    item <- url_list[[i]]
    year <- item$year

    for (asset_name in names(item)[names(item) != "year"]) {
      url <- item[[asset_name]]
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

  if (mosaic_and_crop && !is.null(shp_dt)) {
    temp_shp <- file.path(temp_dir, "shape.gpkg")
    sf::st_write(sf_obj, temp_shp, delete_layer = TRUE)
    print(paste("Saved shapefile to temporary location:", temp_shp))
  }

  raster_objs <- list()

  if (mosaic_and_crop) {
    print("Performing mosaicking and cropping...")

    python_script <- system.file("python_scripts/mosaic_crop.py", package = "geolink")

    if (!file.exists(python_script)) {
      warning("Python script not found at ", python_script, ". Falling back to direct raster loading.")
      mosaic_and_crop <- FALSE
    } else {
      for (key in names(download_by_year_asset)) {
        file_paths <- download_by_year_asset[[key]]
        parts <- strsplit(key, "_")[[1]]
        year <- parts[1]
        asset <- parts[2]

        print(paste("Processing", asset, "for year", year))

        processed_path <- file.path(temp_dir, paste0("processed_", key, ".tif"))

        python_cmd <- paste(
          "python",
          shQuote(python_script),
          shQuote(temp_shp),
          shQuote(processed_path),
          paste(sapply(file_paths, shQuote), collapse = " ")
        )

        print(paste("Executing:", python_cmd))

        result <- system(python_cmd, intern = TRUE)

        success_line <- grep("^SUCCESS:", result, value = TRUE)

        if (length(success_line) > 0) {
          final_path <- sub("^SUCCESS:", "", success_line)
          print(paste("Successfully processed", asset, "for year", year))

          tryCatch({
            rast_obj <- terra::rast(final_path)
            if (!is.null(rast_obj)) {
              raster_objs[[length(raster_objs) + 1]] <- rast_obj
              names(raster_objs)[length(raster_objs)] <- paste0(asset, "_", year)
            }
          }, error = function(e) {
            warning(sprintf("Failed to load processed raster for %s, year %s: %s",
                            asset, year, e$message))
          })
        } else {
          warning(paste("Failed to process", asset, "for year", year))
          print(result)
        }
      }
    }
  }

  if (!mosaic_and_crop || length(raster_objs) == 0) {
    print("Loading individual rasters...")

    for (file_info in downloaded_files) {
      tryCatch({
        rast_obj <- terra::rast(file_info$path)
        if (!is.null(rast_obj)) {
          raster_objs[[length(raster_objs) + 1]] <- rast_obj
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

  raster_objs <- lapply(raster_objs, function(r) {
    if (inherits(r, "SpatRaster")) {
      raster::raster(r)
    } else {
      r
    }
  })

  name_set <- names(raster_objs)

  print("Electrification Access Raster Downloaded and Processed")
  print(sprintf("Processing %d rasters", length(raster_objs)))

  if (return_raster == TRUE && (!is.null(shp_dt) || !is.null(shp_fn))) {
    if (!is.null(shp_fn) && is.null(shp_dt)) {
      shp_for_crop <- sf::st_read(shp_fn)
    } else {
      shp_for_crop <- shp_dt
    }

    tryCatch({
      raster_crs <- raster::crs(raster_objs[[1]])
      shp_crs <- sf::st_crs(shp_for_crop)

      if (!is.na(raster_crs) && !is.na(shp_crs)) {
        if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
          message("Transforming shapefile to match raster CRS...")
          shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
        }
      }

      raster_ext <- raster::extent(raster_objs[[1]])
      shp_bbox <- sf::st_bbox(shp_for_crop)

      shp_ext <- raster::extent(shp_bbox[c("xmin", "xmax", "ymin", "ymax")])

      if (raster_ext@xmin < shp_ext@xmax &&
          raster_ext@xmax > shp_ext@xmin &&
          raster_ext@ymin < shp_ext@ymax &&
          raster_ext@ymax > shp_ext@ymin) {

        raster_objs <- lapply(raster_objs, function(r) {
          tryCatch({
            cropped <- raster::crop(r, shp_for_crop)
            raster::mask(cropped, shp_for_crop)
          }, error = function(e) {
            warning(paste("Error cropping raster:", e$message))
            return(r)
          })
        })
        message("Rasters successfully cropped to shapefile extent")
      } else {
        warning("Raster and shapefile extents do not overlap. Returning uncropped rasters.")
      }
    }, error = function(e) {
      warning(paste("Error in spatial processing:", e$message))
    })

    print("Process Complete!!!")
    return(raster_objs)
  }

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
    name_set = name_set,
    return_raster = return_raster,
    weight_raster = weight_raster
  )

  print("Process Complete!!!")
  return(dt)
}

#' Download OpenCellID data
#'
#' @details This function processes downloaded OpenCellID data,
#' which provides information about cell towers and their coverage areas.
#' The return dataframe gives a count of cell towers within the shapefile area.
#' For the function to run, the user will need to set up an account on www.opencellid.org and download the
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
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
#'
#'
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
#' opencellid_shp <- geolink_opencellid(cell_tower_file = "data/621.csv.gz",
#'                                      shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#'
#'
#' opencellid_shp_fn <- geolink_opencellid(cell_tower_file = "data/621.csv.gz",
#'                                         shp_fn = "tests/testthat/testdata/shp_dt.shp")
#'
#'
#' opencellid_survey <- geolink_opencellid(cell_tower_file = "data/621.csv.gz",
#'                                         buffer_size = 2000,
#'                                         survey_dt = hhgeo_dt[hhgeo_dt$ADM1_EN == "Abia",])
#'
#'
#' opencellid_survey_fn <- geolink_opencellid(cell_tower_file = "data/621.csv.gz",
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
                               grid_size = NULL,
                               return_raster = FALSE,
                               weight_raster = NULL) {

  resolution = 1000
  name_set = "cell_towers"

  read_opencellid_data <- function(file_path) {
    if (grepl("\\.gz$", file_path)) {
      message("Reading gzipped file...")
      data <- data.table::fread(file_path)
    } else {
      data <- data.table::fread(file_path)
    }

    if (!all(c("lat", "lon") %in% colnames(data))) {
      if (ncol(data) >= 8) {
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

  original_sf_obj <- sf_obj

  if (!is.null(buffer_size)) {
    message(sprintf("Creating buffer of %s meters aroundpoints...", buffer_size))
    sf_obj <- sf::st_transform(sf_obj, 3857) %>%
      sf::st_buffer(buffer_size) %>%
      sf::st_transform(4326)
  }

  if (sf::st_crs(sf_obj)$epsg != 4326 | is.na(sf::st_crs(sf_obj)$epsg)) {
    sf_obj <- sf::st_transform(sf_obj, 4326)
  }

  message("Reading cell tower data...")
  cell_towers <- read_opencellid_data_cached(cell_tower_file)
  message(sprintf("Read %d cell towers", nrow(cell_towers)))

  cell_towers_sf <- sf::st_as_sf(cell_towers,
                                 coords = c("lon", "lat"),
                                 crs = 4326,
                                 agr = "constant")

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

    if (return_raster == TRUE) {
      warning("No cell towers found. Cannot create raster.")
      return(NULL)
    }

    num_towers <- rep(0, nrow(original_sf_obj))

    if (!is.null(weight_raster)) {
      original_data$weighted_towers <- 0
      original_data$avg_tower_weight <- NA
    }
  } else {
    message(sprintf("Found %d cell towers within bounding box", nrow(cell_towers_filtered)))

    if (return_raster == TRUE) {
      message("Creating cell tower density raster...")

      if (is.null(grid_size)) {
        grid_size <- 1000
        message(sprintf("No grid_size specified. Using default resolution of %d meters", grid_size))
      }

      cell_towers_filtered$tower_count <- 1

      if (!is.null(weight_raster)) {
        message("Extracting weight values for cell towers...")

        if (is.list(weight_raster)) {
          weight_raster_use <- weight_raster[[1]]
        } else {
          weight_raster_use <- weight_raster
        }

        if (inherits(weight_raster_use, "SpatRaster")) {
          weight_raster_use <- raster::raster(weight_raster_use)
        }

        tryCatch({
          tower_crs <- st_crs(cell_towers_filtered)
          weight_crs <- raster::crs(weight_raster_use)

          if (!is.na(weight_crs) && !is.na(tower_crs)) {
            if (as.character(weight_crs) != as.character(tower_crs$proj4string)) {
              towers_for_extract <- st_transform(cell_towers_filtered, crs = weight_crs)
            } else {
              towers_for_extract <- cell_towers_filtered
            }
          } else {
            towers_for_extract <- cell_towers_filtered
          }

          tower_weights <- raster::extract(weight_raster_use, towers_for_extract)
          tower_weights[is.na(tower_weights)] <- 0
          cell_towers_filtered$weight <- tower_weights

        }, error = function(e) {
          warning("Could not extract weight values. Using unweighted counts.")
          cell_towers_filtered$weight <- 1
        })

        tower_subset <- cell_towers_filtered %>%
          select(weight, geometry)

        agg_field <- "weight"
      } else {
        tower_subset <- cell_towers_filtered %>%
          select(tower_count, geometry)

        agg_field <- "tower_count"
      }

      tryCatch({
        tower_raster <- point_sf_to_raster(
          point_sf = tower_subset,
          crs = "EPSG:4326",
          resolution = grid_size / 111000,
          agg_fun = sum
        )

        tower_raster_r <- raster::raster(tower_raster)

        if (!is.null(shp_dt) || !is.null(shp_fn)) {
          if (!is.null(shp_fn)) {
            shp_for_crop <- sf::st_read(shp_fn)
          } else {
            shp_for_crop <- shp_dt
          }

          raster_crs <- raster::crs(tower_raster_r)
          shp_crs <- sf::st_crs(shp_for_crop)

          if (!is.na(raster_crs) && !is.na(shp_crs)) {
            if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
              shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
            }
          }

          tower_raster_r <- raster::crop(tower_raster_r, shp_for_crop)
          tower_raster_r <- raster::mask(tower_raster_r, shp_for_crop)
        }

        message("Cell tower data successfully converted to raster")
        return(list(tower_raster_r))

      }, error = function(e) {
        warning(sprintf("Could not convert cell tower data to raster: %s", e$message))
        return(NULL)
      })
    }

    message("Counting cell towers within buffered areas...")

    if (!is.null(weight_raster)) {
      message("Extracting weight values at cell tower locations...")

      if (is.list(weight_raster)) {
        weight_raster_use <- weight_raster[[1]]
      } else {
        weight_raster_use <- weight_raster
      }

      if (inherits(weight_raster_use, "SpatRaster")) {
        weight_raster_use <- raster::raster(weight_raster_use)
      }

      tryCatch({
        tower_crs <- st_crs(cell_towers_filtered)
        weight_crs <- raster::crs(weight_raster_use)

        if (!is.na(weight_crs) && !is.na(tower_crs)) {
          if (as.character(weight_crs) != as.character(tower_crs$proj4string)) {
            message("Transforming cell towers to match weight raster CRS...")
            towers_for_extract <- st_transform(cell_towers_filtered, crs = weight_crs)
          } else {
            towers_for_extract <- cell_towers_filtered
          }
        } else {
          towers_for_extract <- cell_towers_filtered
        }

        tower_weights <- raster::extract(weight_raster_use, towers_for_extract)

        tower_weights[is.na(tower_weights)] <- 0

        cell_towers_filtered$weight <- tower_weights

        message(sprintf("Successfully extracted weight values for %d cell towers", sum(tower_weights > 0)))

        if (sum(tower_weights > 0) == 0) {
          warning("All weight values are 0 or NA. Check if weight raster covers the cell tower locations.")
        } else {
          message(sprintf("Weight values range: %.2f to %.2f",
                          min(tower_weights[tower_weights > 0]),
                          max(tower_weights)))
        }

      }, error = function(e) {
        warning(sprintf("Could not extract weight values: %s. Using unweighted counts.", e$message))
        cell_towers_filtered$weight <- 1
      })

      num_towers <- numeric(nrow(sf_obj))
      weighted_towers <- numeric(nrow(sf_obj))
      avg_tower_weights <- numeric(nrow(sf_obj))

      for (i in 1:nrow(sf_obj)) {
        intersecting_towers <- sf::st_intersects(sf_obj[i,], cell_towers_filtered, sparse = FALSE)[1,]
        num_towers[i] <- sum(intersecting_towers)

        if (num_towers[i] > 0) {
          tower_weights_in_area <- cell_towers_filtered$weight[intersecting_towers]
          weighted_towers[i] <- sum(tower_weights_in_area)
          avg_tower_weights[i] <- mean(tower_weights_in_area, na.rm = TRUE)
        } else {
          weighted_towers[i] <- 0
          avg_tower_weights[i] <- NA
        }
      }

      message(sprintf("Total towers found in all buffers: %d", sum(num_towers)))
      message(sprintf("Total weighted tower value: %.2f", sum(weighted_towers)))

      original_data$weighted_towers <- weighted_towers
      original_data$avg_tower_weight <- avg_tower_weights

    } else {
      num_towers <- lengths(sf::st_intersects(sf_obj, cell_towers_filtered, sparse = TRUE))
      message(sprintf("Total towers found in all buffers: %d", sum(num_towers)))
    }
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
#' Using resampling might use a significant amount of memory.
#' @param target_resolution Numeric. Target resolution for resampling in meters (default: 1000)
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
#'
#'
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
#' @import sf rstac terra
#' @importFrom haven read_dta
#' @importFrom httr GET write_disk config timeout status_code
#' @importFrom exactextractr exact_extract
#' @importFrom reticulate use_condaenv use_python py_run_string source_python
#'
#' @export

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
                              target_resolution = 1000,
                              return_raster = FALSE,
                              weight_raster = NULL) {

  is_ubuntu <- FALSE

  if (file.exists("/etc/os-release")) {
    os_info <- readLines("/etc/os-release")
    is_ubuntu <- any(grepl("ubuntu", tolower(os_info), fixed = TRUE))
  }
  if (!is_ubuntu && file.exists("/etc/lsb-release")) {
    lsb_info <- readLines("/etc/lsb-release")
    is_ubuntu <- any(grepl("ubuntu", tolower(lsb_info), fixed = TRUE))
  }
  if (!is_ubuntu) {
    sys_info <- try(system("lsb_release -a", intern = TRUE), silent = TRUE)
    if (!inherits(sys_info, "try-error")) {
      is_ubuntu <- any(grepl("ubuntu", tolower(sys_info), fixed = TRUE))
    }
  }
  if (is_ubuntu && use_resampling) {
    use_resampling <- FALSE
    message("Ubuntu system detected. Setting use_resampling to FALSE for compatibility.")
  }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
  } else if (!is.null(survey_fn)) {
    if (is.null(survey_lat) || is.null(survey_lon)) {
      stop("Both survey_lat and survey_lon must be provided when using survey_fn")
    }

    survey_dt <- try({
      if (grepl("\\.dta$", survey_fn)) {
        haven::read_dta(survey_fn)
      } else if (grepl("\\.csv$", survey_fn)) {
        utils::read.csv(survey_fn)
      } else {
        stop("Unsupported file format. Please provide .dta or .csv file")
      }
    }, silent = TRUE)

    if (inherits(survey_dt, "try-error")) {
      stop("Error reading survey file")
    }

    survey_dt <- st_as_sf(survey_dt,
                          coords = c(survey_lon, survey_lat),
                          crs = survey_crs)

    if (st_crs(survey_dt)$epsg != 4326) {
      survey_dt <- st_transform(survey_dt, 4326)
    }
  }

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

  sf_obj <- sf::st_make_valid(sf_obj)

  Sys.unsetenv("RETICULATE_PYTHON")

  geo_env_name <- get("pkg_env", envir = asNamespace("GeoLink"))$conda_env_name

  env_setup_success <- try({
    reticulate::use_condaenv(geo_env_name, required = TRUE)
    TRUE
  }, silent = TRUE)

  if (inherits(env_setup_success, "try-error")) {
    python_path <- get("pkg_env", envir = asNamespace("GeoLink"))$python_path
    if (!is.null(python_path) && file.exists(python_path)) {
      reticulate::use_python(python_path, required = TRUE)
    } else {
      stop("Failed to configure Python environment")
    }
  }

  try({
    reticulate::py_run_string("
    import certifi
    import os
    os.environ['SSL_CERT_FILE'] = certifi.where()
    ")
  }, silent = TRUE)

  python_utils_path <- system.file("python_scripts", "raster_utils.py", package = "GeoLink")
  if (!file.exists(python_utils_path)) {
    stop("Python utilities not found. Check package installation.")
  }
  reticulate::source_python(python_utils_path)

  filter_features <- function(feature, start_date, end_date) {
    feature_date <- as.Date(feature$properties$start_datetime)
    feature_year <- format(feature_date, "%Y")
    start_year <- format(as.Date(start_date), "%Y")
    end_year <- format(as.Date(end_date), "%Y")

    return(feature_year >= start_year && feature_year <= end_year)
  }

  stac_result <- try({
    s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

    bbox <- sf::st_bbox(sf_obj)
    buffered_bbox <- c(
      bbox["xmin"] - 0.1,
      bbox["ymin"] - 0.1,
      bbox["xmax"] + 0.1,
      bbox["ymax"] + 0.1
    )

    it_obj <- s_obj %>%
      stac_search(
        collections = "io-lulc-annual-v02",
        bbox = buffered_bbox,
        datetime = paste(start_date, end_date, sep = "/")
      ) %>%
      get_request() %>%
      items_sign(sign_fn = sign_planetary_computer())

    it_obj$features <- it_obj$features[sapply(it_obj$features, function(feature) {
      filter_features(feature, start_date, end_date)
    })]

    it_obj
  }, silent = TRUE)

  if (inherits(stac_result, "try-error") || length(stac_result$features) == 0) {
    warning("No features found. Returning empty dataset.")
    return(create_empty_result(sf_obj, start_date))
  }

  temp_dir <- tempdir()
  raster_year_map <- list()

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

  if (length(raster_year_map) == 0) {
    warning("No raster data could be downloaded. Returning empty dataset.")
    return(create_empty_result(sf_obj, start_date))
  }

  if (return_raster == TRUE) {
    message("Preparing rasters for return...")

    raster_list <- list()

    for (year in names(raster_year_map)) {
      message(paste("Processing rasters for year:", year))

      raster_paths <- as.character(raster_year_map[[year]])

      if (!all(file.exists(raster_paths))) {
        warning(paste("Some raster files for year", year, "do not exist. Skipping."))
        next
      }

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

      raster_terra <- try({
        terra::rast(raster_path)
      }, silent = TRUE)

      if (inherits(raster_terra, "try-error")) {
        warning(paste("Failed to load raster for year", year))
        next
      }

      if (is.na(terra::crs(raster_terra))) {
        terra::crs(raster_terra) <- "EPSG:4326"
      }

      raster_r <- raster::raster(raster_terra)

      if (!is.null(shp_dt) || !is.null(shp_fn)) {
        if (!is.null(shp_fn)) {
          shp_for_crop <- sf::st_read(shp_fn)
        } else {
          shp_for_crop <- shp_dt
        }

        raster_crs <- raster::crs(raster_r)
        shp_crs <- sf::st_crs(shp_for_crop)

        if (!is.na(raster_crs) && !is.na(shp_crs)) {
          if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
            message("Transforming shapefile to match raster CRS...")
            shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
          }
        }

        raster_ext <- raster::extent(raster_r)
        shp_bbox <- sf::st_bbox(shp_for_crop)
        shp_ext <- raster::extent(shp_bbox[c("xmin", "xmax", "ymin", "ymax")])

        if (raster_ext@xmin < shp_ext@xmax &&
            raster_ext@xmax > shp_ext@xmin &&
            raster_ext@ymin < shp_ext@ymax &&
            raster_ext@ymax > shp_ext@ymin) {

          raster_r <- raster::crop(raster_r, shp_for_crop)
          raster_r <- raster::mask(raster_r, shp_for_crop)
          message(paste("Raster for year", year, "successfully cropped to shapefile extent"))
        } else {
          warning(paste("Raster and shapefile extents do not overlap for year", year))
        }
      }

      names(raster_r) <- paste0("landcover_", year)
      raster_list[[year]] <- raster_r
    }

    if (length(raster_list) == 0) {
      warning("No rasters could be processed. Returning NULL.")
      return(NULL)
    }

    message("Land cover rasters successfully processed")
    return(raster_list)
  }

  results_list <- list()

  for (year in names(raster_year_map)) {
    message("Processing rasters for year:", year)

    raster_paths <- as.character(raster_year_map[[year]])

    if (!all(file.exists(raster_paths))) {
      warning(paste("Some raster files for year", year, "do not exist. Skipping."))
      next
    }

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

    raster <- try({
      terra::rast(raster_path)
    }, silent = TRUE)

    if (inherits(raster, "try-error")) {
      warning(paste("Failed to load raster for year", year))
      next
    }

    if (is.na(terra::crs(raster))) {
      terra::crs(raster) <- "EPSG:4326"
    }

    class_values <- unlist(lapply(land_cover_classes, function(x) x$values))
    class_names <- tolower(gsub(" ", "_", unlist(lapply(land_cover_classes, function(x) x$summary))))
    all_column_names <- c(class_names, "no_data")

    message(paste("Extracting land cover proportions for year:", year))

    year_results <- sf::st_drop_geometry(sf_obj)

    for (col in all_column_names) {
      year_results[[col]] <- 0
    }

    if (!is.null(weight_raster)) {
      message("Using weight raster for weighted extraction...")

      if (is.list(weight_raster)) {
        weight_raster_use <- weight_raster[[1]]
      } else {
        weight_raster_use <- weight_raster
      }

      if (inherits(weight_raster_use, "SpatRaster")) {
        weight_raster_use <- raster::raster(weight_raster_use)
      }

      weight_terra <- terra::rast(weight_raster_use)

      if (!terra::compareGeom(raster, weight_terra, stopOnError = FALSE)) {
        message("Resampling weight raster to match land cover raster...")
        weight_terra <- terra::resample(weight_terra, raster, method = "bilinear")
      }

      extracted_values <- try({
        exactextractr::exact_extract(raster, sf::st_make_valid(sf_obj),
                                     coverage_area = TRUE,
                                     weights = weight_terra)
      }, silent = TRUE)
    } else {
      extracted_values <- try({
        exactextractr::exact_extract(raster, sf::st_make_valid(sf_obj),
                                     coverage_area = TRUE)
      }, silent = TRUE)
    }

    if (!inherits(extracted_values, "try-error")) {
      for (i in seq_along(extracted_values)) {
        ev <- extracted_values[[i]]

        if (is.null(ev) || nrow(ev) == 0) {
          next
        }

        if (!is.null(weight_raster)) {
          total_weight <- sum(ev$coverage_area * ev$weight, na.rm = TRUE)

          if (total_weight <= 0) {
            next
          }

          for (class_idx in seq_along(class_values)) {
            class_val <- class_values[class_idx]
            class_name <- class_names[class_idx]

            class_rows <- ev$value == class_val

            if (any(class_rows, na.rm = TRUE)) {
              class_weight <- sum(ev$coverage_area[class_rows] * ev$weight[class_rows], na.rm = TRUE)
              year_results[i, class_name] <- round((class_weight / total_weight) * 100, 2)
            }
          }

          na_rows <- is.na(ev$value)
          if (any(na_rows)) {
            na_weight <- sum(ev$coverage_area[na_rows] * ev$weight[na_rows], na.rm = TRUE)
            year_results[i, "no_data"] <- round((na_weight / total_weight) * 100, 2)
          }
        } else {
          total_area <- sum(ev$coverage_area, na.rm = TRUE)

          if (total_area <= 0) {
            next
          }

          for (class_idx in seq_along(class_values)) {
            class_val <- class_values[class_idx]
            class_name <- class_names[class_idx]

            class_rows <- ev$value == class_val

            if (any(class_rows, na.rm = TRUE)) {
              class_area <- sum(ev$coverage_area[class_rows], na.rm = TRUE)
              year_results[i, class_name] <- round((class_area / total_area) * 100, 2)
            }
          }

          na_rows <- is.na(ev$value)
          if (any(na_rows)) {
            na_area <- sum(ev$coverage_area[na_rows], na.rm = TRUE)
            year_results[i, "no_data"] <- round((na_area / total_area) * 100, 2)
          }
        }
      }
    }

    year_results$year <- year
    results_list[[year]] <- year_results
  }

  if (length(results_list) == 0) {
    warning("No results generated. Returning empty dataset.")
    return(create_empty_result(sf_obj, start_date))
  }

  result_df <- do.call(rbind, results_list)
  final_result <- sf::st_sf(result_df, geometry = sf::st_geometry(sf_obj)[rep(1:nrow(sf_obj), length(results_list))])

  return(final_result)
}


#' Download vegetation index data
#'
#' This function download vegetation index data, either NDVI or EVI, based on start and end dates.
#'
#' @param start_date A character object with the start date; it must be specified as "yyyy-mm-dd"
#' @param end_date A character object with the end date; it must be specified as "yyyy-mm-dd"
#' @param indicator A character object with the vegetation index to be used; it must be either "NDVI" or "EVI".
#' @param shp_dt An object of class 'sf', which contains polygons or multipolygons representing the study area.
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only).
#' @param resolution A numeric, the resolution to be used in meters for extracting the vegetation index data.
#' @param grid_size A numeric, the grid size to be used to extract the data.
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey with latitude and longitude values (optional).
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lat A character, latitude variable from survey (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lon A character, longitude variable from survey (for STATA users only & if use survey is TRUE) (optional).
#' @param buffer_size A numeric, the buffer size to be used around each point in the survey data, in meters (optional).
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is "mean". Other options are "sum", "min", "max", "sd", "skew" and "rms" (optional).
#' @param survey_crs An integer, the Coordinate Reference System (CRS) for the survey data. Default is 4326 (WGS84) (optional).
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
#'
#'
#'
#' @return A processed data frame based on the input parameters and downloaded data.
#'
#' @import rstac terra raster dplyr osmdata sf httr geodata progress data.table
#' @importFrom data.table :=
#'
#' @examples
#' \dontrun{
#' \donttest{
#'
#'  #example usage
#' df <- geolink_vegindex(shp_dt = shp_dt[shp_dt$ADM1_EN ==  "Abia",],
#'                              start_date = "2019-01-01",
#'                              end_date = "2019-12-31",
#'                              indicator = "NDVI",
#'                              extract_fun = "mean",
#'                              buffer_size = 1000,
#'                              survey_crs = 4326)
#'
#'
#' }}
#'@export


geolink_vegindex <- function(
    start_date,
    end_date,
    indicator = "NDVI",
    shp_dt = NULL,
    shp_fn = NULL,
    resolution = NULL,
    grid_size = NULL,
    survey_dt = NULL,
    survey_fn = NULL,
    survey_lat = NULL,
    survey_lon = NULL,
    buffer_size = NULL,
    extract_fun = "mean",
    survey_crs = 4326,
    return_raster = FALSE,
    weight_raster = NULL
){

  if (indicator != "NDVI" & indicator != "EVI"){
    stop("Indicator must be either 'NDVI' or 'EVI'")
  }
  indicator_arg <- indicator
  indicator <- paste0("500m_16_days_", indicator)

  if (!is.null(shp_dt)) {
    sf_obj <- ensure_crs_4326(shp_dt)

  } else if (!is.null(survey_dt)) {
    sf_obj <- ensure_crs_4326(survey_dt)

  } else if (!is.null(shp_fn)) {
    sf_obj <- sf::read_sf(shp_fn)
    sf_obj <- ensure_crs_4326(sf_obj)

  } else if (!is.null(survey_fn)) {
    sf_obj <- zonalstats_prepsurvey(
      survey_dt = survey_dt,
      survey_fn = survey_fn,
      survey_lat = survey_lat,
      survey_lon = survey_lon,
      buffer_size = NULL,
      survey_crs = survey_crs)
    sf_obj <- ensure_crs_4326(sf_obj)

  } else {
    stop("Input a valid sf object or geosurvey")
    sf_obj <- NULL
  }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (as.numeric(format(start_date, "%Y"))<2001){
    stop("Start date must be 2001 or later.")
  }
  if (end_date>=as.Date(Sys.Date())){
    stop("End date must before today.")
  }
  if (end_date<start_date){
    stop("End date must be after start date.")
  }

  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  it_obj <- s_obj %>%
    stac_search(collections = "modis-13A1-061",
                bbox = sf::st_bbox(sf_obj),
                datetime = paste(start_date, end_date, sep = "/")) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())


  date_list <- lapply(1:length(it_obj$features),
                      function(x){

                        date <- cbind(as.numeric(format(as.Date(it_obj$features[[x]]$properties$end_datetime), "%Y")),
                                      as.numeric(format(as.Date(it_obj$features[[x]]$properties$end_datetime), "%m")),
                                      as.numeric(format(as.Date(it_obj$features[[x]]$properties$end_datetime), "%d")))
                        colnames(date) <- c("year", "month", "day")

                        return(date)
                      })

  date_list <- data.table::data.table(do.call(rbind, date_list))
  it_obj$features <- it_obj$features[date_list[,as.Date(paste0(year, "-", month, "-", day))]<end_date]
  date_list <- date_list[date_list[,as.Date(paste0(year, "-", month, "-", day))]<end_date]
  date_list$date <- as.Date(paste0(date_list$year, "-", date_list$month, "-", date_list$day))
  date_list$date <- format(date_list$date, "%Y-%m")
  date_list <- date_list[, id := seq_len(.N), by = .(year, month)]
  date_list <- date_list[, id := (day==min(day)), by = .(year, month)]
  it_obj$features <- it_obj$features[date_list$id==1]
  date_list <- date_list[date_list$id==1]

  features_todl <- lapply(1:nrow(date_list),
                          function(x){
                            feat <- c()
                            for (i in 1:length(it_obj$features)){
                              if ((as.numeric(format(as.Date(it_obj$features[[i]]$properties$end_datetime), "%Y")) == date_list[x, .(year)] &
                                   as.numeric(format(as.Date(it_obj$features[[i]]$properties$end_datetime), "%m")) == date_list[x, .(month)] &
                                   as.numeric(format(as.Date(it_obj$features[[i]]$properties$end_datetime), "%d")) == date_list[x, .(day)])==TRUE){
                                feat <- c(feat, i)
                              }
                            }
                            return(feat)

                          })


  url_list <- lapply(1:length(features_todl),
                     function(x){
                       url <- c()
                       for (i in features_todl[x][[1]]){
                         url <- c(url, paste0("/vsicurl/", it_obj$features[[i]]$assets[[indicator]]$href))
                       }
                       return(url)
                     })

  print("NDVI/EVI raster download started. This may take some time, especially for large areas.")

  unique_months <- unique(date_list$date)

  raster_objs <- c()
  for (i in 1:length(unique_months)){
    dls <- url_list[date_list[,date==unique_months[i]]]
    for (j in 1:length(unlist(dls))){
      if (j==1){
        rall <- terra::rast(unlist(dls)[[1]])
      } else{
        r <- terra::rast(unlist(dls)[[j]])
        rall <- terra::mosaic(r, rall, fun = "max")
      }
    }
    rall <- rall/100000000
    raster_objs <- c(raster_objs, rall)
    print(paste0("Month ", i, " of ", length(unique_months), " completed."))
  }

  raster_objs <- lapply(raster_objs, function(x) terra::project(x, "EPSG:4326"))

  raster_objs <- lapply(raster_objs, function(r) {
    if (inherits(r, "SpatRaster")) {
      raster::raster(r)
    } else {
      r
    }
  })

  unique_months <- as.Date(paste0(unique_months, "-01"))
  name_set <- paste0(tolower(indicator_arg), "_", "y", format(unique_months, "%Y"), "_m", format(unique_months, "%m"))

  print("NDVI Raster Downloaded")

  if (return_raster == TRUE && (!is.null(shp_dt) || !is.null(shp_fn))) {
    if (!is.null(shp_fn) && is.null(shp_dt)) {
      shp_for_crop <- sf::st_read(shp_fn)
    } else {
      shp_for_crop <- shp_dt
    }

    tryCatch({
      raster_crs <- raster::crs(raster_objs[[1]])
      shp_crs <- sf::st_crs(shp_for_crop)

      if (!is.na(raster_crs) && !is.na(shp_crs)) {
        if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
          message("Transforming shapefile to match raster CRS...")
          shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
        }
      }

      raster_ext <- raster::extent(raster_objs[[1]])
      shp_bbox <- sf::st_bbox(shp_for_crop)

      shp_ext <- raster::extent(shp_bbox[c("xmin", "xmax", "ymin", "ymax")])

      if (raster_ext@xmin < shp_ext@xmax &&
          raster_ext@xmax > shp_ext@xmin &&
          raster_ext@ymin < shp_ext@ymax &&
          raster_ext@ymax > shp_ext@ymin) {

        raster_objs <- lapply(raster_objs, function(r) {
          tryCatch({
            cropped <- raster::crop(r, shp_for_crop)
            raster::mask(cropped, shp_for_crop)
          }, error = function(e) {
            warning(paste("Error cropping raster:", e$message))
            return(r)
          })
        })
        message("Rasters successfully cropped to shapefile extent")
      } else {
        warning("Raster and shapefile extents do not overlap. Returning uncropped rasters.")
      }
    }, error = function(e) {
      warning(paste("Error in spatial processing:", e$message))
    })

    print("Process Complete!!!")
    return(raster_objs)
  }

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
                               name_set = name_set,
                               return_raster = return_raster,
                               weight_raster = weight_raster)

  print("Process Complete!!!")

  return(dt)
}

#' Download pollution data
#'
#' This function downloads pollution data at the monthly level. It allows for the extraction of five separate pollution indicators.
#'
#' @param start_date A character object with the start date; it must be specified as "yyyy-mm-dd"
#' @param end_date A character object with the end date; it must be specified as "yyyy-mm-dd"
#' @param indicator A character object with the pollution data to be used; it must be one of "aer-ai", "hcho", "no2", "o3", "so2".
#' @param shp_dt An object of class 'sf', which contains polygons or multipolygons representing the study area.
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only).
#' @param resolution A numeric, the resolution to be used in meters for extracting the vegetation index data.
#' @param grid_size A numeric, the grid size to be used to extract the data.
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey with latitude and longitude values (optional).
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lat A character, latitude variable from survey (for STATA users only & if use_survey is TRUE) (optional).
#' @param survey_lon A character, longitude variable from survey (for STATA users only & if use survey is TRUE) (optional).
#' @param buffer_size A numeric, the buffer size to be used around each point in the survey data, in meters (optional).
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is "mean". Other options are "sum", "min", "max", "sd", "skew" and "rms" (optional).
#' @param survey_crs An integer, the Coordinate Reference System (CRS) for the survey data. Default is 4326 (WGS84) (optional).
#' @param return_raster logical, default is FALSE, if TRUE a raster will be returned ONLY. The resulting
#' raster is cropped to the extent of `shp_dt` if `shp_dt` is specified. Otherwise, full raster from
#' source is downloaded.
#' @param weight_raster a raster object of class `spatRaster` or a list of `spatRaster` objects
#'
#'
#' @return A processed data frame based on the input parameters and downloaded data.
#'
#' @import rstac terra raster dplyr osmdata sf httr geodata progress data.table
#' @importFrom lubridate year month days day as_date
#'
#' @examples
#' \dontrun{
#' \donttest{
#'
#'  #example usage
#' df <- geolink_pollution(shp_dt = shp_dt[shp_dt$ADM1_EN ==  "Abia",],
#'                          start_date = "2018-06-01",
#'                          end_date = "2018-09-28",
#'                          indicator = "no2",
#'                          grid_size = 1000,
#'                          extract_fun = "mean")
#'
#'
#' }}
#'
#'@export

geolink_pollution <- function(
    start_date,
    end_date,
    indicator = NULL,
    shp_dt = NULL,
    shp_fn = NULL,
    resolution = NULL,
    grid_size = NULL,
    survey_dt = NULL,
    survey_fn = NULL,
    survey_lat = NULL,
    survey_lon = NULL,
    buffer_size = NULL,
    extract_fun = "mean",
    survey_crs = 4326,
    return_raster = FALSE,
    weight_raster = NULL
){

  if (is.null(indicator)==TRUE){
    stop("You must specify an indicator: aer-ai, hcho, no2, o3, so2")
  } else if ((indicator %in% c("aer-ai", "hcho", "no2", "o3", "so2"))==FALSE){
    stop("You must specify one of the following indicators: aer-ai, hcho, no2, o3, so2")
  }

  if (!is.null(shp_dt)) {
    sf_obj <- ensure_crs_4326(shp_dt)

  } else if (!is.null(survey_dt)) {
    sf_obj <- ensure_crs_4326(survey_dt)

  } else if (!is.null(shp_fn)) {
    sf_obj <- sf::read_sf(shp_fn)
    sf_obj <- ensure_crs_4326(sf_obj)

  } else if (!is.null(survey_fn)) {
    sf_obj <- zonalstats_prepsurvey(
      survey_dt = survey_dt,
      survey_fn = survey_fn,
      survey_lat = survey_lat,
      survey_lon = survey_lon,
      buffer_size = NULL,
      survey_crs = survey_crs)
    sf_obj <- ensure_crs_4326(sf_obj)

  } else {
    stop("Input a valid sf object or geosurvey")
    sf_obj <- NULL
  }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (start_date<as.Date("2018-05-01")){
    stop("Start date must be 2018-05-01 or later.")
  }
  if (end_date>=as.Date(Sys.Date())){
    stop("End date must before today.")
  }
  if (end_date<start_date){
    stop("End date must be after start date.")
  }

  allmonths <- seq.Date(start_date, end_date, by = "month")
  allmonths <- data.table::data.table(allmonths)
  allmonths <- allmonths[, .(year = year(allmonths), month = month(allmonths), day = 1)]

  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  print("Collection of monthly links started.")

  url_list <- c()
  bboxes <- c()
  valid_months <- c()

  for (x in 1:nrow(allmonths)){
    start_date_ind <- as.Date(paste0(allmonths[x, .(year)], "-", allmonths[x, .(month)], "-01"))
    end_date_ind <- start_date_ind + months(1) - days(1)
    it_obj <- s_obj %>%
      stac_search(collections = "sentinel-5p-l2-netcdf",
                  bbox = sf::st_bbox(sf_obj),
                  datetime = paste(start_date_ind, end_date_ind, sep = "/"),
                  limit = 1000) %>%
      get_request() %>%
      items_sign(sign_fn = sign_planetary_computer())

    features_month <- c()
    dates_month <- c()
    bboxes_month <- c()
    features <- c()
    for (i in 1:length(it_obj$features)){
      if (names(it_obj$features[[i]]$assets)==paste0(indicator)){
        if ((it_obj$features[[i]]$bbox[3]-it_obj$features[[i]]$bbox[1])==360 &
            (it_obj$features[[i]]$bbox[4]-it_obj$features[[i]]$bbox[2])>170){
          features_month <- c(features_month, paste0("/vsicurl/", it_obj$features[[i]]$assets[[indicator]]$href))
          dates_month <- c(dates_month, it_obj$features[[i]]$properties$datetime)
          bboxes_month <- c(bboxes_month, list(it_obj$features[[i]]$bbox))
          features <- c(features, list(it_obj$features[[i]]))
        }
      }
    }

    if (length(features_month) > 0) {
      features_month <- features_month[which(day(dates_month)==day(dates_month[which.max(day(dates_month))]))]
      bboxes_month <- bboxes_month[which(day(dates_month)==day(dates_month[which.max(day(dates_month))]))]
      features <- features[which(day(dates_month)==day(dates_month[which.max(day(dates_month))]))]

      url_list <- c(url_list, list(features_month))
      bboxes <- c(bboxes, list(bboxes_month))
      valid_months <- c(valid_months, x)
    } else {
      print(paste0("No data available for month ", x, " (", start_date_ind, "). Skipping."))
    }
  }

  print("Pollution raster download started. This may take some time.")

  layer <- dplyr::case_match(
    indicator,
    "aer-ai" ~ "aerosol_index_340_380",
    "ch4" ~ "methane_strong_twoband_total_column",
    "co" ~ "carbonmonoxide_total_column",
    "hcho" ~ "formaldehyde_tropospheric_vertical_column",
    "no2" ~ "nitrogendioxide_tropospheric_column",
    "o3" ~ "ozone_total_vertical_column",
    "so2" ~ "sulfurdioxide_total_vertical_column"
  )

  raster_objs <- vector("list", length(allmonths$year))

  for (i in 1:length(valid_months)) {
    tryCatch({
      month_idx <- valid_months[i]
      bb <- as.vector(bboxes[[i]][[1]])
      rall <- rast(url_list[[i]][[1]])

      if (is.null(rall) || nlyr(rall) == 0) {
        print(paste0("Invalid raster for month ", month_idx, ". Skipping."))
        raster_objs[[month_idx]] <- NULL
        next
      }

      if (!layer %in% names(rall)) {
        print(paste0("Layer '", layer, "' not found in raster for month ", month_idx, ". Available layers: ",
                     paste(names(rall), collapse=", "), ". Skipping."))
        raster_objs[[month_idx]] <- NULL
        next
      }

      tryCatch({
        ext(rall) <- c(bb[1], bb[3], bb[2], bb[4])
        crs(rall) <- "EPSG:4326"
        raster_objs[[month_idx]] <- rall[[layer]]
        print(paste0("Month ", month_idx, " of ", nrow(allmonths), " completed."))
      }, error = function(e) {
        print(paste0("Error setting extent/crs for month ", month_idx, ": ", e$message, ". Trying alternative approach."))
        tryCatch({
          raster_objs[[month_idx]] <- rall[[layer]]
          if (is.na(crs(raster_objs[[month_idx]]))) {
            crs(raster_objs[[month_idx]]) <- "EPSG:4326"
          }
          print(paste0("Month ", month_idx, " of ", nrow(allmonths), " completed with alternative approach."))
        }, error = function(e2) {
          print(paste0("Alternative approach also failed for month ", month_idx, ": ", e2$message, ". Skipping."))
          raster_objs[[month_idx]] <- NULL
        })
      })
    }, error = function(e) {
      print(paste0("Error processing month ", valid_months[i], ": ", e$message, ". Skipping."))
      raster_objs[[valid_months[i]]] <- NULL
    })
  }

  date_list <- as_date(paste0(allmonths$year, "-", allmonths$month, "-01"))
  name_set <- paste0(indicator, "_", "y", allmonths$year, "_m", allmonths$month)

  print("Pollution Rasters Downloaded")

  valid_indices <- which(!sapply(raster_objs, is.null))

  valid_rasters <- raster_objs[valid_indices]
  valid_names <- name_set[valid_indices]

  if (length(valid_rasters) == 0) {
    warning("No valid rasters were downloaded. Returning NULL.")
    return(NULL)
  }

  valid_rasters <- lapply(valid_rasters, function(r) {
    if (inherits(r, "SpatRaster")) {
      raster::raster(r)
    } else {
      r
    }
  })

  print(paste0("Processing ", length(valid_rasters), " valid rasters out of ", length(raster_objs), " months."))

  if (return_raster == TRUE && (!is.null(shp_dt) || !is.null(shp_fn))) {
    if (!is.null(shp_fn) && is.null(shp_dt)) {
      shp_for_crop <- sf::st_read(shp_fn)
    } else {
      shp_for_crop <- shp_dt
    }

    tryCatch({
      raster_crs <- raster::crs(valid_rasters[[1]])
      shp_crs <- sf::st_crs(shp_for_crop)

      if (!is.na(raster_crs) && !is.na(shp_crs)) {
        if (as.character(raster_crs) != as.character(shp_crs$proj4string)) {
          message("Transforming shapefile to match raster CRS...")
          shp_for_crop <- sf::st_transform(shp_for_crop, crs = raster_crs)
        }
      }

      raster_ext <- raster::extent(valid_rasters[[1]])
      shp_bbox <- sf::st_bbox(shp_for_crop)

      shp_ext <- raster::extent(shp_bbox[c("xmin", "xmax", "ymin", "ymax")])

      if (raster_ext@xmin < shp_ext@xmax &&
          raster_ext@xmax > shp_ext@xmin &&
          raster_ext@ymin < shp_ext@ymax &&
          raster_ext@ymax > shp_ext@ymin) {

        valid_rasters <- lapply(valid_rasters, function(r) {
          tryCatch({
            cropped <- raster::crop(r, shp_for_crop)
            raster::mask(cropped, shp_for_crop)
          }, error = function(e) {
            warning(paste("Error cropping raster:", e$message))
            return(r)
          })
        })
        message("Rasters successfully cropped to shapefile extent")
      } else {
        warning("Raster and shapefile extents do not overlap. Returning uncropped rasters.")
      }
    }, error = function(e) {
      warning(paste("Error in spatial processing:", e$message))
    })

    print("Process Complete!!!")
    return(valid_rasters)
  }

  dt <- postdownload_processor(shp_dt = shp_dt,
                               raster_objs = valid_rasters,
                               shp_fn = shp_fn,
                               grid_size = grid_size,
                               survey_dt = survey_dt,
                               survey_fn = survey_fn,
                               survey_lat = survey_lat,
                               survey_lon = survey_lon,
                               extract_fun = extract_fun,
                               buffer_size = buffer_size,
                               survey_crs = survey_crs,
                               name_set = valid_names,
                               return_raster = return_raster,
                               weight_raster = weight_raster)

  if (length(valid_months) < nrow(allmonths)) {
    missing_months <- setdiff(1:nrow(allmonths), valid_months)
    missing_names <- name_set[missing_months]

    for (missing_name in missing_names) {
      dt[[missing_name]] <- NA
    }

    print(paste0("Added NA values for ", length(missing_months), " missing months."))
  }

  month_pattern <- paste0("^", indicator, "_")
  month_cols <- names(dt)[grepl(month_pattern, names(dt))]

  if (length(month_cols) > 0) {
    year_month <- gsub(paste0("^", indicator, "_y(\\d+)_m(\\d+)$"), "\\1-\\2", month_cols)
    sort_keys <- paste0(year_month, "-01")

    sorted_indices <- order(sort_keys)
    sorted_month_cols <- month_cols[sorted_indices]

    non_month_cols <- setdiff(names(dt), month_cols)

    dt <- dt[, c(non_month_cols, sorted_month_cols), with = FALSE]
  }

  print("Process Complete!!!")

  return(dt)
}
