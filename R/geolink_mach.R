#' Download and Merge monthly rainfall chirp data into geocoded surveys
#'
#' Download rainfall data from the CHIRPS data at monthly/annual intervals for a specified period
#' The data is downloaded in raster format and combined with shapefile and/or survey data provided
#' by the user
#'
#' @param time_unit A character, either "month" or "year" monthly or annual rainfall aggregates
#' are to be estimated
#' @param start_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param end_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only)
#' @param grid logical, TRUE if shapefile needs to tesselated/gridded before estimating grid level
#' zonal statistics and performing a spatial join to the household survey
#' @param grid_size A numeric, the grid size to be used in meters
#' @param use_survey logical, if TRUE a survey is expected (i.e. survey_dt cannot be null)
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey i.e.
#' a household survey with latitude and longitude values.
#' @param survey_fn A character, file path for geocoded survey (.dta format) (for STATA users only &
#' if use_survey is TRUE)
#' @param survey_lat A character, latitude variable from survey (for STATA users only &
#' if use_survey is TRUE)
#' @param survey_lon A character, longitude variable from survey (for STATA users only &
#' if use survey is TRUE)
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is mean. Other options are "sum", "min", "max", "sd", "skew" and "rms".
#'
#' @details Rainfall data is sourced from the Climate Hazards Group InfraRed Precipitation
#' with Station data (CHIRPS). This a 35+ year quasi-global rainfall dataset. Spanning the
#' entire world from 1981 to date. See https://www.chc.ucsb.edu/data/chirps for more details.
#' The data is extracted into a shapefile provided by user. An added service for tesselating/gridding
#' the shapefile is also provided for users that need this data further analytics that require
#' equal area zonal statistics. Shapefile estimates at the grid or native polygon level is a
#' permitted final output. However, a geocoded survey with rainfall estimates are the end goal
#' if the user also choooses. The function will merge shapefile polygons (either gridded or
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
#' #loading the survey data and shapefile
#'
#' data("hhgeo_dt")
#' data("shp_dt")
#'
#' #quick example
#' df <- geolink_chirps(time_unit = "month",
#'                      start_date = "2020-01-01",
#'                      end_date = "2020-03-01",
#'                      shp_dt = shp_dt,
#'                      grid = TRUE,
#'                      grid_size = 1000,
#'                      use_survey = TRUE,
#'                      survey_dt = hhgeo_dt,
#'                      extract_fun = "mean")
#'
#' }
#'
#' @export
#' @import data.table parallel raster
#' @importFrom haven read_dta
#' @importFrom crsuggest suggest_crs



geolink_chirps <- function(time_unit,
                           start_date,
                           end_date,
                           shp_dt,
                           shp_fn = NULL,
                           grid = FALSE,
                           grid_size = 1000,
                           use_survey = TRUE,
                           survey_dt,
                           survey_fn = NULL,
                           survey_lat = NULL,
                           survey_lon = NULL,
                           extract_fun = "mean") {


  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  ## download the data via readlines seeking inputs
  if (time_unit == "month") {

    raster_objs <- get_month_chirps(start_date = as.Date(start_date),
                                    end_date = as.Date(end_date))

    name_count <- lubridate::interval(as.Date(start_date),
                                      as.Date(end_date)) %/% months(1) + 1

  } else if (time_unit == "year") {

    raster_objs <- get_annual_chirps(start_year = lubridate::year(start_date),
                                     end_year = lubridate::year(end_date))

    name_count <- lubridate::year(start_date) - lubridate::year(end_date) + 1

  } else {

    stop("Time unit should either be month or year")

  }

  print("Global Rainfall Raster Downloaded")

  ## crop the rasters to size of area
  if (time_unit == "month") {

    raster_objs <- lapply(raster_objs,
                          function(X) {

                            raster_crop <- raster::crop(x = X,
                                                        y = extent(shp_dt))

                            return(raster_crop)

                          })

  }

  print("Rainfall raster cropped to extent of the country")
  ## create the name for the variables

  name_set <- paste0(time_unit, 1:name_count)

  if (is.null(shp_fn) == FALSE){

    shp_dt <- sf::read_sf(shp_fn)

  }


  if (grid == TRUE) {

    #### check if the shapefile is a UTM or degree CRS projection
    crs_obj <- st_crs(shp_dt)

    if (!("m" %in% crs_obj$units)) {

      suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                           units = "m")

      shp_dt <- st_transform(shp_dt,
                             crs = as.numeric(suggest_dt$crs_code[1]))

    }

    shp_dt <- gengrid2(shp_dt = shp_dt,
                       grid_size = grid_size)

    shp_dt <- st_transform(shp_dt, crs = crs_obj)

  }

  ## extract into the shapefile with different column names
  if (!identical(crs_obj, raster::crs(raster_objs[[1]]))){

    shp_dt <- st_transform(shp_dt, crs = st_crs(raster_objs[[1]])$input)

  }

  ### reproject shapefile to match raster CRS if they are not the same
  print("Extracting rainfall data into shapefile")
  shp_dt <-
    mapply(FUN = function(x, n){
      ### first ensure raster and shapefile have the same crs


      shp_dt[[n]] <- exactextractr::exact_extract(x = x,
                                                  y = shp_dt,
                                                  fun = extract_fun)

      return(shp_dt)

  },
  SIMPLIFY = FALSE,
  x = raster_objs,
  n = name_set)

  if (length(shp_dt) > 1L) {

    shp_dt <- lapply(shp_dt,
                     function(X){

                       X$geoID <- 1:nrow(X)

                       return(X)

                     })

    geoid_dt <- shp_dt[[1]][, c("geoID")]

    shp_crs <- st_crs(shp_dt[[1]])$input

    shp_dt <- lapply(shp_dt,
                     function(X){

                       X <- X %>% st_drop_geometry()

                       return(X)

                     })

    shp_dt <- Reduce(merge, shp_dt)

    shp_dt <- merge(shp_dt, geoid_dt, by = "geoID")

    shp_dt <- st_as_sf(shp_dt, crs = shp_crs, agr = "constant")

  }


  ## merge the shapefile and the household survey
  if (use_survey == TRUE){

    print("Merging shapefile rainfall estimates into survey")

    if (is.null(survey_fn) == FALSE){

      survey_dt <- haven::read_dta(survey_fn)

      survey_dt <- st_as_sf(survey_dt,
                            coords = c(survey_lon, survey_lat),
                            crs = 4326,
                            agr = "constant")
    }

    crs_raster <- crs(raster_objs[[1]])

    survey_dt <- st_as_sf(x = survey_dt,
                          crs = crs_raster,
                          agr = "constant")

    survey_dt <- st_join(survey_dt, shp_dt)

    if (is.null(survey_fn) == FALSE){

      survey_dt <- st_drop_geometry(survey_dt) ## remove geometry for STATA output

      return(survey_dt)

    }

    return(survey_dt)

  }

  print("Process Complete!")
  return(shp_dt)

}



















