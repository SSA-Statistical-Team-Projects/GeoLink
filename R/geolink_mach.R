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
#' #loading the survey data and shapefile
#'
#' data("hhgeo_dt")
#' data("shp_dt")
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



geolink_chirps <- function(time_unit,
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
#' Default is mean. Other options are "sum", "min", "max", "sd", "skew" and "rms".
#' @param month_version A character, the version of month EOG data to use. default set to "v10".
#' @param annual_version A character, the version of annual EOG data for download, default set to "v21"
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
#' df <- geolink_ntl(time_unit = "month",
#'                   start_date = "2020-01-01",
#'                   end_date = "2020-03-01",
#'                   shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'                   version = "v21",
#'                   indicator = "average_masked",
#'                   grid_size = 1000,
#'                   survey_dt = st_as_sf(hhgeo_dt[hhgeo_dt$ADM1_EN == "Abia",], crs = 4326),
#'                   extract_fun = "mean")
#'
#' #estimate annual night time luminosity for each household within a 100 meters
#' #of it's location
#'
#' df <- geolink_ntl(time_unit = "annual",
#'                   start_date = "2020-01-01",
#'                   end_date = "2020-03-01",
#'                   shp_dt = NULL,
#'                   version = "v21",
#'                   indicator = "average_masked",
#'                   survey_dt = st_as_sf(hhgeo_dt[hhgeo_dt$ADM1_EN == "Abia",], crs = 4326),
#'                   extract_fun = "mean",
#'                   buffer_size = 100)
#'
#'
#'
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
                        survey_crs = 4326){

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
                          indicator = indicator)

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


}



#' Download and Merge Annual Land Use Land Cover data into geocoded surveys
#'
#' Download Land Use Land Cover data from the LULC dataset at annual intervals for a specified period
#' The data is downloaded in raster format and combined with shapefile and/or survey data provided
#' by the user. Source data: https://planetarycomputer.microsoft.com/dataset/io-lulc-annual-v02
#'
#' @param time_unit A character, must be annual as the dataset only provides annual data
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
#' @param buffer_survey A logical, specify TRUE if interested in estimating a statistic based on distance
#' from the survey location.
#' @param extract_fun A character, a function to be applied in extraction of raster into the shapefile.
#' Default is mean. Other options are "sum", "min", "max", "sd", "skew" and "rms".
#' @param survey_crs A numeric, the default is 4326
#'
#' @details LULC data is sourced from Microsoft Planetary Computer.
#' The data is extracted into a shapefile provided by user. An added service for tesselating/gridding
#' the shapefile is also provided for users that need this data further analytics that require
#' equal area zonal statistics. Shapefile estimates at the grid or native polygon level is a
#' permitted final output. However, a geocoded survey with land use land cover estimates are the end goal
#' if the user also chooses. The function will merge shapefile polygons (either gridded or
#' native polygons) with the location of the survey units i.e. land use land cover estimates for the
#' locations of the units within the survey will be returned. The function is also set up for
#' stata users and allows the user to pass file paths for the household survey `survey_fn`
#' (with the lat and long variable names `survey_lon` and `survey_lat`) as well. This is requires
#' a .dta file which is read in with `haven::read_dta()` package. Likewise, the user is permitted
#' to pass a filepath for the location of the shapefile `shp_fn` which is read in with the
#' `sf::read_sf()` function.
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
#'df <- geolink_landcover(time_unit,
#                         start_date = "2020-01-01",
#                         end_date = "2021-01-01",
#                         shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#                         grid_size = 1000,
#                         survey_dt = st_as_sf(hhgeo_dt[hhgeo_dt$ADM1_EN == "Abia",],
#                         extract_fun = "mean")
#'
#'
#' }
#'
#' @import  rstac reticulate terra raster osmdata sf geodata httr ncdf4  exactextractr parallel
#'
#'
#'
#'

geolink_landcover <- function(time_unit = "annual",
                              start_date,
                              end_date,
                              shp_dt) {


  # Convert dates
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # STAC search
  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  it_obj <- s_obj %>%
    stac_search(
      collections = "io-lulc-annual-v02",
      bbox = sf::st_bbox(shp_dt),
      datetime = paste(start_date, end_date, sep = "/")
    ) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  # Filter out features with problematic bounding boxes
  filter_features <- function(feature) {
    bbox <- feature$bbox
    !any(bbox %in% c(180, -180))
  }
  it_obj$features <- it_obj$features[sapply(it_obj$features, filter_features)]

  # Check if any features are found
  if (length(it_obj$features) == 0) {
    stop("No suitable raster features found for the given date range and shapefile.")
  }

  # Temporary directory for downloads
  temp_dir <- tempdir()

  # Download and organize rasters
  raster_year_map <- list()

  for (i in seq_along(it_obj$features)) {
    if (is.null(it_obj$features[[i]]$assets$data$href)) {
      next
    }

    year <- format(as.Date(it_obj$features[[i]]$properties$start_datetime), "%Y")
    url <- it_obj$features[[i]]$assets$data$href
    raster_path <- file.path(temp_dir, paste0(year, "_", i, "_raster.tif"))

    tryCatch({
      download.file(url, raster_path, mode = "wb")

      if (!year %in% names(raster_year_map)) {
        raster_year_map[[year]] <- list()
      }
      raster_year_map[[year]] <- c(raster_year_map[[year]], raster_path)
    }, error = function(e) {
      warning(paste("Could not download raster for year", year, ":", e$message))
    })
  }

  # Integrated mosaic_rasters function
  mosaic_rasters <- function(raster_paths, output_crs = "EPSG:4326", mosaic_fun = "mean") {
    if (length(raster_paths) < 1) {
      stop("Provide at least one raster path for mosaicking.")
    }

    # Read all rasters and align them
    raster_list <- lapply(raster_paths, function(path) {
      r <- terra::rast(path)
      return(r)
    })

    # Ensure consistent CRS
    raster_list <- lapply(raster_list, function(r) {
      terra::project(r, output_crs)
    })

    # Align rasters to a common resolution and extent
    if (length(raster_list) == 1) {
      return(raster_list[[1]])
    }

    aligned_rasters <- terra::mosaic(raster_list[[1]], raster_list[[2]], fun = mosaic_fun)

    if (length(raster_list) > 2) {
      for (i in 3:length(raster_list)) {
        tryCatch({
          aligned_rasters <- terra::mosaic(aligned_rasters, raster_list[[i]], fun = mosaic_fun)
        }, error = function(e) {
          warning(paste("Could not mosaic raster:", raster_paths[[i]], "Attempting merge"))
          aligned_rasters <- terra::merge(aligned_rasters, raster_list[[i]])
        })
      }
    }

    return(aligned_rasters)
  }

  # Mosaic rasters for each year
  mosaicked_rasters <- lapply(names(raster_year_map), function(year) {
    cat("Mosaicking rasters for year:", year, "\n")
    mosaic_rasters(raster_year_map[[year]])
  })
  names(mosaicked_rasters) <- names(raster_year_map)

  # Transform shapefile
  projected_shapefile <- sf::st_transform(shp_dt, crs = sf::st_crs("EPSG:4326"))

  # Crop rasters
  cropped_rasters <- lapply(mosaicked_rasters, function(raster_obj) {
    terra::crop(raster_obj, terra::ext(projected_shapefile))
  })

  # Extract land cover proportions
  file_values <- it_obj$features[[1]]$assets$data$`file:values`
  class_values <- unlist(lapply(file_values, `[[`, "values"))
  class_names <- unlist(lapply(file_values, `[[`, "summary"))

  proportions_list <- lapply(cropped_rasters, function(raster) {
    extracted_values <- exactextractr::exact_extract(raster, projected_shapefile, coverage_area = TRUE)
    total_area <- sum(sapply(extracted_values, function(ev) sum(ev$coverage_area, na.rm = TRUE)))

    if (total_area == 0) return(NULL)

    class_proportions <- sapply(class_values, function(class_val) {
      class_area <- sum(sapply(extracted_values, function(ev) {
        sum(ev$coverage_area[ev$value == class_val], na.rm = TRUE)
      }))
      (class_area / total_area) * 100
    })

    class_proportions
  })

  # Remove NULL entries and create dataframe
  proportions_list <- proportions_list[!sapply(proportions_list, is.null)]
  proportions_df <- do.call(rbind, proportions_list)
  colnames(proportions_df) <- class_names

  return(proportions_df)
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
#' @param buffer_survey A logical, specify TRUE if interested in estimating a statistic based on distance
#' from the survey location.
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
#'df <- geolink_population(iso_code = "NGA",
#'                         UN_adjst = "N",
#'                         constrained = "Y",
#'                         shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'                         grid_size = 1000,
#'                         extract_fun = "mean")
#'}
#'

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
}


#' Download points of interest from OSM data using open street maps API.
#'
#' @param osm_feature_category A character, refering to the osm key wiki page, please see details below
#' @param osm_feature_subcategory A character, refering to the osm key wiki page, please see details below
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons
#' @param shp_dsn A link to location of shapefile for stat users
#' @param buffer buffer area around shapefile
#' @param stata A flag for stata users
#'
#' @details
#'
#' Details for feature category and sub-category can be found here: https://wiki.openstreetmap.org/wiki/Map_features
#'
#' @import rstac terra raster osmdata  sf httr geodata
#'
#' @examples
#'\donttest{
#'
#'
#'
#' df <- geolink_get_poi(osm_feature_category = "building",
#' osm_feature_subcategory ="farm",
#' shp_dt = shp_dt)
#'
#'}
#'

geolink_get_poi <- function(osm_feature_category,
                            osm_feature_subcategory,
                            shp_dt = NULL,
                            shp_dsn = NULL,
                            buffer = NULL,
                            stata = FALSE){

  shp_dt <- ensure_crs_4326(shp_dt)

  if (!is.null(shp_dsn)) {
    shp_dt <- st_read(shp_dsn)
  }


  bbox <- create_query_bbox(shp_dt = shp_dt,
                            area_name = NULL,
                            buffer_dist = c(0, 0, 0, 0),
                            metric_crs = FALSE,
                            osm_crs = 4326)

  datapull <- opq(c(bbox = bbox, timeout = 7200)) %>%
    add_osm_feature(osm_feature_category, osm_feature_subcategory)

  features <- osmdata_sf(datapull)


  if (nrow(features$osm_points) == 0) {
    print("No points of interest")
    return()
  } else {
    results <- (features$osm_points)
  }

  query_dt <- st_join(results, shp_dt)

  if (stata) {

    query_dt <- query_dt[, !grepl("geometry", names(query_dt))]
  }




  print("Open Street Maps Raster Downloaded")



  print("Process Complete!!!")

  return(query_dt)}


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
#'
#' @details
#'
#' Details for the dataset can be found here: https://hrea.isr.umich.edu/
#'
#' @import rstac terra raster osmdata  sf httr geodata
#'
#' @examples
#'\donttest{
#'
#'
#' df = geolink_electaccess(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'                        start_date = "2018-12-31", end_date = "2019-12-31")
#'
#' }
#'


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
    survey_crs = 4326
) {

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  it_obj <- s_obj %>%
    stac_search(
      collections = "hrea",
      bbox = sf::st_bbox(shp_dt),
      datetime = paste(start_date, end_date, sep = "/")
    ) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  # Modify url_list extraction
  url_list <- lapply(1:length(it_obj$features), function(x) {
    urls <- list(
      lightscore = paste0("/vsicurl/", it_obj$features[[x]]$assets$lightscore$href),
      light_composite = paste0("/vsicurl/", it_obj$features[[x]]$assets$`light-composite`$href),
      night_proportion = paste0("/vsicurl/", it_obj$features[[x]]$assets$`night-proportion`$href),
      estimated_brightness = paste0("/vsicurl/", it_obj$features[[x]]$assets$`estimated-brightness`$href)
    )
    return(urls)
  })

  raster_objs <- lapply(url_list, function(urls) {
    # Convert each URL to a SpatRaster
    lapply(urls, function(url) {
      tryCatch({
        terra::rast(url)
      }, error = function(e) {
        warning(paste("Could not convert URL to SpatRaster:", url, "Error:", e$message))
        return(NULL)
      })
    })
  })

  # Flatten the list of lists to a single list of 8 SpatRasters
  raster_objs <- unlist(raster_objs, recursive = FALSE)


  # Create name_set with all 4 indicators for each year
  year_sequence <- seq(lubridate::year(start_date), lubridate::year(end_date))

  # Generate name_set with all indicators
  name_set <- unlist(lapply(year_sequence, function(year) {
    c(
      paste0("lightscore_", year),
      paste0("light_composite_", year),
      paste0("night_proportion_", year),
      paste0("estimated_brightness_", year)
    )
  }))



  print("Electrification Access Raster Downloaded")

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

  print("Process Complete!!!")

  return(dt)
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
#' # Example usage with shapefile
#' df <- geolink_elevation(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#'
#' # Example usage with file path
#' df <- geolink_elevation(shp_fn = "path/to/shapefile.shp")
#' }
#'

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
}


#' Download high resolution building data from WorldPop
#'
#' This function downloads high-resolution building data from WorldPop based on the specified version and ISO country code.
#'  It can incorporate survey data for further analysis. The building data is downloaded as raster files and can be processed and
#'  analyzed using the `postdownload_processor` function.
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
#' # Example usage with version 1.1
#'df <- geolink_buildings(version = "v1.1",
#'                             iso_code = "NGA",
#'                           shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
#'                           indicators = "ALL",
#'                             grid_size = 1000)
#' }
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
}

#' Download CMIP6 climate model data
#'
#' This function downloads CMIP6 (Coupled Model Intercomparison Project Phase 6) climate model data for a specific variable, resolution, model, Shared Socioeconomic Pathway (SSP), and time period. It allows for further analysis of the data in conjunction with geographic data.
#'
#' @param var A character, the variable of interest (e.g., "temperature", "precipitation").
#' @param res A character, the resolution of the data (e.g., "2.5m", "5m").
#' @param model A character, the climate model name (e.g., "ACCESS-ESM1-5", "CanESM5").
#' @param ssp A character, the Shared Socioeconomic Pathway (SSP) scenario (e.g., "ssp126", "ssp585").
#' @param time A character, the time period of interest (e.g., "historical", "2020-2049").
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
#' @return A processed data frame or object based on the input parameters and downloaded data.
#'
#' @importFrom httr GET http_type write_disk
#' @import rstac terra raster osmdata sf httr geodata progress
#'
#' @examples
#' \donttest{
#'
#' # Example usage
#' df <- geolink_CMIP6(start_date = "2019-01-01", end_date = "2019-12-31",
#'                       scenario = "ssp245", desired_models = "UKESM1-0-LL",
#'                        shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#'
#'
#' }
#'

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
    sf_obj <- haven::read_dta(survey_fn)
    sf_obj <- st_as_sf(sf_obj,
                       coords = c(survey_lon, survey_lat),
                       crs = survey_crs)
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
#' @import rstac terra raster osmdata sf httr geodata
#'
#' @examples
#' \donttest{
#'
#' # Example usage
#' df <- geolink_cropland(source = "WorldCover",
#'                        shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#' }
#'

geolink_cropland <- function(source = "WorldCover",
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
  unlink(tempdir(), recursive = TRUE)

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

  return(df)}

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
#' # Example usage
#' df <- geolink_worldclim(iso_code = "NGA", var = "temperature", res = "2.5m", shp_dt = shp_dt)
#' }
#'
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

  return(dt)}

#' Download OpenCellID data
#'
#' This function downloads OpenCellID data, which provides information about cell towers and their coverage areas. It allows for further analysis of cellular network coverage in a given area.
#'
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons representing the study area.
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only).
#' @param grid_size_meters A numeric, the grid size to be used in meters for analyzing the cell tower data. The maximum possible is 2000 meters.
#' @param key A character, the API key created in when signing up to Opencellid profile.
#'
#' @return A processed data frame or object based on the input parameters and downloaded data.
#'
#' @importFrom terra rast
#' @importFrom httr GET timeout
#' @import rstac terra raster osmdata sf httr geodata data.table geosphere
#'
#' @examples
#' \donttest{
#'
#' # Example usage
#' results <- geolink_opencellid(cell_tower_file = "C:/Users/username/Downloads/621.csv.gz",
#'                              shapefile_input = shp_dt)
#' }
#'


# Combined function to calculate tower stats and return the nearest lat/lon for a polygon
geolink_opencellid <- function(cell_tower_file, shapefile_input) {

  shp_dt <- ensure_crs_4326(shp_dt)

  # Load the OpenCellID data
  cell_towers <- read_opencellid_data(cell_tower_file)

  # Check if shapefile_input is a file path (character) or an in-memory sf object
  if (is.character(shapefile_input)) {
    # Load shapefile if it's a file path
    if (!file.exists(shapefile_input)) {
      stop("Shapefile not found at the specified path")
    }
    polygons <- st_read(shapefile_input)
  } else if (inherits(shapefile_input, "sf")) {
    # Use the sf object directly
    polygons <- shapefile_input
  } else {
    stop("Invalid shapefile input: must be a file path or an sf object.")
  }

  # Ensure CRS matches between towers and polygons
  cell_towers_sf <- st_as_sf(cell_towers, coords = c("lon", "lat"), crs = 4326)
  cell_towers_sf <- st_transform(cell_towers_sf, st_crs(polygons))

  # Create a list to store results
  results <- list()

  # Loop through each polygon to calculate stats
  for (i in 1:nrow(polygons)) {
    polygon <- polygons[i, ]

    # Towers within the polygon
    towers_in_polygon <- st_within(cell_towers_sf, polygon, sparse = FALSE)
    num_towers <- sum(towers_in_polygon)

    # Calculate centroid of the polygon
    centroid <- st_centroid(polygon)

    # Calculate nearest tower distance
    if (num_towers > 0) {
      towers_sf <- cell_towers_sf[towers_in_polygon, ]
      distances <- st_distance(centroid, towers_sf, by_element = FALSE)
      nearest_idx <- which.min(distances)
      nearest_distance <- min(distances)

      nearest_lon <- st_coordinates(towers_sf)[nearest_idx, "X"]
      nearest_lat <- st_coordinates(towers_sf)[nearest_idx, "Y"]
    } else {
      nearest_distance <- NA
      nearest_lon <- NA
      nearest_lat <- NA
    }

    # Store results
    results[[i]] <- data.frame(
      polygon_id = i,
      num_towers = num_towers,
      nearest_distance = nearest_distance,
      nearest_lon = nearest_lon,
      nearest_lat = nearest_lat
    )
  }

  # Combine all results into a data frame
  results_df <- do.call(rbind, results)

  return(results_df)
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
#' @import rstac reticulate  terra  raster  osmdata  sf  geodata  httr  ncdf4
#'
#' @examples
#' \donttest{
#'
#' # Example usage
#' df <- geolink_terraclimate(var ="tmin", year = 2017, shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#' }
#'

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

  unlink(tempdir(), recursive = TRUE)

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
  } else {
    # Print the error status
    print(paste("Error downloading the file. Status code:", http_status(response)$status_code))
    return(NULL)
  }
}






#' Download and Merge monthly NDVI into geocoded surveys
#'
#' This function downloads MODIS NDVI or EVI data for a specific region and time period. Please note that, due to the way the data is stored, this function can take a long time to complete.
#'
#' @param time_unit A character, the time unit for the data. Default is "monthly". For now, no other time unit is supported.
#' @param start_date An object of class date, this indicates the start time for which the indicator will be pulled.
#' @param end_date An object of class date, this indicates the end time for which the indicator will be pulled.
#' @param indicator A character, the indicator of interest. Default is "NDVI". Other options are "EVI".
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons representing the study area.
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only).
#' @param grid_size A numeric, the grid size to be used in meters for pulling the vegetation data.
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
#' # Example usage
#' df <- geolink_terraclimate(var ="tmin",
#'                            year = 2017,
#'                            shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#' }
#' @export
#'

geolink_vegindex <- function(time_unit = "monthly",
                             start_date,
                             end_date,
                             indicator = "NDVI",
                             shp_dt,
                             shp_fn = NULL,
                             grid_size = NULL,
                             survey_dt,
                             survey_fn = NULL,
                             survey_lat = NULL,
                             survey_lon = NULL,
                             buffer_size = NULL,
                             extract_fun = "mean",
                             survey_crs = 4326){

  if (indicator != "NDVI" & indicator != "EVI"){
    stop("Indicator must be either 'NDVI' or 'EVI'")
  }
  indicator <- paste0("500m_16_days_", indicator)

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)


  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  it_obj <- s_obj %>%
    stac_search(collections = "modis-13A1-061",
                bbox = sf::st_bbox(shp_dt),
                datetime = paste(start_date, end_date, sep = "/")) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())


  date_list <- lapply(1:length(it_obj$features),
                      function(x){

                        date <- cbind(year(it_obj$features[[x]]$properties$end_datetime),
                                      month(it_obj$features[[x]]$properties$end_datetime),
                                      lubridate::day(it_obj$features[[x]]$properties$end_datetime))
                        colnames(date) <- c("year", "month", "day")

                        return(date)

                      })
  date_list <- data.table::data.table(do.call(rbind, date_list))
  date_list <- date_list[, .SD[1], by = .(year, month)]

  features_todl <- lapply(1:nrow(date_list),
                          function(x){

                            feat <- c()
                            for (i in 1:length(it_obj$features)){
                              if ((year(it_obj$features[[i]]$properties$end_datetime) == date_list[x, .(year)] &
                                   month(it_obj$features[[i]]$properties$end_datetime) == date_list[x, .(month)] &
                                   lubridate::day(it_obj$features[[i]]$properties$end_datetime) == date_list[x, .(day)])==TRUE){
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

  print("NDVI/EVI raster download started. This may take some time.")

  raster_objs <- c()
  num <- 1
  for (x in url_list){
    rall <- terra::rast(x[[1]])
    for (i in 2:length(x)){
      r <- terra::rast(x[[i]])
      rall <- terra::mosaic(r, rall, fun = "max")
    }
    raster::crs(rall) <- "EPSG:3857"
    rall <- project(rall, crs(shp_dt))
    rall <- rall/100000000
    raster_objs <- c(raster_objs, rall)
    print(paste0("Month ", num, " of ", length(url_list), " completed."))
    num <- num + 1
  }

  # raster_objs <- lapply(url_list,
  #                       function(x){
  #                         rall <- terra::rast(x[[1]])
  #                         for (i in 2:length(x)){
  #                           r <- terra::rast(x[[i]])
  #                           rall <- terra::mosaic(r, rall, fun = "mean")
  #                         }
  #                         raster::crs(rall) <- "EPSG:3857"
  #                         rall <- project(rall, crs(shp_dt))
  #                         rall <- rall/100000000
  #                         return(rall)
  #                       })



  name_set <- paste0("ndvi_", "y", date_list$year, "_m", date_list$month)

  print("NDVI Raster Downloaded")

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







#' This function downloads monthly pollution data from Sentinel for specific areas and time periods.
#'
#' @param time_unit A character, the time unit for the data. Default is "monthly". For now, no other time unit is supported.
#' @param start_date An object of class date, this indicates the start time for which the indicator will be pulled.
#' @param end_date An object of class date, this indicates the end time for which the indicator will be pulled.
#' @param indicator A character, the indicator of interest. Indicator must be one of aer-ai, ch4, co, hcho, no2, o3, or so2.
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons representing the study area.
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only).
#' @param grid_size A numeric, the grid size to be used in meters for pulling the vegetation data.
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
#' # Example usage
#' df <- geolink_terraclimate(var ="tmin",
#'                            year = 2017,
#'                            shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#' }
#' @export
#'
geolink_pollution <- function(time_unit = "monthly",
                              start_date,
                              end_date,
                              indicator,
                              shp_dt,
                              shp_fn = NULL,
                              grid_size = NULL,
                              survey_dt,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              extract_fun = "mean",
                              survey_crs = 4326){

  # checks
  if (missing(indicator)==TRUE){
    print("You must specify an indicator: aer-ai, ch4, co, hcho, no2, o3, so2")
  }
  if ((indicator %in% c("aer-ai", "ch4", "co", "hcho", "no2", "o3", "so2"))==FALSE){
    print("You must specify one of the following indicators: aer-ai, ch4, co, hcho, no2, o3, so2")
  }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # get the months we need
  allmonths <- seq.Date(start_date, end_date, by = "month")
  allmonths <- data.table::data.table(allmonths)
  allmonths <- allmonths[, .(year = year(allmonths), month = month(allmonths), day = 1)]

  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")


  print("Collection of monthly links started.")

  # this will collect the features we need to download for the variable of interest
  # all months
  url_list <- c()
  bboxes <- c()
  for (x in 1:nrow(allmonths)){
    start_date_ind <- as.Date(paste0(allmonths[x, .(year)], "-", allmonths[x, .(month)], "-01"))
    end_date_ind <- start_date_ind + months(1) - days(1)
    it_obj <- s_obj %>%3
    stac_search(collections = "sentinel-5p-l2-netcdf",
                bbox = sf::st_bbox(shp_dt),
                datetime = paste(start_date_ind, end_date_ind, sep = "/"),
                limit = 1000) %>%
      get_request() %>%
      items_sign(sign_fn = sign_planetary_computer())

    features_month <- c()
    dates_month <- c()
    bboxes_month <- c()
    for (i in 1:length(it_obj$features)){
      if (names(it_obj$features[[i]]$assets)==paste0(indicator)){
        features_month <- c(features_month, paste0("/vsicurl/", it_obj$features[[i]]$assets[[indicator]]$href))
        dates_month <- c(dates_month, it_obj$features[[i]]$properties$datetime)
        bboxes_month <- c(bboxes_month, list(it_obj$features[[i]]$bbox))
      }
    }
    features_month <- features_month[which(lubridate::day(dates_month)==lubridate::day(dates_month[which.max(lubridate::day(dates_month))]))]
    bboxes_month <- bboxes_month[which(lubridate::day(dates_month)==lubridate::day(dates_month[which.max(lubridate::day(dates_month))]))]

    url_list <- c(url_list, list(features_month))
    bboxes <- c(bboxes, list(bboxes_month))
  }

  print("Pollution raster download started. This may take some time.")


  # right layer name depending on the indicator
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

  # The rasters do not have the appropriate extent and CRS
  # We have saved the bbox from the metadata and will use it to transform the raster
  raster_objs <- c()
  num <- 1
  for (x in url_list){
    # get bbox for this raster
    bb <- bboxes[[num]][1]
    # load raster
    rall <- terra::rast(x[[1]])
    # keep just the layer we want and transform to array
    rall <- as.array(rall[[paste0(layer)]])
    # now back to raster with the appropriate extent and CRS
    rall <- rast(rall, crs = "EPSG:4326", extent = ext(c(bb[[1]][1], bb[[1]][3], bb[[1]][2], bb[[1]][4])))
    # match shp_dt
    rall <- project(rall, crs(shp_dt))
    raster_objs <- c(raster_objs, rall)
    print(paste0("Month ", num, " of ", length(url_list), " completed."))
    num <- num + 1
  }


  date_list <- as_date(paste0(allmonths$year, "-", allmonths$month, "-01"))
  name_set <- paste0(indicator, "_", "y", allmonths$year, "_m", allmonths$month)

  print("Pollution Rasters Downloaded")

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








