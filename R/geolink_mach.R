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
                           shp_dt,
                           shp_fn = NULL,
                           grid_size = 1000,
                           survey_dt,
                           survey_fn = NULL,
                           survey_lat = NULL,
                           survey_lon = NULL,
                           buffer_size = NULL,
                           extract_fun = "mean",
                           survey_crs = 4326) {


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


  name_set <- paste0("rainfall_", time_unit, 1:name_count)


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
#' @param buffer_survey A logical, specify TRUE if interested in estimating a statistic based on distance
#' from the survey location.
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
                        shp_dt,
                        shp_fn = NULL,
                        grid_size = 1000,
                        use_survey = TRUE,
                        survey_dt,
                        survey_fn = NULL,
                        survey_lat = NULL,
                        survey_lon = NULL,
                        extract_fun = "mean",
                        buffer_size = NULL,
                        survey_crs = 4326){

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

  name_set <- paste0("radians_", time_unit, name_count)

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
#'
#' @import rstac terra raster osmdata sp sf httr geodata
#'
#'
geolink_landcover <- function(time_unit = "annual",
                              start_date,
                              end_date,
                              shp_dt) {

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  it_obj <- s_obj %>%
    stac_search(collections = "io-lulc-annual-v02",
                bbox = sf::st_bbox(shp_dt),
                datetime = paste(start_date, end_date, sep = "/")) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  url_list <- lapply(1:length(it_obj$features),
                     function(x) {
                       url <- paste0("/vsicurl/", it_obj$features[[x]]$assets$data$href)
                       return(url)
                     })


  raster_list <- lapply(url_list, terra::rast)



  raster_list <- lapply(seq_along(raster_list), function(i) {
    setNames(raster_objs[[i]], as.character(i))
  })

  # Define class names based on the given unique values and corresponding categories
  class_names <- c("No Data", "Water", "Trees", "Flooded vegetation", "Crops", "Built area", "Bare ground", "Snow/ice", "Clouds", "Rangeland")

  # Define class names and values based on the given unique values and corresponding categories
  class_values <- c(No_Data = 0, Water = 1, Trees = 2, Flooded_Vegetation = 4,
                    Crops = 5, Built_Area = 7, Bare_Ground = 8, Snow_Ice = 9, Clouds = 10, Rangeland = 11)

  # Initialize a list to store proportions for each class
  proportions_list <- list()

  # Apply the summarizing function to each raster and combine results
  for (i in seq_along(raster_list)) {
    print(paste("Processing raster:", i))

    # Extract values from raster that intersect with shapefile
    extracted_values <- exact_extract(raster_list[[i]], shp_dt, coverage_area = TRUE)

    # Summarize the extracted values into proportions for each class
    class_proportions <- lapply(class_values, function(class_val) {
      class_proportion <- sum(extracted_values$value == class_val, na.rm = TRUE) / length(extracted_values$value)
      return(class_proportion)
    })

    # Append the proportions for the current raster to the list
    proportions_list[[i]] <- class_proportions
  }

  # Combine the proportions for all rasters into a data frame
  proportions_df <- do.call(rbind, proportions_list)

  # Set column names to class names
  colnames(proportions_df) <- names(class_values)

  # Return the proportions table along with other information
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
#''@import rstac terra raster osmdata sp sf httr geodata reticulate
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
#'
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
                               grid_size = 1000,
                               survey_dt,
                               survey_fn = NULL,
                               survey_lat = NULL,
                               survey_lon = NULL,
                               buffer_size = NULL,
                               extract_fun = "mean",
                               survey_crs = 4326) {


  unlink(tempdir(), recursive = TRUE)

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

  # raster_objs <- lapply(raster_objs, raster)

  # if (!is.null(start_year) && !is.null(end_year)) {
  #   year_sequence <- seq(lubridate::year(start_date), lubridate::year(end_date))
  # } else {
  #   year_sequence <- year(start_date)
  # }

  name_set <- paste0("population_", 1:length(raster_objs))

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
#' @import rstac terra raster osmdata sp sf httr geodata
#'
#' @examples
#'\donttest{
#'
#'

#df <- geolink_get_poi(osm_feature_category = "building",
# osm_feature_subcategory ="farm",
# shp_dt = shp_dt)

#'

geolink_get_poi <- function(osm_feature_category,
                            osm_feature_subcategory,
                            shp_dt,
                            shp_dsn = NULL,
                            buffer = NULL,
                            stata = FALSE){

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
#' @import rstac terra raster osmdata sp sf httr geodata
#'
#' @examples
#'\donttest{
#'
#'
#' df <- geolink_electaccess(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#'

geolink_electaccess <- function(start_date = NULL,
                                end_date = NULL,
                                shp_dt = NULL,
                                shp_fn = NULL,
                                grid_size = 1000,
                                survey_dt,
                                survey_fn = NULL,
                                survey_lat = NULL,
                                survey_lon = NULL,
                                buffer_size = NULL,
                                extract_fun = "mean",
                                survey_crs = 4326){


  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  s_obj <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")


  it_obj <- s_obj %>%

    rstac::stac_search(collections = "hrea",
                bbox = sf::st_bbox(shp_dt)) %>%
    rstac::get_request() %>%
    rstac::items_sign(sign_fn = rstac::sign_planetary_computer())


  url_list <- lapply(1:length(it_obj$features),
                     function(x) {
                       url <- paste0("/vsicurl/", it_obj$features[[x]]$assets$lightscore$href)
                       return(url)
                     })

  raster_objs <- lapply(url_list, terra::rast)

  raster_objs <- lapply(raster_objs, raster)

  # year_sequence <- seq(lubridate::year(start_date), lubridate::year(end_date))
  #
  # name_set <- paste0("lightscore_", year_sequence)

  #### create raster names
  name_set <- unlist(lapply(X = raster_objs,
                            FUN = names))

  print("Electrification Access Raster Downloaded")

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

  return(dt)}

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
#' @import rstac terra raster osmdata sp sf httr geodata
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
                              shp_dt,
                              shp_fn = NULL,
                              grid_size = 1000,
                              survey_dt,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              extract_fun = "mean",
                              survey_crs = 4326){

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
#' @import rstac terra raster osmdata sp sf httr geodata
#'
#' @examples
#' \donttest{
#'
#' # Example usage with version 1.1
#' df <- geolink_buildings(version = "v1.1", iso_code = "NGA", shp_dt = shp_dt)
#'
#' }
#'

geolink_buildings <- function(version,
                              iso_code,
                              shp_dt = NULL,
                              shp_fn = NULL,
                              grid_size = 1000,
                              survey_dt,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              extract_fun = "mean",
                              survey_crs = 4326){

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



  tif_files <- list.files(path = temp_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

  name_set <- c()

  for (file in tif_files) {
    base_name <- basename(file)

    extracted_string <- sub(".*1_([^\\.]+)\\.tif$", "\\1", base_name)

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
#' @import rstac terra raster osmdata sp sf httr geodata
#'
#' @examples
#' \donttest{
#'
#' # Example usage
#' df <- geolink_CMIP6(var = "temperature", res = "2.5m", model = "ACCESS-ESM1-5", ssp = "ssp126", time = "2020-2049", shp_dt = shp_dt)
#'
#' bio10 <- geolink_CMIP6(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],"CNRM-CM6-1", "585", "2061-2080", var="bioc", res=10)
#' }
#'


geolink_CMIP6 <- function(var,
                          res,
                          model,
                          ssp,
                          time,
                          shp_dt,
                          shp_fn = NULL,
                          grid_size = 1000,
                          survey_dt = NULL,
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


  data <- cmip6_tile(var=var, res=res, lon=lon, lat=lat, model = model, ssp = ssp, time = time, path = tempdir())

  tif_files <- list.files(tempdir(), pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

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

  name_set <- paste0("elevation_")


  print("CMIP6  Raster Downloaded")

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

  return(df)}

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
#' @import rstac terra raster osmdata sp sf httr geodata
#'
#' @examples
#' \donttest{
#'
#' # Example usage
#' df <- geolink_cropland(source = "WorldCover", shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#' }
#'
geolink_cropland <- function(source = "WorldCover",
                             shp_dt,
                             shp_fn = NULL,
                             grid_size = 1000,
                             survey_dt,
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
                              shp_dt,
                              shp_fn = NULL,
                              grid_size = 1000,
                              survey_dt,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              extract_fun = "mean",
                              survey_crs = 4326){


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
#' @import rstac terra raster osmdata sp sf httr geodata data.table
#'
#' @examples
#' \donttest{
#'
#' # Example usage
#' df <- geolink_opencellid(shp_dt = shp_dt)
#' }
#'


# Function to read shapefile and call OpenCellID API
geolink_opencellid <- function(shp_dt,
                               shp_fn = NULL,
                               grid_size_meters = 1000,
                               key = "pk.b6cb635812d221e898b979b1a6440b22") {

  max_attempts <- 3

  call_opencellid_api <- function(latmin, lonmin, latmax, lonmax, temp_file, attempt = 1) {
    key <- key
    url <- paste0("http://opencellid.org/cell/getInArea?key=", key, "&BBOX=",
                  latmin, ",", lonmin, ",", latmax, ",", lonmax, "&format=csv")
    timeout_seconds <- 7200
    response <- GET(url, timeout(timeout_seconds))

    if (status_code(response) == 200) {
      content <- content(response, "text")
      # Open file in append mode and write content
      con <- file(temp_file, "a")  # "a" for append mode
      writeLines(content, con)
      close(con)  # Close the file connection
      print(paste("Data downloaded successfully for BBOX:", latmin, lonmin, latmax, lonmax))
    } else {
      if (attempt < max_attempts) {
        Sys.sleep(5)  # Wait for 5 seconds before retrying
        print(paste("Retrying API call for BBOX:", latmin, lonmin, latmax, lonmax))
        recall <- Recall(latmin, lonmin, latmax, lonmax, temp_file, attempt + 1)
        return(recall)
      } else {
        print(paste("Failed to download data for BBOX:", latmin, lonmin, latmax, lonmax))
      }
    }
  }

  # Ensure the CRS is defined
  if (is.na(st_crs(shp_dt))) {
    st_crs(shp_dt) <- 4326  # Assign a default CRS (WGS 84) if none is defined
  }

  # Transform the shapefile to CRS 4326 if it is not already in that CRS
  if (st_crs(shp_dt)$epsg != 4326) {
    shp_dt <- st_transform(shp_dt, crs = 4326)
  }

  # Get the bounding box in the target CRS coordinates
  bbox <- st_bbox(shp_dt)

  # Convert grid size from meters to degrees (approximation)
  grid_size_lat_degrees <- grid_size_meters / 111320  # Latitude degrees (approx.)

  # Initialize temp file for saving API responses
  temp_file <- file.path(tempdir(), "data.csv")
  if (file.exists(temp_file)) file.remove(temp_file)  # Remove if it exists

  # Loop through each grid cell and call the API
  latmin <- bbox["ymin"]
  while (latmin < bbox["ymax"]) {
    lonmin <- bbox["xmin"]
    while (lonmin < bbox["xmax"]) {
      latmax <- latmin + grid_size_lat_degrees
      if (latmax > bbox["ymax"]) latmax <- bbox["ymax"]

      # Calculate longitude degree size based on the latitude
      grid_size_lon_degrees <- grid_size_meters / (111320 * cos(latmin * pi / 180))
      lonmax <- lonmin + grid_size_lon_degrees
      if (lonmax > bbox["xmax"]) lonmax <- bbox["xmax"]

      # Call the API for the current grid cell
      call_opencellid_api(latmin, lonmin, latmax, lonmax, temp_file)

      lonmin <- lonmax
    }
    latmin <- latmax
  }

  # Read the CSV file into a data.table
  dt <- fread(temp_file)

  return(dt)
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
#' @import rstac, reticulate, terra, raster, osmdata, sp, sf, geodata, httr, ncdf4, rgdal
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
                                 grid_size = 1000,
                                 survey_dt,
                                 survey_fn = NULL,
                                 survey_lat = NULL,
                                 survey_lon = NULL,
                                 buffer_size = NULL,
                                 extract_fun = "mean",
                                 survey_crs = 4326) {

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
    print(raster_list)

    months <- month.abb
    print(months)


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
