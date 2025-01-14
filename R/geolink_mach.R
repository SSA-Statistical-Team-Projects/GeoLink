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
#'                   annual_version = "v21",
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
#'                   annual_version = "v21",
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
#' @param start_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param end_date An object of class date, must be specified like "yyyy-mm-dd"
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household survey i.e.
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
                              survey_dt = NULL) {

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  source_python(file.path("inst", "python", "raster_utils.py"))

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
      cat("Downloaded raster to:", raster_path, "\n")

      if (!year %in% names(raster_year_map)) {
        raster_year_map[[year]] <- list()
      }
      raster_year_map[[year]] <- append(raster_year_map[[year]], raster_path)
    }, error = function(e) {
      warning(paste("Could not download raster for year", year, ":", e$message))
    })
  }

  # Python-based mosaicking and resampling
  mosaicked_rasters <- list()

  for (year in names(raster_year_map)) {
    cat("Processing rasters for year:", year, "\n")

    raster_paths <- raster_year_map[[year]]
    raster_paths <- as.character(raster_paths)
    raster_paths <- normalizePath(raster_paths)

    nonexistent_files <- raster_paths[!file.exists(raster_paths)]
    if (length(nonexistent_files) > 0) {
      stop("The following files do not exist: ", paste(nonexistent_files, collapse = ", "))
    }

    cat("Rasters for year:", year, "are located at:", raster_paths, "\n")

    if (length(raster_paths) == 1) {
      resampled_rasters <- resample_rasters(
        input_files = raster_paths,
        output_folder = file.path(temp_dir, "resampled", year),
        target_resolution = 1000
      )
    } else {
      resampled_rasters <- resample_rasters(
        input_files = raster_paths,
        output_folder = file.path(temp_dir, "resampled", year),
        target_resolution = 1000
      )
    }

    if (length(resampled_rasters) == 1) {
      mosaicked_path <- resampled_rasters[[1]]
    } else {
      mosaicked_path <- mosaic_rasters(input_files = resampled_rasters)
    }

    mosaicked_raster <- terra::rast(mosaicked_path)

    if (is.na(terra::crs(mosaicked_raster))) {
      terra::crs(mosaicked_raster) <- "EPSG:4326"
    } else {
      if (terra::crs(mosaicked_raster, describe = TRUE)$code != "4326") {
        mosaicked_raster <- terra::project(
          mosaicked_raster,
          "EPSG:4326"
        )
      }
    }

    mosaicked_rasters[[year]] <- mosaicked_raster
  }

  # Transform shapefile
  projected_shapefile <- if (sf::st_crs(shp_dt)$input == "EPSG:4326") {
    shp_dt
  } else {
    tryCatch({
      sf::st_transform(shp_dt, crs = sf::st_crs("EPSG:4326"))
    }, error = function(e) {
      warning("Could not reproject shapefile. Assigning EPSG:4326 CRS.")
      sf::st_set_crs(shp_dt, "EPSG:4326")
    })
  }

  cropped_rasters <- lapply(mosaicked_rasters, function(raster_obj) {
    terra::crop(raster_obj, terra::ext(projected_shapefile))
  })
  names(cropped_rasters) <- names(mosaicked_rasters)

  # Extract land cover proportions
  file_values <- it_obj$features[[1]]$assets$data$`file:values`
  class_values <- unlist(lapply(file_values, `[[`, "values"))
  class_names <- tolower(gsub(" ", "_", unlist(lapply(file_values, `[[`, "summary"))))

  # Add an additional column name for "no_data"
  all_column_names <- c(class_names, "no_data")

  # Create a list to store results for each year
  results_list <- list()

  for (i in seq_along(cropped_rasters)) {
    year <- names(cropped_rasters)[i]
    raster <- cropped_rasters[[i]]

    # Extract values for each polygon
    extracted_values <- exactextractr::exact_extract(raster, projected_shapefile, coverage_area = TRUE)

    # Prepare proportions for each polygon
    polygon_proportions <- lapply(seq_along(extracted_values), function(j) {
      ev <- extracted_values[[j]]
      total_area <- sum(ev$coverage_area, na.rm = TRUE)

      if (total_area == 0) {
        # Return NA for all proportions if total area is zero
        proportions <- setNames(rep(NA, length(all_column_names)), all_column_names)
      } else {
        proportions <- setNames(sapply(class_values, function(class_val) {
          class_area <- sum(ev$coverage_area[ev$value == class_val], na.rm = TRUE)
          (class_area / total_area) * 100
        }), class_names)

        # Calculate remainder and assign to "no_data"
        remainder <- 100 - sum(proportions, na.rm = TRUE)
        proportions["no_data"] <- max(0, remainder)
      }

      return(proportions)
    })

    # Convert to matrix
    proportions_matrix <- do.call(rbind, polygon_proportions)

    # Create a data frame with shapefile data and proportions
    year_results <- sf::st_drop_geometry(projected_shapefile)

    # Add proportions as columns
    for (col in all_column_names) {
      year_results[[col]] <- proportions_matrix[, col]
    }

    # Add year column
    year_results$year <- year

    # Add results for this year to the list
    results_list[[year]] <- year_results
  }

  # Combine results from all years
  final_result <- do.call(rbind, results_list)

  # Re-attach geometry to the combined result
  final_result <- sf::st_sf(final_result, geometry = sf::st_geometry(projected_shapefile))

  return(final_result)
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
#''@import rstac reticulate terra raster osmdata sp sf  geodata rvest httr
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

  unlink(tempdir(), recursive = TRUE)

  shp_dt <- ensure_crs_4326(shp_dt)

  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
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
#' @param osm_key A character, refering to the osm key wiki page, please see details below
#' @param osm_value A character, refering to the osm key wiki page, please see details below
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons
#' @param shp_fn A link to location of shapefile for stat users
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
#' poi_survey_df <- geolink_get_poi(osm_key = "amenity",
#'                                shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#'
#'}
#'

geolink_get_poi <- function(osm_key,
                            osm_value = NULL,
                            shp_dt = NULL,
                            survey_dt = NULL,
                            shp_fn = NULL,
                            survey_fn = NULL,
                            buffer = NULL,
                            stata = FALSE) {

  # Automatically convert survey_dt to sf if provided and shp_dt is NULL
  if (!is.null(survey_dt)) {
    if (!inherits(survey_dt, "sf")) {
      if (inherits(survey_dt, c("data.table", "data.frame"))) {
        if ("geometry" %in% names(survey_dt)) {
          survey_dt <- st_as_sf(survey_dt)
        } else if ("lon" %in% names(survey_dt) && "lat" %in% names(survey_dt)) {
          survey_dt <- st_as_sf(survey_dt, coords = c("lon", "lat"), crs = 4326)
        } else {
          stop("survey_dt must have a 'geometry' column or both 'lon' and 'lat' columns to be converted to an sf object.")
        }
      } else {
        stop("survey_dt must be an sf object, data.table, or data.frame.")
      }
    }
    survey_dt <- ensure_crs_4326(survey_dt)
    shp_dt <- survey_dt
  }

  if (!is.null(shp_fn)) {
    shp_dt <- st_read(shp_fn)
  }

  if (is.null(shp_dt)) {
    stop("Either shp_dt or survey_dt must be provided.")
  }

  if (nrow(shp_dt) == 0) {
    stop("shp_dt is empty after filtering. Please check the filter conditions.")
  }
  if (any(is.na(st_geometry(shp_dt)))) {
    stop("shp_dt contains invalid geometries. Please ensure all geometries are valid.")
  }

  # Updated warning suppression function
  oldw <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = oldw))

  if (!is.null(survey_dt)) {
    points <- st_geometry(survey_dt)
    results_list <- list()

    for (point in points) {
      point_bbox <- st_bbox(point)
      datapull <- opq(c(bbox = point_bbox, timeout = 7200)) %>%
        add_osm_feature(key = osm_key, value = osm_value)
      features <- osmdata_sf(datapull)
      results_list[[length(results_list) + 1]] <- features$osm_points
    }

    results <- do.call(rbind, results_list)
  } else {
    bbox <- st_bbox(shp_dt)
    if (any(is.na(bbox))) {
      stop("Bounding box contains NA values. Please ensure shp_dt has valid geometries.")
    }

    datapull <- opq(c(bbox = bbox, timeout = 7200)) %>%
      add_osm_feature(key = osm_key, value = osm_value)

    features <- osmdata_sf(datapull)
    results <- features$osm_points
  }

  results <- results %>%
    filter(if_any(-c(osm_id, geometry), ~ !is.na(.x)))

  query_dt <- st_join(results, shp_dt, left = FALSE)

  if (nrow(query_dt) == 0) {
    print("No points of interest")
  }

  if (stata) {
    query_dt <- query_dt[, !grepl("geometry", names(query_dt))]
  }

  print("OpenStreetMap data downloaded.")
  print("Process complete!")

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

  shp_dt <- ensure_crs_4326(shp_dt)

  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
  }

  if (!is.null(shp_fn)) {
    shp_dt <- st_read(shp_fn)
  }

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
#' df <- geolink_buildings(version = "v1.1", iso_code = "NGA", shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",])
#'
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
                              extract_fun = "mean") {

  shp_dt <- ensure_crs_4326(shp_dt)

  if (!is.null(survey_dt)) {
    survey_dt <- ensure_crs_4326(survey_dt)
  }

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
#' @import rstac terra raster osmdata sp sf httr geodata progress
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
                          shp_dt,
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

 it_obj <- fetch_planetary_data("nasa-nex-gddp-cmip6", start_date, end_date, shp_dt)
  # Set date range
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Create STAC connection
  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  # Retrieve URLs for the specified scenario
  it_obj <- s_obj %>%
    stac_search(
      collections = "nasa-nex-gddp-cmip6",
      bbox = sf::st_bbox(shp_dt),
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
#' @import rstac terra raster osmdata sp sf httr geodata
#'
#' @examples
#' \donttest{
#'
#' # Example usage
#' df <- geolink_cropland(source = "WorldCover", shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#' }
#'

geolink_cropland <- function(
    source = "WorldCover",
    shp_dt = NULL,
    shp_fn = NULL,
    grid_size = 1000,
    survey_dt = NULL,
    survey_fn = NULL,
    survey_lat = NULL,
    survey_lon = NULL,
    buffer_size = NULL,
    extract_fun = "mean",
    survey_crs = 4326) {

  # Ensure consistent CRS
  shp_dt <- ensure_crs_4326(shp_dt)
  survey_dt <- ensure_crs_4326(survey_dt)

  # Clear temporary directory
  unlink(tempdir(), recursive = TRUE)

  # Get bounding box of shapefile
  bbox <- sf::st_bbox(shp_dt)

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
#' This function processes downloaded OpenCellID data, which provides information about cell towers and their coverage areas.
#' The return dataframe gives data of the nearest cell tower to the shapefile polygon centroid.
#'
#' @param cell_tower_file A csv.gz file path downloaded from OpencellID.
#' @param shp_dt An object of class 'sf', 'data.frame' which contains polygons or multipolygons representing the study area.
#' @param shp_fn A character, file path for the shapefile (.shp) to be read (for STATA users only).
#' @param grid_size A numeric, the grid size to be used in meters for analyzing the cell tower data. The maximum possible is 2000 meters.
#'
#' @return A processed data frame or object based on the input parameters and downloaded data.
#'
#' @importFrom terra rast
#' @importFrom httr GET timeout
#' @import rstac terra raster osmdata sp sf httr geodata data.table geosphere
#'
#' @examples
#' \donttest{
#'
#' # Example usage
#' results <- geolink_opencellid(cell_tower_file = "C:/Users/username/Downloads/621.csv.gz",
#'                              shp_dt = shp_dt)
#' }
#'


# Combined function to calculate tower stats and return the nearest lat/lon for a polygon

geolink_opencellid <- function(cell_tower_file,
                               shp_dt = NULL,
                               survey_dt = NULL,
                               shp_fn = NULL,
                               survey_fn = NULL,
                               grid_size = 1000) {

  # Convert data.table or data.frame to sf if needed
  convert_to_sf <- function(dt) {
    if ("geometry" %in% names(dt)) {
      st_as_sf(dt, crs = 4326)
    } else if ("lon" %in% names(dt) && "lat" %in% names(dt)) {
      st_as_sf(dt, coords = c("lon", "lat"), crs = 4326)
    } else {
      stop("survey_dt must have a 'geometry' column or both 'lon' and 'lat' columns to convert to sf.")
    }
  }

  # Check input type and ensure CRS is EPSG:4326
  input_type <- NULL

  if (!is.null(shp_dt)) {
    input_type <- "shapefile"
    if (is.character(shp_dt)) {
      if (!file.exists(shp_dt)) {
        stop("Shapefile not found at the specified path")
      }
      shp_dt <- st_read(shp_dt)
    } else if (!inherits(shp_dt, "sf")) {
      stop("shp_dt must be a file path or an sf object.")
    }
    shp_dt <- ensure_crs_4326(shp_dt)
  } else if (!is.null(survey_dt)) {
    input_type <- "survey"
    if (inherits(survey_dt, c("data.table", "data.frame"))) {
      survey_dt <- convert_to_sf(survey_dt)
    } else if (!inherits(survey_dt, "sf")) {
      stop("survey_dt must be an sf object, data.table, or data.frame.")
    }
    survey_dt <- ensure_crs_4326(survey_dt)
  } else {
    stop("Either shp_dt or survey_dt must be provided.")
  }

  if (is.null(shp_fn) == FALSE){
    shp_dt <- sf::read_sf(shp_fn)}


  # Load and prepare cell tower data
  cell_towers <- read_opencellid_data(cell_tower_file)
  cell_towers_sf <- st_as_sf(cell_towers, coords = c("lon", "lat"), crs = 4326)

  # Helper function to find the nearest cell tower
  find_nearest_tower <- function(geometry, cell_towers_sf) {
    distances <- st_distance(geometry, cell_towers_sf, by_element = FALSE)
    nearest_idx <- which.min(distances)
    nearest_distance <- min(distances)
    nearest_coordinates <- st_coordinates(cell_towers_sf)[nearest_idx, ]

    list(
      nearest_distance = nearest_distance,
      nearest_lon = nearest_coordinates["X"],
      nearest_lat = nearest_coordinates["Y"]
    )
  }

  # Process based on input type
  if (input_type == "shapefile") {
    # Transform cell tower CRS to match shapefile
    cell_towers_sf <- st_transform(cell_towers_sf, st_crs(shp_dt))

    # Process each polygon
    results <- lapply(1:nrow(shp_dt), function(i) {
      polygon <- shp_dt[i, ]
      towers_in_polygon <- st_within(cell_towers_sf, polygon, sparse = FALSE)
      num_towers <- sum(towers_in_polygon)

      if (num_towers > 0) {
        towers_sf <- cell_towers_sf[towers_in_polygon, ]
        nearest_tower <- find_nearest_tower(st_centroid(polygon), towers_sf)
      } else {
        nearest_tower <- list(nearest_distance = NA, nearest_lon = NA, nearest_lat = NA)
      }

      cbind(
        as.data.frame(polygon),
        data.frame(
          polygon_id = i,
          num_towers = num_towers,
          nearest_distance = nearest_tower$nearest_distance,
          nearest_lon = nearest_tower$nearest_lon,
          nearest_lat = nearest_tower$nearest_lat
        )
      )
    })

    results_df <- do.call(rbind, results)
    return(results_df)
  }

  if (input_type == "survey") {
    # Transform cell tower CRS to match survey points
    cell_towers_sf <- st_transform(cell_towers_sf, st_crs(survey_dt))

    # Process each survey point
    results <- lapply(1:nrow(survey_dt), function(i) {
      point <- survey_dt[i, ]
      nearest_tower <- find_nearest_tower(point, cell_towers_sf)

      cbind(
        as.data.frame(point),
        data.frame(
          nearest_distance = nearest_tower$nearest_distance,
          nearest_lon = nearest_tower$nearest_lon,
          nearest_lat = nearest_tower$nearest_lat
        )
      )
    })

    results_df <- do.call(rbind, results)
    return(results_df)
  }
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








