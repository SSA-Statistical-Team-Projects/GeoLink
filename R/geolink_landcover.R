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

  raster_objs <- lapply(url_list, terra::rast)

  raster_list <- lapply(raster_objs, raster)

  raster_list <- lapply(seq_along(raster_objs), function(i) {
    setNames(raster_objs[[i]], as.character(i))
  })

  class_names <- c("No Data", "Water", "Trees", "Flooded vegetation", "Crops",
                   "Built area", "Bare ground", "Snow/ice", "Clouds", "Rangeland")

  class_values <- c(No_Data = 0, Water = 1, Trees = 2, Flooded_Vegetation = 4,
                    Crops = 5, Built_Area = 7, Bare_Ground = 8, Snow_Ice = 9,
                    Clouds = 10, Rangeland = 11)
  #return classes and values without hardcoding them
  #geos package

  proportions_list <- list()

  for (i in seq_along(raster_list)) {
    print(paste("Processing raster for year:", i))

    extracted_values <- exact_extract(raster_list[[i]], shp_dt, coverage_area = TRUE)

    class_proportions <- lapply(class_values, function(class_val) {
      class_proportion <- sum(extracted_values$value == class_val, na.rm = TRUE) / length(extracted_values$value)
      return(class_proportion)
    })

    proportions_list[[i]] <- class_proportions
  }

  proportions_df <- do.call(rbind, proportions_list)

  colnames(proportions_df) <- names(class_values)

  return(proportions_df)

}
