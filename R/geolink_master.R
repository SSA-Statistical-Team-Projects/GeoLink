#' Master function to run geolink_chirps, geolink_ntl, and geolink_landcover
#'
#' This function serves as a master function to run the functions in geolink_mach, each function based on the provided function name.
#' It dynamically passes all other arguments to the selected function.
#'
#' @param func_name A character string indicating the function to run, either "geolink_chirps", "geolink_ntl", or "geolink_landcover".
#' @param time_unit A character string indicating the aggregation period. For `geolink_landcover`, it must be "annual".
#' @param start_date A Date object or a character string in the format "yyyy-mm-dd" specifying the start date.
#' @param end_date A Date object or a character string in the format "yyyy-mm-dd" specifying the end date.
#' @param shp_dt An 'sf' or 'data.frame' object containing polygons or multipolygons.
#' @param shp_fn A character string specifying the file path for the shapefile (.shp) (optional).
#' @param grid_size A numeric value indicating the grid size to be used in meters.
#' @param survey_dt An 'sf' or 'data.frame' object representing a geocoded household survey.
#' @param survey_fn A character string specifying the file path for the geocoded survey (.dta format) (optional).
#' @param survey_lat A character string specifying the latitude variable from the survey (optional).
#' @param survey_lon A character string specifying the longitude variable from the survey (optional).
#' @param extract_fun A character string specifying the function to be applied in extraction of raster into the shapefile.
#' Default is "mean". Other options are "sum", "min", "max", "sd", "skew" and "rms".
#' @param buffer_size A numeric value specifying the buffer size in meters (optional).
#' @param survey_crs An integer specifying the CRS for the survey data. Default is 4326.
#' @param annual_version A character string specifying the version of annual NTL data for download. Default is "v21" (for `geolink_ntl` only).
#' @param month_version A character string specifying the version of monthly NTL data to use. Default is "v10" (for `geolink_ntl` only).
#' @param indicator A character string specifying the NTL indicator to use (for `geolink_ntl` only).
#' @param slc_type A character string specifying the SLC type (for `geolink_ntl` only).
#' @param use_survey A logical indicating whether to use survey data (for `geolink_ntl` only). Default is TRUE.
#' @param buffer_survey A logical specifying whether to estimate a statistic based on distance from the survey location (for `geolink_landcover` only).
#' @param iso_code A character, specifying the iso code for country to download data from. (For 'geolink_population' and 'geolink_buidings' only)
#' @param UN_adjst A character 'Y' if want 2020 UN adjusted dataset returned, if not leave blank.(For 'geolink_population' only)
#' @param constrained A character 'Y' if want constrained dataset returned, if not leave blank.(For 'geolink_population' only)
#' @param bespoke A character 'Y' if want bespoke dataset returned, if not leave blank. Version also should be specified.(For 'geolink_population' only)
#' @param version A character such as v2.0 or v2.1 which correlates to the iso_code provided for bespoke datasets.(For 'geolink_population' and 'geolink_buidings' only)
#' @param osm_feature_category A character, refering to the osm key wiki page, please see details below. (For 'geolink_get_POI' only)
#' @param osm_feature_subcategory A character, refering to the osm key wiki page, please see details below. (For 'geolink_get_POI' only)
#' @param buffer buffer area around shapefile. (For 'geolink_get_POI' only)
#' @param stata A flag for stata users. (For 'geolink_get_POI' only)
#' @param var A character, the variable of interest (e.g., "temperature", "precipitation").(For geolink_CMIP6', geolink_worldclim' and 'geolink_terraclimate' only)
#' @param res A character, the resolution of the data (e.g., "2.5m", "5m").(For geolink_CMIP6' and 'geolink_worldclim' only)
#' @param model A character, the climate model name (e.g., "ACCESS-ESM1-5", "CanESM5").(For geolink_CMIP6' only)
#' @param ssp A character, the Shared Socioeconomic Pathway (SSP) scenario (e.g., "ssp126", "ssp585").(For geolink_CMIP6' only)
#' @param time A character, the time period of interest (e.g., "historical", "2020-2049").(For geolink_CMIP6' only)
#' @param key A character, the API key created in when signing up to Opencellid profile. (For 'geolink_opencellid' only)
#' @param year A numeric, the year for which data is to be downloaded. (For 'geolink_terraclimate only)
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#' # Example usage
#'    df = run_geolink(
#'      func_name = "geolink_chirps",
#'      time_unit = "month",
#'      start_date = "2020-01-01",
#'      end_date = "2020-03-01",
#'      shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001", ],
#'      grid_size = 1000,
#'      survey_dt = hhgeo_dt,
#'      extract_fun = "mean")
#'
#'
#'      df = run_geolink(
#'      func_name = "geolink_electaccess"
#'      shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#'
#'
#'
#'
#'
#'
#'
run_geolink <- function(func_name,
                        time_unit,
                        start_date,
                        end_date,
                        shp_dt,
                        shp_fn = NULL,
                        grid_size = 1000,
                        survey_dt,
                        survey_fn = NULL,
                        survey_lat = NULL,
                        survey_lon = NULL,
                        extract_fun = "mean",
                        buffer_size = NULL,
                        survey_crs = 4326,
                        annual_version = "v21",
                        month_version = "v10",
                        indicator = NULL,
                        slc_type = "vcmslcfg",
                        use_survey = TRUE,
                        buffer_survey = FALSE,
                        iso_code,
                        UN_adjst = NULL,
                        constrained = NULL,
                        bespoke = NULL,
                        version = NULL,
                        osm_feature_category,
                        osm_feature_subcategory,
                        buffer = NULL,
                        stata = FALSE,
                        var,
                        res,
                        model,
                        ssp,
                        time,
                        key,
                        year) {

  if (func_name == "geolink_chirps") {
    geolink_chirps(
      time_unit = time_unit,
      start_date = start_date,
      end_date = end_date,
      shp_dt = shp_dt,
      shp_fn = shp_fn,
      grid_size = grid_size,
      survey_dt = survey_dt,
      survey_fn = survey_fn,
      survey_lat = survey_lat,
      survey_lon = survey_lon,
      extract_fun = extract_fun,
      buffer_size = buffer_size,
      survey_crs = survey_crs
    )
  } else if (func_name == "geolink_ntl") {
    geolink_ntl(
      time_unit = time_unit,
      start_date = start_date,
      end_date = end_date,
      annual_version = annual_version,
      month_version = month_version,
      indicator = indicator,
      slc_type = slc_type,
      shp_dt = shp_dt,
      shp_fn = shp_fn,
      grid_size = grid_size,
      use_survey = use_survey,
      survey_dt = survey_dt,
      survey_fn = survey_fn,
      survey_lat = survey_lat,
      survey_lon = survey_lon,
      extract_fun = extract_fun,
      buffer_size = buffer_size,
      survey_crs = survey_crs
    )
  } else if (func_name == "geolink_landcover") {
    geolink_landcover(
      time_unit = time_unit,
      start_date = start_date,
      end_date = end_date,
      shp_dt = shp_dt,
      shp_fn = shp_fn,
      grid_size = grid_size,
      survey_dt = survey_dt,
      survey_fn = survey_fn,
      survey_lat = survey_lat,
      survey_lon = survey_lon,
      buffer_survey = buffer_survey,
      extract_fun = extract_fun,
      survey_crs = survey_crs
    )
  }  else if (func_name == "geolink_population") {
    geolink_population(
      start_year = NULL,
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
      survey_crs = 4326
    )
  } else if (func_name == "geolink_get_poi") {
    geolink_get_poi(
      osm_feature_category,
      osm_feature_subcategory,
      shp_dt,
      shp_dsn = NULL,
      buffer = NULL,
      stata = FALSE)
  } else if (func_name == "geolink_electaccess") {
    geolink_electaccess(start_date = NULL,
                        end_date = NULL,
                        shp_dt,
                        shp_fn = NULL,
                        grid_size = 1000,
                        survey_dt,
                        survey_fn = NULL,
                        survey_lat = NULL,
                        survey_lon = NULL,
                        buffer_size = NULL,
                        extract_fun = "mean",
                        survey_crs = 4326)
  } else if (func_name == "geolink_elevation") {
    geolink_elevation(shp_dt,
                      shp_fn = NULL,
                      grid_size = 1000,
                      survey_dt,
                      survey_fn = NULL,
                      survey_lat = NULL,
                      survey_lon = NULL,
                      buffer_size = NULL,
                      extract_fun = "mean",
                      survey_crs = 4326) }


      else if (func_name == "geolink_buildings") {
      geolink_buildings(version,
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
                        survey_crs = 4326)

      }else if (func_name == "geolink_CMIP6") {
       geolink_CMIP6(var,
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
                     survey_crs = 4326)}


        else if (func_name == "geolink_cropland") {
        geolink_cropland(source = "WorldCover",
                           shp_dt,
                           shp_fn = NULL,
                           grid_size = 1000,
                           survey_dt,
                           survey_fn = NULL,
                           survey_lat = NULL,
                           survey_lon = NULL,
                           buffer_size = NULL,
                           extract_fun = "mean",
                           survey_crs = 4326)}


        else if (func_name == "geolink_worldclim"){
        geolink_worldclim(var,
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
                          survey_crs = 4326)}

        else if (func_name == "geolink_opencellid"){
        geolink_opencellid(shp_dt,
                           shp_fn = NULL,
                           grid_size_meters = 1000,
                           key = key)}

        else if (func_name == "geolink_terraclimate"){
        geolink_terraclimate(var,
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
                             survey_crs = 4326)}

        else {
            stop("Function name should be either 'geolink_chirps' 'geolink_ntl'
            or 'geolink_landcover'
                 or 'geolink_population' or 'geolink_get_poi',
                 or 'geolink_electaccess' or 'geolink_elevation'
                 or 'geolink_buildings' or
                 'geolink_CMIP6' or geolink_cropland'
                 or 'geolink_worldclim' or 'geolink_opencellid'
                 or 'geolink_terraclimate'")
          }


        }




