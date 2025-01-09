#' Master Function for Geospatial Data Extraction
#'
#' A comprehensive function to extract various types of geospatial data
#' from multiple sources with automatic parameter suggestions.
#'
#' @param data_type A character string specifying the type of data to extract.
#' Options include: "rainfall", "nightlight", "landcover", "population",
#' "poi", "electaccess", "elevation", "buildings", "cmip6", "cropland",
#' "worldclim", "opencellid", "terraclimate", "vegindex", "pollution".
#'
#' @return A dataframe or spatial dataframe containing the extracted geospatial data.
#'
#' @examples
#' \donttest{
#' # 1. Rainfall Data Extraction
#' # Using Administrative Boundary (shp_dt)
#' rainfall_shp_data <- run_geolink(
#'     data_type = "rainfall",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     time_unit = "month",
#'     grid_size = 1000
#' )
#'
#' # Using Survey Points (survey_dt)
#' rainfall_survey_data <- run_geolink(
#'     data_type = "rainfall",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     survey_dt = survey_points,
#'     time_unit = "month",
#'     grid_size = 1000
#' )
#'
#' # 2. Nighttime Lights Data Extraction
#' # Using Administrative Boundary
#' nightlight_shp_data <- run_geolink(
#'     data_type = "nightlight",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     indicator = "average_masked",
#'     version = "v21"
#' )
#'
#' # Using Survey Points
#' nightlight_survey_data <- run_geolink(
#'     data_type = "nightlight",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     survey_dt = survey_points,
#'     indicator = "average_masked",
#'     version = "v21"
#' )
#'
#' # 3. Land Cover Data Extraction
#' # Using Administrative Boundary
#' landcover_shp_data <- run_geolink(
#'     data_type = "landcover",
#'     start_date = "2020-01-01",
#'     end_date = "2021-01-01",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",]
#' )
#'
#' # Using Survey Points
#' landcover_survey_data <- run_geolink(
#'     data_type = "landcover",
#'     start_date = "2020-01-01",
#'     end_date = "2021-01-01",
#'     survey_dt = survey_points
#' )
#'
#' # 4. Population Data Extraction
#' # Using Administrative Boundary
#' population_shp_data <- run_geolink(
#'     data_type = "population",
#'     iso_code = "NGA",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     constrained = "Y"
#' )
#'
#' # Using Survey Points
#' population_survey_data <- run_geolink(
#'     data_type = "population",
#'     iso_code = "NGA",
#'     survey_dt = survey_points,
#'     constrained = "Y"
#' )
#'
#' # 5. Points of Interest Extraction
#' # Using Administrative Boundary
#' poi_shp_data <- run_geolink(
#'     data_type = "poi",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     osm_key = "amenity",
#'     osm_value = "hospital"
#' )
#'
#' # Using Survey Points
#' poi_survey_data <- run_geolink(
#'     data_type = "poi",
#'     survey_dt = survey_points,
#'     osm_key = "amenity",
#'     osm_value = "hospital"
#' )
#'
#' # 6. Electrification Access Data Extraction
#' # Using Administrative Boundary
#' elect_shp_data <- run_geolink(
#'     data_type = "electaccess",
#'     start_date = "2018-12-31",
#'     end_date = "2019-12-31",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",]
#' )
#'
#' # Using Survey Points
#' elect_survey_data <- run_geolink(
#'     data_type = "electaccess",
#'     start_date = "2018-12-31",
#'     end_date = "2019-12-31",
#'     survey_dt = survey_points
#' )
#'
#' # 7. Elevation Data Extraction
#' # Using Administrative Boundary
#' elevation_shp_data <- run_geolink(
#'     data_type = "elevation",
#'     iso_code = "NGA",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",]
#' )
#'
#' # Using Survey Points
#' elevation_survey_data <- run_geolink(
#'     data_type = "elevation",
#'     iso_code = "NGA",
#'     survey_dt = survey_points
#' )
#'
#' # 8. Buildings Data Extraction
#' # Using Administrative Boundary
#' buildings_shp_data <- run_geolink(
#'     data_type = "buildings",
#'     iso_code = "NGA",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     version = "v1.1"
#' )
#'
#' # Using Survey Points
#' buildings_survey_data <- run_geolink(
#'     data_type = "buildings",
#'     iso_code = "NGA",
#'     survey_dt = survey_points,
#'     version = "v1.1"
#' )
#'
#' # 9. CMIP6 Climate Model Data Extraction
#' # Using Administrative Boundary
#' cmip6_shp_data <- run_geolink(
#'     data_type = "cmip6",
#'     start_date = "2019-01-01",
#'     end_date = "2019-12-31",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     scenario = "ssp245",
#'     desired_models = "UKESM1-0-LL"
#' )
#'
#' # Using Survey Points
#' cmip6_survey_data <- run_geolink(
#'     data_type = "cmip6",
#'     start_date = "2019-01-01",
#'     end_date = "2019-12-31",
#'     survey_dt = survey_points,
#'     scenario = "ssp245",
#'     desired_models = "UKESM1-0-LL"
#' )
#'
#' # 10. Cropland Data Extraction
#' # Using Administrative Boundary
#' cropland_shp_data <- run_geolink(
#'     data_type = "cropland",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     source = "WorldCover"
#' )
#'
#' # Using Survey Points
#' cropland_survey_data <- run_geolink(
#'     data_type = "cropland",
#'     survey_dt = survey_points,
#'     source = "WorldCover"
#' )
#'
#' # 11. WorldClim Climate Data Extraction
#' # Using Administrative Boundary
#' worldclim_shp_data <- run_geolink(
#'     data_type = "worldclim",
#'     iso_code = "NGA",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     var = "temperature",
#'     res = "2.5m"
#' )
#'
#' # Using Survey Points
#' worldclim_survey_data <- run_geolink(
#'     data_type = "worldclim",
#'     iso_code = "NGA",
#'     survey_dt = survey_points,
#'     var = "temperature",
#'     res = "2.5m"
#' )
#'
#' # 12. OpenCellID Data Extraction
#' # Using Administrative Boundary
#' opencellid_shp_data <- run_geolink(
#'     data_type = "opencellid",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     cell_tower_file = "path/to/cell_towers.csv.gz"
#' )
#'
#' # Using Survey Points
#' opencellid_survey_data <- run_geolink(
#'     data_type = "opencellid",
#'     survey_dt = survey_points,
#'     cell_tower_file = "path/to/cell_towers.csv.gz"
#' )
#'
#' # 13. TerraClimate Data Extraction
#' # Using Administrative Boundary
#' terraclimate_shp_data <- run_geolink(
#'     data_type = "terraclimate",
#'     start_date = "2017-01-01",
#'     end_date = "2017-12-31",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     var = "tmin"
#' )
#'
#' # Using Survey Points
#' terraclimate_survey_data <- run_geolink(
#'     data_type = "terraclimate",
#'     start_date = "2017-01-01",
#'     end_date = "2017-12-31",
#'     survey_dt = survey_points,
#'     var = "tmin"
#' )
#'
#' # 14. Vegetation Index Data Extraction
#' # Using Administrative Boundary
#' vegindex_shp_data <- run_geolink(
#'     data_type = "vegindex",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     indicator = "NDVI"
#' )
#'
#' # Using Survey Points
#' vegindex_survey_data <- run_geolink(
#'     data_type = "vegindex",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     survey_dt = survey_points,
#'     indicator = "NDVI"
#' )
#'
#' # 15. Pollution Data Extraction
#' # Using Administrative Boundary
#' pollution_shp_data <- run_geolink(
#'     data_type = "pollution",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     indicator = "no2"
#' )
#'
#' # Using Survey Points
#' pollution_survey_data <- run_geolink(
#'     data_type = "pollution",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     survey_dt = survey_points,
#'     indicator = "no2"
#' )
#' }
#'
#' @export
run_geolink <- function(
    data_type,
    start_date = NULL,
    end_date = NULL,
    iso_code = NULL,
    shp_dt = NULL,
    survey_dt = NULL,
    ...
) {
  # Comprehensive parameter suggestions for each data type
  param_suggestions <- list(
    "rainfall" = list(
      description = "Download monthly/annual rainfall data from CHIRPS",
      required = c("time_unit", "start_date", "end_date", "shp_dt" = NULL),
      optional = list(
        time_unit = "month",
        grid_size = 1000,
        extract_fun = "mean",
        survey_crs = 4326
      )
    ),
    "nightlight" = list(
      description = "Download nighttime lights luminosity data",
      required = c("start_date", "end_date", "indicator", "shp_dt" = NULL),
      optional = list(
        time_unit = "annual",
        grid_size = 1000,
        extract_fun = "mean",
        version = "v21"
      )
    ),
    "landcover" = list(
      description = "Download annual land use land cover data",
      required = c("start_date", "end_date", "shp_dt" = NULL),
      optional = list(
        use_resampling = FALSE,
        grid_size = 1000
      )
    ),
    "population" = list(
      description = "Download population data from WorldPop",
      required = c("iso_code", "shp_dt" = NULL),
      optional = list(
        UN_adjst = "N",
        constrained = "Y",
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "poi" = list(
      description = "Download points of interest from OpenStreetMap",
      required = c("osm_key", "shp_dt" = NULL),
      optional = list(
        osm_value = NULL,
        buffer = NULL,
        shp_dt = NULL,
        survey_dt = NULL
      )
    ),
    "electaccess" = list(
      description = "Download high-resolution electrification access data",
      required = c("start_date", "end_date", "shp_dt" = NULL),
      optional = list(
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "elevation" = list(
      description = "Download high-resolution elevation data",
      required = c("iso_code"),
      optional = list(
        shp_dt = NULL,
        grid_size = 1000,
        extract_fun = "mean",
        survey_dt = NULL
      )
    ),
    "buildings" = list(
      description = "Download high-resolution building data",
      required = c("iso_code", "version", "shp_dt" = NULL),
      optional = list(
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "cmip6" = list(
      description = "Download CMIP6 climate model data",
      required = c("start_date", "end_date", "scenario", "desired_models", "shp_dt" = NULL),
      optional = list(
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "cropland" = list(
      description = "Download cropland data",
      required = c("shp_dt" = NULL),
      optional = list(
        source = "WorldCover",
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "worldclim" = list(
      description = "Download WorldClim climate data",
      required = c("iso_code", "var", "res", "shp_dt" = NULL),
      optional = list(
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "opencellid" = list(
      description = "Download cell tower data from OpenCellID",
      required = c("cell_tower_file", "shp_dt" = NULL),
      optional = list(
        grid_size = 1000,
        shp_dt = NULL,
        survey_dt = NULL
      )
    ),
    "terraclimate" = list(
      description = "Download TerraClimate data",
      required = c("var", "year", "shp_dt" = NULL),
      optional = list(
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "vegindex" = list(
      description = "Download vegetation index data (NDVI/EVI)",
      required = c("start_date", "end_date", "indicator", "shp_dt" = NULL),
      optional = list(
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "pollution" = list(
      description = "Download monthly pollution data",
      required = c("start_date", "end_date", "indicator", "shp_dt" = NULL),
      optional = list(
        grid_size = 1000,
        extract_fun = "mean"
      )
    )
  )


  # Validate input data type
  if (!(data_type %in% names(param_suggestions))) {
    stop(paste("Invalid data_type. Choose from:",
               paste(names(param_suggestions), collapse = ", ")))
  }

  # Automatically print parameter suggestions
  cat("\n--- Parameter Suggestions for", data_type, "Data Extraction ---\n")
  cat("Description:", param_suggestions[[data_type]]$description, "\n\n")

  cat("Required Parameters:\n")
  for (req in param_suggestions[[data_type]]$required) {
    cat("- ", req, "\n")
  }

  data_functions <- list(
    "rainfall" = geolink_chirps,
    "nightlight" = geolink_ntl,
    "landcover" = geolink_landcover,
    "population" = geolink_population,
    "poi" = geolink_get_poi,
    "electaccess" = geolink_electaccess,
    "elevation" = geolink_elevation,
    "buildings" = geolink_buildings,
    "cmip6" = geolink_CMIP6,
    "cropland" = geolink_cropland,
    "worldclim" = geolink_worldclim,
    "opencellid" = geolink_opencellid,
    "terraclimate" = geolink_terraclimate,
    "vegindex" = geolink_vegindex,
    "pollution" = geolink_pollution
  )


  # Prepare arguments dynamically
  args <- list(
    shp_dt = shp_dt,
    start_date = start_date,
    end_date = end_date
  )

  # Add additional arguments passed via ...
  additional_args <- list(...)
  args <- c(args, additional_args)



  # Call the function with prepared arguments
  tryCatch({
    result <- do.call(data_functions[[data_type]], args)
    return(result)
  }, error = function(e) {
    stop(paste("Error in extracting", data_type, "data:", e$message))
  })
}
