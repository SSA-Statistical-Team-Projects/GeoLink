#' Master Function for Geospatial Data Extraction
#'
#' A comprehensive function to extract various types of geospatial data
#' from multiple sources with automatic parameter suggestions.
#'
#' @param data_type A character string specifying the type of data to extract.
#' Options include: "rainfall", "nightlight", "landcover", "population",
#' "poi", "electaccess", "elevation", "buildings", "cmip6", "cropland",
#' "worldclim", "opencellid", "terraclimate", "vegindex", "pollution".
#' @param start_date Optional. A character or date object representing the
#' start date for data extraction (format: "YYYY-MM-DD").
#' @param end_date Optional. A character or date object representing the
#' end date for data extraction (format: "YYYY-MM-DD").
#' @param iso_code Optional. A character string representing the ISO country code.
#' @param shp_dt Optional. A spatial dataframe (sf object) containing polygon
#' or multipolygon geometries for spatial filtering.
#' @param survey_dt Optional. A spatial dataframe (sf object) containing
#' survey point locations.
#' @param additional_params Optional. A list of additional parameters specific
#' to the chosen data type.
#'
#' @return A dataframe or spatial dataframe containing the extracted geospatial data.
#'
#' @details
#' This function provides a unified interface for extracting various types of
#' geospatial data. Upon calling the function, it automatically prints parameter
#' suggestions and requirements for the specified data type.
#'
#' @examples
#' \donttest{
#' # 1. Rainfall Data Extraction
#' rainfall_data <- run_geolink(
#'     data_type = "rainfall",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",]
#' )
#'
#' # 2. Nighttime Lights Data Extraction
#' nightlight_data <- run_geolink(
#'     data_type = "nightlight",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     additional_params = list(
#'         indicator = "average_masked"
#'     )
#' )
#'
#' # 3. Land Cover Data Extraction
#' landcover_data <- run_geolink(
#'     data_type = "landcover",
#'     start_date = "2020-01-01",
#'     end_date = "2021-01-01",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",]
#' )
#'
#' # 4. Population Data Extraction
#' population_data <- run_geolink(
#'     data_type = "population",
#'     iso_code = "NGA",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     additional_params = list(
#'         constrained = "Y"
#'     )
#' )
#'
#' # 5. Points of Interest Extraction
#' poi_data <- run_geolink(
#'     data_type = "poi",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     additional_params = list(
#'         osm_key = "amenity",
#'         osm_value = "hospital"
#'     )
#' )
#'
#' # 6. Electrification Access Data Extraction
#' elect_data <- run_geolink(
#'     data_type = "electaccess",
#'     start_date = "2018-12-31",
#'     end_date = "2019-12-31",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",]
#' )
#'
#' # 7. Elevation Data Extraction
#' elevation_data <- run_geolink(
#'     data_type = "elevation",
#'     iso_code = "NGA",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",]
#' )
#'
#' # 8. Buildings Data Extraction
#' buildings_data <- run_geolink(
#'     data_type = "buildings",
#'     iso_code = "NGA",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     additional_params = list(
#'         version = "v1.1"
#'     )
#' )
#'
#' # 9. CMIP6 Climate Model Data Extraction
#' cmip6_data <- run_geolink(
#'     data_type = "cmip6",
#'     start_date = "2019-01-01",
#'     end_date = "2019-12-31",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     additional_params = list(
#'         scenario = "ssp245",
#'         desired_models = "UKESM1-0-LL"
#'     )
#' )
#'
#' # 10. Cropland Data Extraction
#' cropland_data <- run_geolink(
#'     data_type = "cropland",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     additional_params = list(
#'         source = "WorldCover"
#'     )
#' )
#'
#' # 11. WorldClim Climate Data Extraction
#' worldclim_data <- run_geolink(
#'     data_type = "worldclim",
#'     iso_code = "NGA",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     additional_params = list(
#'         var = "temperature",
#'         res = "2.5m"
#'     )
#' )
#'
#' # 12. OpenCellID Data Extraction
#' opencellid_data <- run_geolink(
#'     data_type = "opencellid",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     additional_params = list(
#'         cell_tower_file = "path/to/cell_towers.csv.gz"
#'     )
#' )
#'
#' # 13. TerraClimate Data Extraction
#' terraclimate_data <- run_geolink(
#'     data_type = "terraclimate",
#'     start_date = "2017-01-01",
#'     end_date = "2017-12-31",
#'     shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'     additional_params = list(
#'         var = "tmin"
#'     )
#' )
#'
#' }
#'
#' @export
#' @import sf
#' @importFrom methods is

run_geolink <- function(
    data_type,
    start_date = NULL,
    end_date = NULL,
    iso_code = NULL,
    shp_dt = NULL,
    survey_dt = NULL,
    additional_params = list()
) {
  # Comprehensive parameter suggestions for each data type
  param_suggestions <- list(
    "rainfall" = list(
      description = "Download monthly/annual rainfall data from CHIRPS",
      required = c("start_date", "end_date", "shp_dt"),
      optional = list(
        time_unit = "month",
        grid_size = 1000,
        extract_fun = "mean",
        survey_crs = 4326
      )
    ),
    "nightlight" = list(
      description = "Download nighttime lights luminosity data",
      required = c("start_date", "end_date", "indicator", "shp_dt"),
      optional = list(
        time_unit = "annual",
        grid_size = 1000,
        extract_fun = "mean",
        version = "v21"
      )
    ),
    "landcover" = list(
      description = "Download annual land use land cover data",
      required = c("start_date", "end_date", "shp_dt"),
      optional = list(
        use_resampling = FALSE,
        grid_size = 1000
      )
    ),
    "population" = list(
      description = "Download population data from WorldPop",
      required = c("iso_code", "shp_dt"),
      optional = list(
        UN_adjst = "N",
        constrained = "Y",
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "poi" = list(
      description = "Download points of interest from OpenStreetMap",
      required = c("osm_key", "shp_dt"),
      optional = list(
        osm_value = NULL,
        buffer = NULL,
        shp_dt = NULL,
        survey_dt = NULL
      )
    ),
    "electaccess" = list(
      description = "Download high-resolution electrification access data",
      required = c("start_date", "end_date", "shp_dt"),
      optional = list(
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "elevation" = list(
      description = "Download high-resolution elevation data",
      required = c("iso_code", "shp_dt"),
      optional = list(
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "buildings" = list(
      description = "Download high-resolution building data",
      required = c("iso_code", "version", "shp_dt"),
      optional = list(
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "cmip6" = list(
      description = "Download CMIP6 climate model data",
      required = c("start_date", "end_date", "scenario", "desired_models", "shp_dt"),
      optional = list(
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "cropland" = list(
      description = "Download cropland data",
      required = c("shp_dt"),
      optional = list(
        source = "WorldCover",
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "worldclim" = list(
      description = "Download WorldClim climate data",
      required = c("iso_code", "var", "res", "shp_dt"),
      optional = list(
        grid_size = 1000,
        extract_fun = "mean"
      )
    ),
    "opencellid" = list(
      description = "Download cell tower data from OpenCellID",
      required = c("cell_tower_file", "shp_dt"),
      optional = list(
        grid_size = 1000,
        shp_dt = NULL,
        survey_dt = NULL
      )
    ),
    "terraclimate" = list(
      description = "Download TerraClimate data",
      required = c("var", "year", "shp_dt"),
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

  cat("\nOptional Parameters with Default Values:\n")
  for (opt_name in names(param_suggestions[[data_type]]$optional)) {
    cat("- ", opt_name, ": ",
        param_suggestions[[data_type]]$optional[[opt_name]], "\n")
  }
  cat("\n")

  # Function mapping
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
    "terraclimate" = geolink_terraclimate
  )

  # Prepare arguments dynamically
  args <- list(
    shp_dt = shp_dt,
    survey_dt = survey_dt,
    start_date = start_date,
    end_date = end_date,
    iso_code = iso_code
  )

  # Add additional parameters
  args <- c(args, additional_params)

  # Remove NULL arguments
  args <- args[!sapply(args, is.null)]

  # Call the function with prepared arguments
  tryCatch({
    result <- do.call(data_functions[[data_type]], args)
    return(result)
  }, error = function(e) {
    stop(paste("Error in extracting", data_type, "data:", e$message))
  })
}
