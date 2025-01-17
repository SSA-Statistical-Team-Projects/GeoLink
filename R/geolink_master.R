#' Master Function for Geospatial Data Extraction
#'
#' @param data_type Type of data to extract
#' @param start_date Optional start date
#' @param end_date Optional end date
#' @param iso_code Optional ISO country code
#' @param shp_dt Optional spatial dataframe
#' @param survey_dt Optional survey points
#' @param ... Additional parameters passed directly to the specific data extraction function
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
    ...
) {
  # Parameter suggestions for each data type
  param_suggestions <- list(
    "rainfall" = list(
      description = "Download monthly/annual rainfall data from CHIRPS",
      required = c("time_unit", "start_date", "end_date", "shp_dt"),
      optional = list(
        shp_fn = NULL,
        grid_size = NULL,
        survey_dt = NULL,
        survey_fn = NULL,
        survey_lat = NULL,
        survey_lon = NULL,
        buffer_size = NULL,
        extract_fun = "mean",
        survey_crs = 4326
      )
    ),
    "nightlight" = list(
      description = "Download nighttime lights luminosity data",
      required = c("start_date", "end_date", "indicator"),
      optional = list(
        time_unit = "annual",
        annual_version = "v21",
        month_version = "v10",
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
        survey_crs = 4326
      )
    ),
    "landcover" = list(
      description = "Download annual land use land cover data",
      required = c("start_date", "end_date", "shp_dt"),
      optional = list(
        use_resampling = FALSE,
        grid_size = 1000,
        survey_dt = NULL,
        survey_fn = NULL,
        survey_lat = NULL,
        survey_lon = NULL,
        buffer_size = NULL,
        survey_crs = 4326,
        shp_fn = NULL
      )
    ),
    "population" = list(
      description = "Download population data from WorldPop",
      required = c("iso_code", "shp_dt"),
      optional = list(
        start_year = NULL,
        end_year = NULL,
        UN_adjst = "N",
        constrained = "Y",
        bespoke = NULL,
        version = NULL,
        shp_fn = NULL,
        grid_size = 1000,
        survey_dt = NULL,
        survey_fn = NULL,
        survey_lat = NULL,
        survey_lon = NULL,
        buffer_size = NULL,
        extract_fun = "mean",
        survey_crs = 4326,
        file_location = tempdir()
      )
    ),
    "poi" = list(
      description = "Download points of interest from OpenStreetMap",
      required = c("osm_key", "shp_dt"),
      optional = list(
        osm_value = NULL,
        buffer = NULL,
        survey_dt = NULL,
        survey_fn = NULL,
        stata = FALSE
      )
    ),
    "electaccess" = list(
      description = "Download high-resolution electrification access data",
      required = c("start_date", "end_date", "shp_dt"),
      optional = list(
        shp_fn = NULL,
        grid_size = 1000,
        survey_dt = NULL,
        survey_fn = NULL,
        survey_lat = NULL,
        survey_lon = NULL,
        buffer_size = NULL,
        extract_fun = "mean",
        survey_crs = 4326
      )
    ),
    "elevation" = list(
      description = "Download high-resolution elevation data",
      required = c("iso_code", "shp_dt"),
      optional = list(
        shp_fn = NULL,
        grid_size = NULL,
        survey_dt = NULL,
        survey_fn = NULL,
        survey_lat = NULL,
        survey_lon = NULL,
        buffer_size = NULL,
        extract_fun = "mean",
        survey_crs = 4326
      )
    ),
    "buildings" = list(
      description = "Download high-resolution building data",
      required = c("iso_code", "version", "shp_dt"),
      optional = list(
        shp_fn = NULL,
        grid_size = NULL,
        survey_dt = NULL,
        survey_fn = NULL,
        survey_lat = NULL,
        survey_lon = NULL,
        buffer_size = NULL,
        extract_fun = "mean",
        survey_crs = 4326,
        indicators = "ALL"
      )
    ),
    "cmip6" = list(
      description = "Download CMIP6 climate model data",
      required = c("start_date", "end_date", "scenario", "desired_models", "shp_dt"),
      optional = list(
        shp_fn = NULL,
        grid_size = NULL,
        survey_dt = NULL,
        survey_fn = NULL,
        survey_lat = NULL,
        survey_lon = NULL,
        buffer_size = NULL,
        extract_fun = "mean",
        survey_crs = 4326
      )
    ),
    "cropland" = list(
      description = "Download cropland data",
      required = c("shp_dt"),
      optional = list(
        source = "WorldCover",
        shp_fn = NULL,
        grid_size = 1000,
        survey_dt = NULL,
        survey_fn = NULL,
        survey_lat = NULL,
        survey_lon = NULL,
        buffer_size = NULL,
        extract_fun = "mean",
        survey_crs = 4326
      )
    ),
    "worldclim" = list(
      description = "Download WorldClim climate data",
      required = c("iso_code", "var", "res", "shp_dt"),
      optional = list(
        shp_fn = NULL,
        grid_size = 1000,
        survey_dt = NULL,
        survey_fn = NULL,
        survey_lat = NULL,
        survey_lon = NULL,
        buffer_size = NULL,
        extract_fun = "mean",
        survey_crs = 4326
      )
    ),
    "opencellid" = list(
      description = "Download cell tower data from OpenCellID",
      required = c("cell_tower_file", "shp_dt"),
      optional = list(
        grid_size = 1000,
        survey_dt = NULL,
        survey_fn = NULL
      )
    ),
    "terraclimate" = list(
      description = "Download TerraClimate data",
      required = c("var", "year", "shp_dt"),
      optional = list(
        shp_fn = NULL,
        grid_size = 1000,
        survey_dt = NULL,
        survey_fn = NULL,
        survey_lat = NULL,
        survey_lon = NULL,
        buffer_size = NULL,
        extract_fun = "mean",
        survey_crs = 4326
      )
    )
  )

  # Validate input data type
  if (!(data_type %in% names(param_suggestions))) {
    stop(paste("Invalid data_type. Choose from:",
               paste(names(param_suggestions), collapse = ", ")))
  }

  # Print parameter suggestions
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

  # Get the target function
  target_function <- data_functions[[data_type]]

  # Collect all arguments
  all_args <- list(...)

  # Validate and add date parameters
  if (!is.null(start_date)) {
    if (!inherits(try(as.Date(start_date), silent = TRUE), "Date")) {
      stop("start_date must be in a valid date format (YYYY-MM-DD)")
    }
    all_args$start_date <- start_date
  }

  if (!is.null(end_date)) {
    if (!inherits(try(as.Date(end_date), silent = TRUE), "Date")) {
      stop("end_date must be in a valid date format (YYYY-MM-DD)")
    }
    all_args$end_date <- end_date
  }

  # Validate and add spatial parameters
  if (!is.null(shp_dt)) {
    if (!inherits(shp_dt, "sf")) {
      stop("shp_dt must be an sf object")
    }
    all_args$shp_dt <- shp_dt
  }

  if (!is.null(survey_dt)) {
    if (!inherits(survey_dt, "sf") && !is.data.frame(survey_dt)) {
      stop("survey_dt must be an sf object or data frame")
    }
    all_args$survey_dt <- survey_dt
  }

  # Add ISO code if provided
  if (!is.null(iso_code)) {
    if (!is.character(iso_code) || nchar(iso_code) != 3) {
      stop("iso_code must be a 3-character ISO country code")
    }
    all_args$iso_code <- iso_code
  }

  # Verify required parameters are present
  required_params <- param_suggestions[[data_type]]$required
  missing_params <- required_params[!required_params %in% names(all_args)]
  if (length(missing_params) > 0) {
    stop(paste("Missing required parameters for", data_type, ":",
               paste(missing_params, collapse = ", ")))
  }

  # Call the function with all arguments
  tryCatch({
    result <- do.call(target_function, all_args)
    return(result)
  }, error = function(e) {
    stop(paste("Error in extracting", data_type, "data:\n",
               "Original error:", conditionMessage(e)))
  })
}
