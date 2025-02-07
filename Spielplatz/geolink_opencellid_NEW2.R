library(sf)
library(data.table)
library(memoise)
library(haven)

# Helper function to read OpenCellID data with caching and spatial indexing
read_opencellid_data <- function(file_path) {
  if (!grepl("\\.csv$|\\.csv\\.gz$", file_path)) {
    stop("Unsupported file format. Please provide a CSV file (plain or gzipped)")
  }

  # Read data without headers
  cell_towers <- fread(file_path, header = FALSE)

  # Assign proper column names
  colnames(cell_towers) <- c("radio", "mcc", "net", "area", "cell", "unit",
                             "lon", "lat", "range", "samples", "changeable",
                             "created", "updated", "averageSignal")

  # Ensure lon and lat are numeric
  cell_towers[, `:=`(
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  )]

  # Filter invalid coordinates
  cell_towers <- cell_towers[!is.na(lon) & !is.na(lat) &
                               lon >= -180 & lon <= 180 &
                               lat >= -90 & lat <= 90]

  return(cell_towers)
}

# Helper function to read survey data
read_survey_data <- function(file_path) {
  if (grepl("\\.dta$", file_path)) {
    # Read Stata file
    dt <- as.data.table(haven::read_dta(file_path))
  } else if (grepl("\\.csv$", file_path)) {
    # Read CSV file
    dt <- fread(file_path)
  } else {
    stop("Unsupported file format. Please provide .dta or .csv file")
  }
  return(dt)
}

geolink_opencellid <- function(cell_tower_file,
                               shp_dt = NULL,
                               shp_fn = NULL,
                               survey_dt = NULL,
                               survey_fn = NULL,
                               survey_lat = NULL,
                               survey_lon = NULL,
                               buffer_size = NULL,
                               survey_crs = 4326,
                               debug = TRUE) {

  log_debug <- function(msg) {
    if (debug) message(paste0("DEBUG: ", msg))
  }

  log_debug("Starting function execution")

  # Process input geometry
  if (!is.null(survey_dt)) {
    log_debug("Processing survey_dt")
    if (!inherits(survey_dt, "sf")) {
      # Check if there's a geometry column
      if ("geometry" %in% names(survey_dt)) {
        # Convert to sf object using existing geometry column
        sf_obj <- st_as_sf(survey_dt)
      } else if (!is.null(survey_lat) && !is.null(survey_lon)) {
        # Convert using coordinate columns if provided
        if (!(survey_lat %in% names(survey_dt)) || !(survey_lon %in% names(survey_dt))) {
          stop(sprintf("Coordinate columns '%s' and '%s' not found in survey data",
                       survey_lat, survey_lon))
        }

        # Convert to sf object efficiently
        sf_obj <- st_as_sf(survey_dt,
                           coords = c(survey_lon, survey_lat),
                           crs = survey_crs,
                           agr = "constant")
      } else {
        stop("Survey data must either have a geometry column or survey_lat/survey_lon must be provided")
      }
      original_data <- as.data.table(st_drop_geometry(survey_dt))
    } else {
      sf_obj <- survey_dt
      original_data <- as.data.table(st_drop_geometry(survey_dt))
    }
  } else if (!is.null(survey_fn)) {
    log_debug("Processing survey file")
    if (is.null(survey_lat) || is.null(survey_lon)) {
      stop("Both survey_lat and survey_lon column names must be provided when using survey_fn")
    }

    # Read survey file using appropriate method
    original_data <- read_survey_data(survey_fn)

    # Check if coordinate columns exist
    if (!(survey_lat %in% names(original_data)) || !(survey_lon %in% names(original_data))) {
      stop(sprintf("Coordinate columns '%s' and '%s' not found in survey file",
                   survey_lat, survey_lon))
    }

    # Convert to sf object efficiently
    sf_obj <- st_as_sf(original_data,
                       coords = c(survey_lon, survey_lat),
                       crs = survey_crs,
                       agr = "constant")
  } else if (!is.null(shp_dt)) {
    log_debug("Processing shapefile data")
    if (!inherits(shp_dt, "sf")) {
      stop("Input shp_dt must be an sf object")
    }
    sf_obj <- shp_dt
    original_data <- as.data.table(st_drop_geometry(shp_dt))
  } else if (!is.null(shp_fn)) {
    # Read shapefile
    sf_obj <- st_read(shp_fn, quiet = TRUE)
    original_data <- as.data.table(st_drop_geometry(sf_obj))
  } else {
    stop("Please provide either shapefile data or survey data with coordinate columns")
  }

  # Handle buffering if needed
  if (!is.null(buffer_size)) {
    sf_obj <- st_transform(sf_obj, 3857) %>%
      st_buffer(buffer_size) %>%
      st_transform(4326)
  }

  # Ensure CRS is 4326
  if (st_crs(sf_obj)$epsg != 4326) {
    sf_obj <- st_transform(sf_obj, 4326)
  }

  # Load cell tower data efficiently
  log_debug("Reading cell tower data")
  cell_towers <- read_opencellid_data_cached(cell_tower_file)
  log_debug(sprintf("Read %d cell towers", nrow(cell_towers)))

  # Convert cell towers to sf object efficiently
  log_debug("Converting cell towers to sf object")
  cell_towers_sf <- st_as_sf(cell_towers,
                             coords = c("lon", "lat"),
                             crs = 4326,
                             agr = "constant")

  # Create spatial index for efficiency
  cell_towers_sf <- st_sf(cell_towers_sf)

  # Get bounding box of input geometries
  bbox <- st_bbox(sf_obj)

  # Filter cell towers by bounding box first
  cell_towers_filtered <- cell_towers_sf[st_intersects(
    cell_towers_sf,
    st_as_sfc(bbox),
    sparse = FALSE
  )[,1], ]

  # Count towers per geometry using filtered dataset
  log_debug("Counting towers per geometry")
  num_towers <- lengths(st_intersects(sf_obj, cell_towers_filtered, sparse = TRUE))

  # Add tower counts to original data
  original_data[, num_towers := num_towers]

  return(original_data)
}
