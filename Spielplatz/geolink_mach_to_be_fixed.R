

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
#' #pull annual land use land cover and combine with household survey based on
#' #grid tesselation of shapefile at 1000m
#'
#'     df <- geolink_landcover(
#'                          start_date = "2020-01-01",
#'                          end_date = "2021-01-01",
#'                          shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#'
#'
#' }
#'@export
#' @import  rstac reticulate terra raster osmdata  sf geodata httr ncdf4 exactextractr parallel
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
  unlink(tempdir(), recursive = TRUE)
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
#' df <- geolink_get_poi(osm_key = "amenity",
#'                       shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#'
#'}
#'@export


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
    shp_dt <- survey_dt  # Use survey_dt as shp_dt for OSM queries
  }

  # If shp_fn is provided, read shapefile
  if (!is.null(shp_fn)) {
    shp_dt <- st_read(shp_fn)
  }

  # Ensure either shp_dt or survey_dt is provided
  if (is.null(shp_dt)) {
    stop("Either shp_dt or survey_dt must be provided.")
  }

  # Validate shp_dt and ensure it's not empty
  if (nrow(shp_dt) == 0) {
    stop("shp_dt is empty after filtering. Please check the filter conditions.")
  }
  if (any(is.na(st_geometry(shp_dt)))) {
    stop("shp_dt contains invalid geometries. Please ensure all geometries are valid.")
  }

  # Query OpenStreetMap for POI data
  if (!is.null(survey_dt)) {
    # Use individual points from survey_dt for the OSM query
    points <- st_geometry(survey_dt)
    results_list <- list()

    for (point in points) {
      point_bbox <- st_bbox(point)

      # Use withCallingHandlers to suppress CRS and Bounding Box warnings
      datapull <- opq(c(bbox = point_bbox, timeout = 7200)) %>%
        add_osm_feature(key = osm_key, value = osm_value)

      # Suppress CRS and Bounding Box warnings during osmdata_sf call
      features <- withCallingHandlers({
        osmdata_sf(datapull)
      }, warning = function(w) {
        if (grepl("Bounding Box", conditionMessage(w)) || grepl("CRS is already EPSG:4326", conditionMessage(w))) {
          return(NULL)  # Ignore this specific warning
        }
        # Otherwise, re-throw the warning
        invokeRestart("muffleWarning")
      })

      if (!is.null(features)) {
        results_list[[length(results_list) + 1]] <- features$osm_points
      }
    }

    # Combine results from all points
    results <- do.call(rbind, results_list)
  } else {
    # Use bbox of shp_dt for the OSM query
    bbox <- st_bbox(shp_dt)
    if (any(is.na(bbox))) {
      stop("Bounding box contains NA values. Please ensure shp_dt has valid geometries.")
    }

    datapull <- opq(c(bbox = bbox, timeout = 7200)) %>%
      add_osm_feature(key = osm_key, value = osm_value)

    # Suppress warnings here as well using withCallingHandlers
    features <- withCallingHandlers({
      osmdata_sf(datapull)
    }, warning = function(w) {
      if (grepl("Bounding Box", conditionMessage(w)) || grepl("CRS is already EPSG:4326",
                                                              conditionMessage(w))) {
        return(NULL)  # Ignore this specific warning
      }
      # Otherwise, re-throw the warning
      invokeRestart("muffleWarning")
    })

    if (!is.null(features)) {
      results <- features$osm_points
    }
  }

  # Filter out points without meaningful data
  results <- results %>%
    filter(if_any(-c(osm_id, geometry), ~ !is.na(.x)))

  # Spatial join between results and shp_dt/survey_dt
  query_dt <- st_join(results, shp_dt, left = FALSE)

  # Handle case with no matching POI
  if (nrow(query_dt) == 0) {
    print("No points of interest")
  }

  # Optionally remove geometry for Stata compatibility

  if (stata) {

    query_dt <- query_dt[, !grepl("geometry", names(query_dt))]
  }

  # Log completion
  print("OpenStreetMap data downloaded.")
  print("Process complete!")



  print("Process Complete!!!")

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
#' @import rstac terra raster osmdata sf httr geodata
#'
#' @examples
#'\donttest{
#'
#'
#' df <- geolink_electaccess(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
#'         start_date = "2018-12-31", end_date = "2019-12-31")
#'
#' }
#'@export


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
#' @import rstac terra raster osmdata sf httr geodata data.table geosphere
#'
#' @examples
#' \donttest{
#'
#'  #example usage
#' results <- geolink_opencellid(cell_tower_file = "C:/Users/username/Downloads/621.csv.gz",
#'                              shp_dt = shp_dt)
#'
#'  results <- geolink_opencellid(cell_tower_file = "C:/Users/username/Downloads/621.csv.gz",
#'                                     survey_dt = hhgeo_dt[hhgeo_dt$ADM1_EN == "Abia",],
#'                                    shp_dt = NULL)
#'
#' }
#'@export


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
#' df <- geolink_get_poi(osm_key = "amenity",
#'                       shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])
#'
#'}
#'@export


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
    shp_dt <- survey_dt  # Use survey_dt as shp_dt for OSM queries
  }

  # If shp_fn is provided, read shapefile
  if (!is.null(shp_fn)) {
    shp_dt <- st_read(shp_fn)
  }

  # Ensure either shp_dt or survey_dt is provided
  if (is.null(shp_dt)) {
    stop("Either shp_dt or survey_dt must be provided.")
  }

  # Validate shp_dt and ensure it's not empty
  if (nrow(shp_dt) == 0) {
    stop("shp_dt is empty after filtering. Please check the filter conditions.")
  }
  if (any(is.na(st_geometry(shp_dt)))) {
    stop("shp_dt contains invalid geometries. Please ensure all geometries are valid.")
  }

  # Query OpenStreetMap for POI data
  if (!is.null(survey_dt)) {
    # Use individual points from survey_dt for the OSM query
    points <- st_geometry(survey_dt)
    results_list <- list()

    for (point in points) {
      point_bbox <- st_bbox(point)

      # Use withCallingHandlers to suppress CRS and Bounding Box warnings
      datapull <- opq(c(bbox = point_bbox, timeout = 7200)) %>%
        add_osm_feature(key = osm_key, value = osm_value)

      # Suppress CRS and Bounding Box warnings during osmdata_sf call
      features <- withCallingHandlers({
        osmdata_sf(datapull)
      }, warning = function(w) {
        if (grepl("Bounding Box", conditionMessage(w)) || grepl("CRS is already EPSG:4326", conditionMessage(w))) {
          return(NULL)  # Ignore this specific warning
        }
        # Otherwise, re-throw the warning
        invokeRestart("muffleWarning")
      })

      if (!is.null(features)) {
        results_list[[length(results_list) + 1]] <- features$osm_points
      }
    }

    # Combine results from all points
    results <- do.call(rbind, results_list)
  } else {
    # Use bbox of shp_dt for the OSM query
    bbox <- st_bbox(shp_dt)
    if (any(is.na(bbox))) {
      stop("Bounding box contains NA values. Please ensure shp_dt has valid geometries.")
    }

    datapull <- opq(c(bbox = bbox, timeout = 7200)) %>%
      add_osm_feature(key = osm_key, value = osm_value)

    # Suppress warnings here as well using withCallingHandlers
    features <- withCallingHandlers({
      osmdata_sf(datapull)
    }, warning = function(w) {
      if (grepl("Bounding Box", conditionMessage(w)) || grepl("CRS is already EPSG:4326",
                                                              conditionMessage(w))) {
        return(NULL)  # Ignore this specific warning
      }
      # Otherwise, re-throw the warning
      invokeRestart("muffleWarning")
    })

    if (!is.null(features)) {
      results <- features$osm_points
    }
  }

  # Filter out points without meaningful data
  results <- results %>%
    filter(if_any(-c(osm_id, geometry), ~ !is.na(.x)))

  # Spatial join between results and shp_dt/survey_dt
  query_dt <- st_join(results, shp_dt, left = FALSE)

  # Handle case with no matching POI
  if (nrow(query_dt) == 0) {
    print("No points of interest")
  }

  # Optionally remove geometry for Stata compatibility

  if (stata) {

    query_dt <- query_dt[, !grepl("geometry", names(query_dt))]
  }

  # Log completion
  print("OpenStreetMap data downloaded.")
  print("Process complete!")



  print("Process Complete!!!")

  return(query_dt)

}

