
#' Compute zonal statistics in-parallel
#'
#' A function to split a shapefile into many parts and computes the zonal statistics
#' for each part in parallel to speed up the zonal statistics estimations
#'
#' @param x a `RasterLayer`, `RasterStack`, `RasterBrick`, or `SpatRaster` with data
#' to be extracted from
#' @param y a `sf`, `sfc`, `SpatialPolygonsDataFrame`, or `SpatialPolygons` object
#' with polygonal geometries to be extracted into
#' @param fun the function to be used compute zonal statistics
#' @param numCores the number of cores to be used in parallel processing
#'
#' @import parallelMap doParallel foreach raster sf exactextractr
#' @export


parallel_zonalstats <- function(x,
                                y,
                                fun,
                                numCores){

  ### parallelization processing
  numCores <- min(numCores, parallel::detectCores())
  parallelMap::parallelLibrary("foreach")
  parallelMap::parallelLibrary("raster")
  parallelMap::parallelLibrary("sf")
  parallelMap::parallelLibrary("exactextractr")

  doParallel::registerDoParallel(cores = numCores) ##initiate the number of cores to be used

  numparts <- ceiling(nrow(y) / numCores)

  y$part <- rep(1:numCores,
                each = numparts,
                length.out = nrow(y))

  y <- split(y, y$part)

  results_dt <-
    foreach(i = 1:numCores) %dopar% {

      exactextractr::exact_extract(x = x,
                                   y = y[[i]],
                                   fun = fun)


    }

  endCluster()

  results_dt <- unlist(results_dt)


  return(results_dt)

}

#' A function to process raster downloads and compute zonal statistics for shapefiles
#' and geocoded surveys
#'
#' @param survey_dt a sf/data.frame object, the household survey with point geometry
#' @param survey_fn a character, the filename of the survey (.dta file) (STATA users only)
#' @param survey_lat a character, for latitude variable name in survey_fn
#' (STATA users only)
#' @param survey_lon a character, for longitude variable name in survey_fn
#' (STATA users only)
#' @param survey_dt a sf/data.frame object, the geocoded survey
#' @param buffer_size a numeric, radius of buffer for survey_dt (or survey_fn) units.
#' @param survey_crs an integer or character, the CRS for the survey
#'
#' @import parallelMap doParallel
#' @export


zonalstats_prepsurvey <- function(survey_dt,
                                  survey_fn = NULL,
                                  survey_lat = NULL,
                                  survey_lon = NULL,
                                  buffer_size = NULL,
                                  survey_crs = NULL){

  if (!is.null(survey_fn)) {
    survey_dt <- haven::read_dta(survey_fn)
    survey_dt <- st_as_sf(survey_dt,
                          coords = c(survey_lon, survey_lat),
                          crs = survey_crs)
  }

  if (!is.null(survey_dt)) {
    if ("geometry" %in% names(survey_dt) && !"sf" %in% class(survey_dt)) {
      survey_dt <- st_as_sf(survey_dt, crs = survey_crs)
    }
    else if (!"sf" %in% class(survey_dt)) {
      if (!is.null(survey_lat) && !is.null(survey_lon)) {
        if (survey_lon %in% names(survey_dt) && survey_lat %in% names(survey_dt)) {
          survey_dt <- st_as_sf(survey_dt,
                                coords = c(survey_lon, survey_lat),
                                crs = survey_crs,
                                remove = FALSE)
        } else {
          stop(paste("Columns", survey_lon, "and/or", survey_lat, "not found in survey_dt"))
        }
      } else {
        stop("survey_dt is not an sf object, has no geometry column, and survey_lat/survey_lon are not specified.")
      }
    }

    if ("sf" %in% class(survey_dt) && is.na(st_crs(survey_dt))) {
      st_crs(survey_dt) <- survey_crs
    }
  }

  if (!is.null(buffer_size) && !is.null(survey_dt)) {
    if (!("m" %in% st_crs(survey_dt)$units)) {
      suggest_dt <- crsuggest::suggest_crs(survey_dt, units = "m")
      survey_dt <- st_transform(survey_dt,
                                crs = as.numeric(suggest_dt$crs_code[1]))
    }

    survey_dt <- sf::st_buffer(survey_dt, dist = buffer_size)

    survey_dt <- ensure_crs_4326(survey_dt)
  }

  return(survey_dt)
}



#' A function to process raster downloads and compute zonal statistics for shapefiles
#' and geocoded surveys
#'
#' @param shp_dt a sf/data.frame object, the shapefile
#' @param shp_fn a character, the name of shapefile for STATA users only (.shp/.gpkg file)
#' @param grid_size a numeric, size of the size of the grid in meters
#'
#' @export

zonalstats_prepshp <- function(shp_dt,
                               shp_fn = NULL,
                               grid_size){

  if (is.null(shp_fn) == FALSE){

    shp_dt <- sf::read_sf(shp_fn)

  }


  if (is.null(grid_size) == FALSE){


    if (!("m" %in% st_crs(shp_dt)$units)) {

      suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                           units = "m")

      shp_dt <- st_transform(shp_dt,
                             crs = as.numeric(suggest_dt$crs_code[1]))

    }

    shp_dt <- gengrid2(shp_dt = shp_dt,
                       grid_size = grid_size)


  }

  return(shp_dt)

}


#' A function to harmonize the weight raster with the raster to be extracted
#'
#' This function will check and resample the weighting raster to match the data
#' raster which is to be extracted from. This step is necessary before zonal
#' statistics estimation that requires the use of weights
#'
#' @param raster_objs a list of rasters
#' @param weight_raster a list of weighting rasters of length 1 or the equal in length to
#' `raster_objs`
#' @param method see `terra::resample` for details
#'
#' @importFrom terra compareGeom resample
#'
#'

harmonize_weights <- function(raster_objs,
                              weight_raster,
                              method = "bilinear"){

  ### ensure the length of data raster and weighting raster objects are the same

  if (length(raster_objs) != length(weight_raster) & length(weight_raster) != 1){
    stop("raster_objs and weight_raster must be the same length")
  }

  ## ensure weighting raster(s) is/are the same length as raster_objs
  if (length(raster_objs) > 1 & length(weight_raster) == 1){

    weight_raster <- replicate(length(raster_objs),
                               weight_raster,
                               simplify = FALSE)


  }

  ### creating an empty vector the length of weighting raster
  aligned_weights <- vector("list", length(weight_raster))

  ### ensure the raster types adjustments
  for (i in seq_along(raster_objs)) {

    r_main <- raster_objs[[i]]
    r_weight <- weight_raster[[i]]

    if (!inherits(r_main, "SpatRaster") || !inherits(r_weight, "SpatRaster")) {

      warning(paste("Item", i, "is not a SpatRaster; skipping."))

      aligned_weights[[i]] <- NA

      next

    }

    # Compare geometry: resolution, extent, CRS, alignment
    if (terra::compareGeom(r_main, r_weight, stopOnError = FALSE)) {

      aligned_weights[[i]] <- r_weight  # Already aligned

    } else {

      message(paste("Resampling weights raster for item", i, "to match geometry."))

      aligned_weights[[i]] <- terra::resample(r_weight, r_main, method = method)

    }
  }

  return(aligned_weights)


}




#' A function to compute zonal statistics from multiple rasters into one shapefile
#' object
#'
#' @param shp_dt an sf/dataframe with polygons
#' @param raster_objs a list of raster objects to be extracted from
#' @param extract_fun function to be extracted
#' @param name_set a nameset which will be created for the variables
#' @param weight_raster a raster object or a list of weighting rasters or the
#' equal in length to `raster_objs`
#'
#'
#' @details A parallelization option will be added at
#' some point
#'
#' @export



compute_zonalstats <- function(shp_dt,
                               raster_objs,
                               extract_fun,
                               name_set,
                               weight_raster) {
  # Validate inputs
  if (is.null(shp_dt) || is.null(raster_objs)) {
    warning("Input shapefile or raster objects are NULL. Cannot proceed.")
    return(NULL)
  }

  # Ensure shapefile is in sf format
  if (!inherits(shp_dt, "sf")) {
    tryCatch({
      shp_dt <- sf::st_as_sf(shp_dt)
    }, error = function(e) {
      stop("Cannot convert input to sf object: ", e$message)
    })
  }

  # Check raster objects
  if (length(raster_objs) == 0) {
    warning("No raster objects provided.")
    return(shp_dt)
  }

  # Validate name_set
  if (length(name_set) != length(raster_objs)) {
    stop("Length of name_set must match number of raster objects")
  }

  # Print extraction message
  print("Extracting raster/vector data into shapefile")

  # Get CRS and ensure all objects are raster package objects
  all_raster_objs <- lapply(raster_objs, function(r) {
    if (inherits(r, "SpatRaster")) {
      raster::raster(r)
    } else {
      r
    }
  })

  raster_crs <- raster::crs(all_raster_objs[[1]])
  print(paste("CRS of raster objects:", raster_crs@projargs))

  # Transform shapefile to raster CRS
  tryCatch({
    shp_dt <- sf::st_transform(shp_dt, crs = raster_crs)
  }, error = function(e) {
    warning("Could not transform shapefile CRS, continuing anyway: ", e$message)
  })

  ### use weights if weight_raster is specified
  if (!is.null(weight_raster)){

    # Ensure weight_raster is in the correct format
    if (is.list(weight_raster)) {
      weight_raster_use <- weight_raster[[1]]
    } else {
      weight_raster_use <- weight_raster
    }

    # Convert to raster if it's terra
    if (inherits(weight_raster_use, "SpatRaster")) {
      weight_raster_use <- raster::raster(weight_raster_use)
    }

    # Check if weight raster needs resampling
    if (!raster::compareRaster(all_raster_objs[[1]], weight_raster_use,
                               extent = FALSE, rowcol = FALSE,
                               crs = TRUE, res = FALSE,
                               stopiffalse = FALSE)) {
      print("Resampling weight raster to match data raster...")
      weight_raster_use <- raster::resample(weight_raster_use, all_raster_objs[[1]], method = "bilinear")
    }

    # Extract values for each raster with weights
    for (i in seq_along(all_raster_objs)) {
      print(paste("Extracting", name_set[i], "with weighted", extract_fun))

      tryCatch({
        # For weighted mean, we might need to handle it differently
        if (extract_fun == "weighted_mean" || extract_fun == "weighted.mean") {
          # Extract both values and weights
          values <- exactextractr::exact_extract(all_raster_objs[[i]], shp_dt)
          weights <- exactextractr::exact_extract(weight_raster_use, shp_dt)

          # Calculate weighted mean manually for each polygon
          result <- sapply(1:length(values), function(j) {
            val_df <- values[[j]]
            wgt_df <- weights[[j]]

            # Align the data
            if (is.data.frame(val_df) && is.data.frame(wgt_df)) {
              # Get the value and weight columns
              val_col <- names(val_df)[1]  # First column is usually the value
              wgt_col <- names(wgt_df)[1]

              # Calculate weighted mean
              valid_idx <- !is.na(val_df[[val_col]]) & !is.na(wgt_df[[wgt_col]])
              if (sum(valid_idx) > 0) {
                sum(val_df[[val_col]][valid_idx] * wgt_df[[wgt_col]][valid_idx]) /
                  sum(wgt_df[[wgt_col]][valid_idx])
              } else {
                NA
              }
            } else {
              NA
            }
          })

          shp_dt[[name_set[i]]] <- result

        } else {
          # Use exact_extract with weights parameter
          shp_dt[[name_set[i]]] <- exactextractr::exact_extract(
            x = all_raster_objs[[i]],
            y = shp_dt,
            fun = extract_fun,
            weights = weight_raster_use
          )
        }

        print(paste("Successfully extracted", name_set[i],
                    "- sample values:", paste(head(shp_dt[[name_set[i]]], 3), collapse = ", ")))

      }, error = function(e) {
        warning(paste("Could not extract values for", name_set[i], ":", e$message))
        shp_dt[[name_set[i]]] <- NA
      })
    }

  } else {
    # Extract values for each raster without weights
    for (i in seq_along(all_raster_objs)) {
      print(paste("Extracting", name_set[i], "with", extract_fun))

      tryCatch({
        shp_dt[[name_set[i]]] <- exactextractr::exact_extract(
          x = all_raster_objs[[i]],
          y = shp_dt,
          fun = extract_fun
        )

        print(paste("Successfully extracted", name_set[i],
                    "- sample values:", paste(head(shp_dt[[name_set[i]]], 3), collapse = ", ")))

      }, error = function(e) {
        warning(paste("Could not extract values for", name_set[i], ":", e$message))
        shp_dt[[name_set[i]]] <- NA
      })
    }
  }

  return(shp_dt)
}

#' A function to process downloaded raster into the final output in shapefile/geocoded survey
#'
#' @inheritParams compute_zonalstats
#' @inheritParams zonalstats_prepsurvey
#' @param shp_fn a character, the name of shapefile for STATA users only (.shp/.gpkg file)
#' @param grid_size a numeric, size of the size of the grid in meters
#'
#' @export

postdownload_processor <- function(raster_objs,
                                   survey_dt = NULL,
                                   survey_fn = NULL,
                                   survey_lat = NULL,
                                   survey_lon = NULL,
                                   extract_fun = "mean",
                                   buffer_size = NULL,
                                   survey_crs = 4326,
                                   name_set,
                                   shp_dt = NULL,
                                   shp_fn = NULL,
                                   grid_size = 1000,
                                   return_raster,
                                   weight_raster) {

  # Check if return_raster is TRUE early and return immediately
  if (return_raster == TRUE) {
    if (!is.null(shp_dt) || !is.null(shp_fn)) {
      # If shapefile data is provided, crop the raster
      if (!is.null(shp_fn)) {
        shp_dt <- zonalstats_prepshp(shp_fn = shp_fn, grid_size = grid_size)
      } else if (!is.null(shp_dt)) {
        shp_dt <- zonalstats_prepshp(shp_dt = shp_dt, grid_size = grid_size)
      }
      raster_objs <- lapply(X = raster_objs,
                            FUN = raster::crop,
                            y = shp_dt)
    }
    return(raster_objs)
  }

  # Continue with the rest of the function only if return_raster is FALSE
  # Create the required survey and shapefile frames
  if (!is.null(shp_fn)) {
    shp_dt <- zonalstats_prepshp(shp_fn = shp_fn, grid_size = grid_size)
  }
  if (!is.null(shp_dt)) {
    shp_dt <- zonalstats_prepshp(shp_dt = shp_dt, grid_size = grid_size)
  }

  # Process survey data only if it's not NULL
  if (!is.null(survey_dt) || !is.null(survey_fn)) {
    # Check if survey_dt has CRS, if not assign the survey_crs
    if (!is.null(survey_dt)) {
      # Check if it's already an sf object
      if (!"sf" %in% class(survey_dt)) {
        # If lat/lon columns are provided, create sf object
        if (!is.null(survey_lat) && !is.null(survey_lon)) {
          survey_dt <- sf::st_as_sf(survey_dt,
                                    coords = c(survey_lon, survey_lat),
                                    crs = survey_crs,
                                    remove = FALSE)
        }
      } else {
        # If it's already sf but missing CRS, assign it
        if (is.na(sf::st_crs(survey_dt))) {
          sf::st_crs(survey_dt) <- survey_crs
        }
      }
    }

    # Safely prepare survey data
    survey_dt <- tryCatch({
      zonalstats_prepsurvey(
        survey_dt = survey_dt,
        survey_fn = survey_fn,
        survey_lat = survey_lat,
        survey_lon = survey_lon,
        buffer_size = buffer_size,
        survey_crs = survey_crs
      )
    }, error = function(e) {
      warning("Error preparing survey data: ", e$message)
      return(NULL)
    })

    # Ensure survey data is in correct CRS
    if (!is.null(survey_dt)) {
      sf_obj <- ensure_crs_4326(survey_dt)
    }
  } else if (!is.null(shp_dt)) {
    sf_obj <- ensure_crs_4326(shp_dt)
  } else if (!is.null(shp_fn)) {
    sf_obj <- sf::read_sf(shp_fn)
    sf_obj <- ensure_crs_4326(sf_obj)
  } else {
    print("Input a valid sf object or geosurvey")
    sf_obj <- NULL
  }

  # Extract raster into shapefile
  if (!is.null(shp_dt)) {
    shp_dt <- compute_zonalstats(
      shp_dt = shp_dt,
      raster_objs = raster_objs,
      extract_fun = extract_fun,
      name_set = name_set,
      weight_raster = weight_raster
    )
  }

  # Process survey data if it exists and has buffer
  if (!is.null(survey_dt)) {
    if (!is.null(buffer_size)) {
      survey_dt <- compute_zonalstats(
        shp_dt = survey_dt,
        raster_objs = raster_objs,
        extract_fun = extract_fun,
        name_set = name_set,
        weight_raster = weight_raster
      )
    }

    ### adding some lines to ensure output is returned for STATA users
    if (!is.null(shp_fn)) {
      return(shp_dt %>% st_drop_geometry())
    }
    if (!is.null(survey_fn)) {
      return(survey_dt %>% st_drop_geometry())
    }

    # Only attempt st_as_sf if survey_dt is not NULL
    tryCatch({
      # Ensure survey_dt has CRS before proceeding
      if (is.na(sf::st_crs(survey_dt))) {
        survey_dt <- sf::st_set_crs(survey_dt, sf::st_crs(raster_objs[[1]])$input)
      }
      survey_dt <- sf::st_as_sf(survey_dt, crs = sf::st_crs(raster_objs[[1]])$input)
      return(survey_dt)
    }, error = function(e) {
      warning("Error processing survey data: ", e$message)
      return(shp_dt)
    })
  }

  return(shp_dt)
}
