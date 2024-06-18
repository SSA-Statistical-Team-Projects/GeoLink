
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
#' @import parallelMap doParallel
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
#' @import parallelMap doParallel
#' @export


zonalstats_prepsurvey <- function(survey_dt,
                                  survey_fn = NULL,
                                  survey_lat = NULL,
                                  survey_lon = NULL,
                                  buffer_size = NULL,
                                  survey_crs){

  ### read in stata file and convert to an sf/data.frame obj

  if (is.null(survey_fn) == FALSE) {

    survey_dt <- haven::read_dta(survey_fn)

    ### we are assuming it is crs 4326 but there is no systematic
    ### way of automating the process of knowing
    survey_dt <- st_as_sf(survey_dt,
                          coords = c(survey_lon, survey_lat),
                          crs = survey_crs)

  }


  if (is.null(buffer_size) == FALSE) {

        if (!("m" %in% st_crs(survey_dt)$units)) {

          suggest_dt <- crsuggest::suggest_crs(survey_dt,
                                               units = "m")

          survey_dt <- st_transform(survey_dt,
                                    crs = as.numeric(suggest_dt$crs_code[1]))

        }


    survey_dt <- sf::st_buffer(survey_dt,
                               dist = buffer_size)



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


#' A function to compute zonal statistics from multiple rasters into one shapefile
#' object
#'
#' @param shp_dt an sf/dataframe with polygons
#' @param raster_objs a list of raster objects to be extracted from
#' @param extract_fun function to be extracted
#' @param name_set a nameset which will be created for the variables
#'
#'
#' @details A parallelization option will be added
#' some point
#'
#' @export


compute_zonalstats <- function(shp_dt,
                               raster_objs,
                               extract_fun,
                               name_set){

  ### reproject shapefile to match raster CRS if they are not the same
  print("Extracting raster/vector data into shapefile")

  raster_crs_proj4 <- projection(raster_objs[[1]], asText = TRUE)

  shp_dt <- st_transform(shp_dt, crs = raster_crs_proj4)

  shp_dt <-
    mapply(FUN = function(x, n){
      ### first ensure raster and shapefile have the same crs


      shp_dt[[n]] <- exactextractr::exact_extract(x = x,
                                                  y = shp_dt,
                                                  fun = extract_fun)

      return(shp_dt)

    },
    SIMPLIFY = FALSE,
    x = raster_objs,
    n = name_set)

  if (length(shp_dt) > 1L) {

    shp_dt <- lapply(shp_dt,
                     function(X){

                       X$geoID <- 1:nrow(X)

                       return(X)

                     })

    geoid_dt <- shp_dt[[1]][, c("geoID")]

    shp_crs <- st_crs(shp_dt[[1]])$input

    shp_dt <- lapply(shp_dt,
                     function(X){

                       X <- X %>% st_drop_geometry()

                       return(X)

                     })

    shp_dt <- Reduce(merge, shp_dt)

    shp_dt <- merge(shp_dt, as.data.frame(geoid_dt), by = "geoID")

    shp_dt <- st_as_sf(shp_dt, crs = shp_crs, agr = "constant")

  } else if (length(shp_dt) == 1L){

    shp_dt <- shp_dt[[1]]

  }

  shp_dt <- st_as_sf(x = shp_dt,
                     crs = st_crs(raster_objs[[1]])$input,
                     agr = "constant")

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
                                   survey_dt,
                                   survey_fn = NULL,
                                   survey_lat = NULL,
                                   survey_lon = NULL,
                                   extract_fun = "mean",
                                   buffer_size = NULL,
                                   survey_crs = 4326,
                                   name_set,
                                   shp_dt,
                                   shp_fn = NULL,
                                   grid_size = 1000){


  #### ------ create the required survey and shapefile frames ------- ####

  if (!is.null(shp_fn)) {
    shp_dt <- zonalstats_prepshp(shp_fn = shp_fn,
                                 grid_size = grid_size)
  } else if (!missing(shp_dt)){
    shp_dt <- zonalstats_prepshp(shp_dt = shp_dt,
                                 grid_size = grid_size)
  }


  if (!missing(survey_dt) | !is.null(survey_fn)) {

    survey_dt <- zonalstats_prepsurvey(survey_dt = survey_dt,
                                       survey_fn = survey_fn,
                                       survey_lat = survey_lat,
                                       survey_lon = survey_lon,
                                       buffer_size = buffer_size,
                                       survey_crs = survey_crs)

  }


  #### ------- begin extraction in objects as necessary -------- ####

  ##### extract raster into buffered survey

  if (!is.null(buffer_size)){

    # survey_dt <- st_transform(survey_dt,
    #                           crs = st_crs(raster_objs[[1]])$input)

    survey_dt <- compute_zonalstats(shp_dt = survey_dt,
                                    raster_objs = raster_objs,
                                    extract_fun = extract_fun,
                                    name_set = name_set)

    # survey_dt <- st_as_sf(survey_dt,
    #                       crs = st_crs(raster_objs[[1]]$input),
    #                       agr = "constant")

    return(survey_dt)

  }


  ##### extract raster into shapefile for survey

  if (!is.null(shp_dt)){

    # shp_dt <- st_transform(shp_dt,
    #                        crs = st_crs(raster_objs[[1]])$input)

    shp_dt <- compute_zonalstats(shp_dt = shp_dt,
                                 raster_objs = raster_objs,
                                 extract_fun = extract_fun,
                                 name_set = name_set)

    # shp_dt <- st_as_sf(shp_dt,
    #                    crs = st_crs(raster_objs[[1]]$input),
    #                    agr = "constant")

  }


  if (!missing(survey_dt)) {

    shp_dt <- st_as_sf(shp_dt,
                       crs = st_crs(raster_objs[[1]])$input)

    survey_dt <- st_as_sf(survey_dt,
                          crs = st_crs(raster_objs[[1]])$input)

    survey_dt <- st_join(survey_dt, shp_dt)

    return(survey_dt)

  }

  return(shp_dt)

}







