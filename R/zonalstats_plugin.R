
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
