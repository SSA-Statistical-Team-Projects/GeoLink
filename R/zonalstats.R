
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
                                 append_cols,
                                 full_colnames,
                                 numCores) {
  ## x <- chirpsresnd
  ## y <- adminunitsf


  ### parallelization processing
  numCores <- parallel::detectCores()
  parallelMap::parallelLibrary("foreach")
  parallelMap::parallelLibrary("raster")
  parallelMap::parallelLibrary("sf")
  parallelMap::parallelLibrary("exactextractr")

  doParallel::registerDoParallel(cores = numCores) ## initiate the number of cores to be used

  numparts <- ceiling(nrow(y) / numCores)

  y$part <- rep(1:numCores,
    each = numparts,
    length.out = nrow(y)
  )

  y <- split(y, y$part)

  results_dt <-
    foreach(i = 1:numCores) %dopar% {
      ### adminunit <<- as.polygons(adminunitraster, values=TRUE, na.rm=TRUE, dissolve=FALSE)
      ### usergrid <- rast(resolution = usergridres, val=0, crs="+proj=longlat +datum=WGS84")
      ### adminunitraster <<- rasterize(adminunit, usergrid, field="layer", filename="adminunit.tif",
      ### overwrite=TRUE, NAflag=3.40282346639e+038, datatype="FLT4S")

      exactextractr::exact_extract(
        x = x,
        y = y[[i]],
        fun = c("min", "max", "count", "sum", "mean", "median", "stdev"),
        append_cols = c("layer"),
        full_colnames = TRUE
      )
    }

  endCluster()

  results_dt2 <- do.call(rbind, results_dt)
  results_dt2 <- results_dt2[order(results_dt2$layer), ]
  setDT(results_dt2)

  ## results_dt <- unlist(results_dt, use.names = TRUE)


  return(results_dt2)
}
