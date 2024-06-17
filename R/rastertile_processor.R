################################################################################
####### A SET OF FUNCTIONS FOR HANDLING TILED RASTERS FROM MPC AND MORE .. #####
################################################################################

### function to convert raster to as.data.table object

#' Convert a raster to data.table object
#'
#' This function acts just like the `raster::as.data.frame()` object
#' @param x an object of class `raster`.
#' @inheritParams raster::as.data.frame()
#'
#' @export

as.data.table.raster <- function(x,
                                 row.names = NULL,
                                 optional = FALSE,
                                 xy = FALSE,
                                 inmem = raster::canProcessInMemory(x, 2),
                                 ...) {

  stopifnot(require("data.table"))
  if(inmem) {
    v <- as.data.table(as.data.frame(x, row.names=row.names, optional=optional, xy=xy, ...))
  } else {
    tr <- blockSize(x, n=2)
    l <- lapply(1:tr$n, function(i)
      as.data.table(as.data.frame(getValues(x,
                                            row=tr$row[i],
                                            nrows=tr$nrows[i]),
                                  row.names=row.names, optional=optional, xy=xy, ...)))
    v <- rbindlist(l)
  }
  coln <- names(x)
  if(xy) coln <- c("x", "y", coln)
  setnames(v, coln)

  return(v)

}


#' This function will crop and then rask rasters to the extent of the shapefile
#'
#' @param raster_objs a list of rasters (typically the tiles that were read in)
#' @param dt the shapefile `sf` object
#' @param numCores the number of cores to be used, set to NULL if only 1 raster
#'
#' @export
#'

filter_tiles <- function(raster_objs,
                         dt = shp_dt,
                         numCores = NULL){

  filter_worker <- function(x,
                            dt = dt){

    ### make sure shp_dt and x have the same crs
    if (st_crs(dt)$wkt != st_crs(x)$wkt){

      dt <- dt %>%
        st_transform(crs = st_crs(x)$wkt)

    }

    y <- crop(x, extent(dt))

    y <- mask(y, dt)

    return(y)

  }

  if (is.null(numCores) == FALSE) {

    ### parallelization processing when numCores is not NULL
    numCores <- min(numCores, parallel::detectCores())
    parallelMap::parallelLibrary("foreach")
    parallelMap::parallelLibrary("raster")
    parallelMap::parallelLibrary("terra")
    parallelMap::parallelLibrary("exactextractr")

    doParallel::registerDoParallel(cores = numCores)

    raster_objs <-
      foreach(i = 1:numCores) %dopar% {

        filter_worker(x = raster_objs[[i]])

      }

  } else {

    ### use this when we do not want to parallelize

    raster_objs <-
      lapply(X = raster_objs,
             FUN = filter_worker)


  }

  return(raster_objs)


}



























