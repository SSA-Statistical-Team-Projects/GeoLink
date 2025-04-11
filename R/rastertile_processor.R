################################################################################
####### A SET OF FUNCTIONS FOR HANDLING TILED RASTERS FROM MPC AND MORE .. #####
################################################################################

#' Convert a raster to data.table object
#'
#' @param x An object of class `Raster*`, typically from the `raster` package.
#' @param keep.rownames Logical. If `TRUE`, include row names as a column in the output.
#' @param xy (optional) Logical. If `TRUE`, include coordinates of raster cells in the output.
#' @param inmem (optional) Logical. If `TRUE`, process the raster in memory. Default is determined by `raster::canProcessInMemory()`.

#' @param ... Additional arguments passed to methods.

#'
#' @importFrom data.table as.data.table
#' @inheritParams raster::as.data.frame
#'
#' @export
as.data.table.raster <- function(x,
                                 keep.rownames = FALSE,
                                 xy = FALSE,
                                 inmem = raster::canProcessInMemory(x, 2),
                                 ...) {
  if(inmem) {
    # Pass keep.rownames to as.data.frame, but may need to adapt based on raster::as.data.frame parameters
    v <- as.data.table(as.data.frame(x, row.names=NULL, optional=FALSE, xy=xy, ...),
                       keep.rownames=keep.rownames)
  } else {
    tr <- blockSize(x, n=2)
    l <- lapply(1:tr$n, function(i)
      as.data.table(as.data.frame(getValues(x,
                                            row=tr$row[i],
                                            nrows=tr$nrows[i]),
                                  row.names=NULL, optional=FALSE, xy=xy, ...),
                    keep.rownames=keep.rownames))
    v <- rbindlist(l)
  }
  coln <- names(x)
  if(xy) coln <- c("x", "y", coln)
  setnames(v, coln)
  return(v)
}


#' This function will crop and then mask rasters to the extent of the shapefile
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


#' @import terra sf exactextractr

# Function to filter and reproject tiles
filter_tiles_landcover <- function(raster_objs, dt, numCores = NULL) {

  filter_worker <- function(x, dt) {

    # Ensure dt and x have the same CRS by transforming dt if necessary
    if (terra::crs(x) != terra::crs(dt)) {
      dt <- st_transform(dt, crs = terra::crs(x))
    }

    y <- terra::crop(x, terra::ext(dt))
    y <- terra::mask(y, dt)

    return(y)
  }

  if (!is.null(numCores)) {

    # Parallelization processing when numCores is not NULL
    numCores <- min(numCores, parallel::detectCores())
    parallelMap::parallelLibrary("foreach")
    parallelMap::parallelLibrary("raster")
    parallelMap::parallelLibrary("terra")
    parallelMap::parallelLibrary("exactextractr")

    doParallel::registerDoParallel(cores = numCores)

    raster_objs <- foreach(i = 1:numCores) %dopar% {
      filter_worker(x = raster_objs[[i]], dt = dt)
    }

  } else {
    # Use this when we do not want to parallelize
    raster_objs <- lapply(raster_objs, filter_worker, dt = dt)
  }

  return(raster_objs)
}



#' A function convert point data to raster
#'
#' @param point_sf `sf`, `data.frame` object with point geometries, the first
#' column is assumed contain the intended raster's field value.
#' @param crs integer or crs projection string, default is EPSG:4326
#' @param resolution numeric, the value of the raster's intended resolution
#' @param agg_fun the aggregation function (e.g. sum, mean etc)
#'
#' @details Ensure, the `sf`, `data.frame` object contains the field value first and
#' then the geometry and `resolution` of `point_dt` is in the same units as `crs`.
#' i.e. a degree resolution only works correctly if `point_dt` has a `crs` in
#' degrees and likewise for a metric resolution. In addition, if the point_dt
#' comes as a `data.frame` object with latitude and longitude, it must be
#' converted into an `sf`, `data.frame` by using the `sf::st_as_sf()` function.
#' See help(sf::st_as_sf) for more details on the conversion process. In
#' specifying the `crs`, simply using the 4 digit number will provide an error.
#' See default for example.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' \donttest{
#'
#'  #example usage with shapefile
#'  raster_obj <- point_sf_to_raster(point_sf = point_dt,
#'                                   crs = "EPSG:4326",
#'                                   resolution = 0.1)
#'
#' }}
#'@export


point_sf_to_raster <- function(point_sf,
                               crs = "EPSG:4326",
                               resolution,
                               agg_fun = sum){

  ### ensure point_df is an sf, data.frame
  if(!inherits(point_sf, "sf")) {

    stop("point_dt must be an point geometry sf")

  }

  coords <- st_coordinates(point_sf)
  df <- st_drop_geometry(point_sf)

  # Create a SpatRaster template based on extent and resolution
  raster_obj <- rast(ext(min(coords[,1]), max(coords[,1]),
                         min(coords[,2]), max(coords[,2])),
                     resolution = resolution,
                     crs = crs)

  # Convert to SpatVector
  points_vect <- vect(data.frame(coords, df),
                      geom = c("X", "Y"),
                      crs = crs)

  # Rasterize using aggregation function
  raster_obj <- rasterize(points_vect,
                          raster_obj,
                          field = names(df)[1],
                          fun = agg_fun)

  return(raster_obj)

}














