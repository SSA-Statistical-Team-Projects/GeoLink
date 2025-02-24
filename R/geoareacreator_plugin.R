#' Another function to create a gridified shapefile and extract a raster if specified
#'
#' This function takes in only a shapefile and creates a square or hexagon polygon grid based on a specified
#' grid size
#'
#' @param shp_dt an object of class 'sf' or 'sfc'
#' @param grid_size numeric of length 1; representing the desired size of the grid in meters
#' @param sqr logical; if TRUE, a square grid is created. If FALSE, a hexagonal polygon grid is created
#'
#' @importFrom raster raster
#' @importFrom raster cellStats
#' @importFrom units set_units
#' @import sf lwgeom
#'
#' @export


gengrid2 <- function(shp_dt,
                     grid_size,
                     sqr = TRUE) {

  sf_use_s2(FALSE) ##just to ensure we don't begin to have issues with duplicate vertices

  ## now we are ready to grid our district shapefile
  print("Initiating shape object tesselation")
  if (sqr == TRUE) {

    grid_system <- st_make_grid(x = shp_dt,
                                cellsize = c(grid_size, grid_size),
                                square = sqr) %>%
      sf::st_sf()

  } else if (sqr == FALSE) {

    grid_system <- st_make_grid(x = shp_dt,
                                cellsize = grid_size,
                                square = sqr) %>%
      sf::st_sf()


  }
  print("Tesselation complete for shapefile extent, ensuring validity of shapefile ...")
  ## the process creates squares with parts outside the area so we should take the intersection
  ## of the shapefile with our newly created grid

  ## to avoid failures we need to make sure geometries are valid
  shp_checklist <- st_is_valid(shp_dt)

  while(sum(shp_checklist) != length(shp_checklist)){

    shp_dt <- st_make_valid(shp_dt)
    shp_checklist <- st_is_valid(shp_dt)

  }
  print("Limiting tesselated object to shapefile area ...")

  ## figure out which grids belong within the shapefile
  grid_system$poly_id <- 1:nrow(grid_system)
  grid_system <- st_join(grid_system, shp_dt, left = F, largest = TRUE)

  ## compute area of duplicated grids and assign shp_dt areas

  print("The shapefile is fully gridded!!")

  ## make sure all geometries are polygons
  clean_geometry <- function(geo_dt){

    geo_dt <- geo_dt[st_dimension(geo_dt) == 2,] ##ensure that all geometries are surfaces
    ##find any other enclosure geometries
    add_dt <- geo_dt[st_geometry_type(geo_dt) == "GEOMETRYCOLLECTION",]
    add_dt <- st_collection_extract(add_dt)
    add_dt <- add_dt[st_dimension(add_dt) == 2,]
    geo_dt <- geo_dt[!(st_geometry_type(geo_dt) == "GEOMETRYCOLLECTION"),]

    geo_dt <- rbind(geo_dt, add_dt)


    return(geo_dt)

  }

  grid_system <- clean_geometry(grid_system)

  print("Ensuring geometries are properly fixed")
  grid_system$poly_id <- 1:nrow(grid_system)

  grid_system$poly_area <- st_area(grid_system) ##compute area of each square
  grid_system$poly_area <- set_units(grid_system$poly_area, "km^2")

  grid_system <- grid_system[as.numeric(grid_system$poly_area) > 0,]

  print(paste0("The tesselated object represents a total area of ",
               round(sum(grid_system$poly_area, na.rm = TRUE),2),
               " km^2"))

  grid_check <- as.numeric(grid_system$poly_area)

  print("The plot window should show you a distribution of the polygon sizes")

  return(grid_system)

}


#' A function to create an osmdata package readable bounding box (bbox)
#' with a buffer distance
#'
#' @param shp_dt `sf`, `data.frame`, a shapefile object with polygon geometry
#' @param area_name character, the geographic name of the area of interest
#' @param buffer_dist numeric, a numeric vector of length 4 for the distance from
#' xmin, xmax, ymin and ymax to be added to the extent of `shp_dt`
#' @param metric_crs logical, default is FALSE. If TRUE, `shp_dt` posseses a metric
#' crs.
#' @param osm_crs integer, the integer coordinate reference system for the OSM object
#'
#' @export
#' @importFrom osmdata getbb
#' @importFrom sf st_bbox
#' @importFrom crsuggest suggest_crs
#' @importFrom raster extent
#' @importFrom dbscan dbscan
#' @import dplyr

create_query_bbox <- function(shp_dt = NULL,
                              area_name,
                              buffer_dist = c(0, 0, 0, 0),
                              metric_crs = FALSE,
                              osm_crs = 4326){

  if (is.null(shp_dt)){

    bbox_obj <- getbb(area_name)

    bbox_obj <- sf::st_bbox(raster::extent(bbox_obj),
                            crs = osm_crs)

    if (is.null(buffer_dist) == FALSE){

      ### convert to metric scale
      bbox_obj <- sf::st_as_sfc(x = bbox_obj,
                                crs = osm_crs)

      suggest_dt <- crsuggest::suggest_crs(bbox_obj, units = "m")

      bbox_obj <- st_transform(bbox_obj,
                               crs = as.numeric(suggest_dt$crs_code[1]))

      bbox_obj <- st_bbox(bbox_obj)

    }

  } else {

    if (metric_crs == FALSE) {

      suggest_dt <- crsuggest::suggest_crs(st_as_sfc(st_bbox(shp_dt)),
                                           units = "m")

      bbox_obj <- st_transform(st_as_sfc(st_bbox(shp_dt)),
                               crs = as.numeric(suggest_dt$crs_code[1]))
    } else {

      bbox_obj <- st_as_sfc(st_bbox(shp_dt))

    }

    bbox_obj <- sf::st_bbox(bbox_obj)

  }

  #### add buffer dist
  if (is.null(buffer_dist) == FALSE){

    bbox_obj[1] <- bbox_obj[1] - buffer_dist[1]
    bbox_obj[2] <- bbox_obj[2] - buffer_dist[2]
    bbox_obj[3] <- bbox_obj[3] + buffer_dist[3]
    bbox_obj[4] <- bbox_obj[4] + buffer_dist[4]


    ### recreate an st_as_sfc readable object
    if (metric_crs == TRUE) {

      bbox_obj <- sf::st_bbox(raster::extent(bbox_obj),
                              crs = st_crs(shp_dt))

    } else {

      bbox_obj <- sf::st_bbox(raster::extent(bbox_obj),
                              crs = as.numeric(suggest_dt$crs_code[1]))

    }

    bbox_obj <- st_as_sfc(bbox_obj)

    bbox_obj <- st_transform(bbox_obj,
                             crs = osm_crs)

    bbox_obj <- sf::st_bbox(bbox_obj)

    ## convert to osm_bbox type
    bbox_obj <- matrix(c(bbox_obj[[1]],
                         bbox_obj[[3]],
                         bbox_obj[[2]],
                         bbox_obj[[4]]),
                       ncol = 2,
                       byrow = TRUE)

    colnames(bbox_obj) <- c("min", "max")
    rownames(bbox_obj) <- c("x", "y")


  }

  return(bbox_obj)


}

