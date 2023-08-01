#' Another function to create a gridified shapefile and extract a raster if specified
#'
#' This function takes in only a shapefile and creates a square or hexagon polygon grid based on a specified
#' grid size
#'
#' @param shp_dt an object of class 'sf' or 'sfc'
#' @param shp_dsn character; the local directory folder in which the shapefile is location. Must be specified
#' when shp_dt is not specified.
#' @param shp_layer character; the layer name for the shapefile. Must be specified with shp_dsn when shp_dt is not
#' specified
#' @param grid_size numeric of length 1; representing the desired size of the grid in meters
#' @param sqr logical; if TRUE, a square grid is created. If FALSE, a hexagonal polygon grid is created
#' @param pop_raster raster; an object of class 'raster'
#' @param raster_path character; if pop_raster is not specified but raster is to read in from file. raster_path is
#' the full name of the raster (including filepath)
#' @param extract_name character of length 1; the name of the indicator to be extracted from the raster
#' @param raster_function function to be applied in extracting raster into created grids
#'
#' @importFrom raster raster
#' @importFrom raster cellStats
#' @importFrom units set_units
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
  hist(x = grid_check,
       xlab = "Polygon Size (in km^2)",
       main = "Distribution of Polygon Size")

  print("The plot window should show you a distribution of the polygon sizes")

  return(grid_system)

}
