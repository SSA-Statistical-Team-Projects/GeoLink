## geolink_landcover with python = FALSE: all tiles of a year, in different UTM zones, are
## combined in R; polygons not covered by any tile get NA shares. Offline: synthetic tiles.

.lc_values <- c(0, 1, 2, 4, 5, 7, 8, 9, 10, 11)
.lc_names  <- c("no_data", "water", "trees", "flooded_vegetation", "crops", "built_area",
                "bare_ground", "snow/ice", "clouds", "rangeland")

## a categorical tile in UTM zone `epsg` covering lon x lat, 100 m pixels, all `value`;
## north of `nodata_lat` (if given) the class is 0 ("No Data")
.lc_tile <- function(lon, lat, epsg, value, nodata_lat = NULL) {
  poly <- sf::st_as_sfc(sf::st_bbox(c(xmin = lon[1], xmax = lon[2], ymin = lat[1], ymax = lat[2]),
                                    crs = sf::st_crs(4326)))
  bb <- sf::st_bbox(sf::st_transform(poly, epsg))
  r <- terra::rast(xmin = floor(bb[["xmin"]] / 100) * 100, xmax = ceiling(bb[["xmax"]] / 100) * 100,
                   ymin = floor(bb[["ymin"]] / 100) * 100, ymax = ceiling(bb[["ymax"]] / 100) * 100,
                   resolution = 100, crs = paste0("EPSG:", epsg))
  terra::values(r) <- value
  if (!is.null(nodata_lat)) {
    y_cut <- sf::st_coordinates(sf::st_transform(
      sf::st_sfc(sf::st_point(c(mean(lon), nodata_lat)), crs = 4326), epsg))[, "Y"]
    r <- terra::ifel(terra::init(r, "y") > y_cut, 0, r)
  }
  r
}

.lc_box <- function(x0, x1, y0, y1) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))))
}

.lc_cells <- function() {
  sf::st_sf(
    cell = c("west", "east", "straddle", "outside", "west_north"),
    geometry = sf::st_sfc(
      .lc_box(5.2, 5.4, 10.10, 10.30),   # only tile 1 (zone 31)
      .lc_box(6.6, 6.8, 10.10, 10.30),   # only tile 2 (zone 32)
      .lc_box(5.9, 6.1, 10.10, 10.30),   # both tiles
      .lc_box(8.0, 8.2, 10.10, 10.30),   # no tile
      .lc_box(5.2, 5.4, 10.30, 10.50),   # tile 1, half of it class 0 (no data)
      crs = 4326))
}

test_that("tiles in different UTM zones are all used and combined on one EPSG:4326 grid", {

  tile1 <- .lc_tile(c(5, 6), c(10, 10.5), 32631, value = 2, nodata_lat = 10.4)  # trees
  tile2 <- .lc_tile(c(6, 7), c(10, 10.5), 32632, value = 7)                      # built area
  f1 <- tempfile(fileext = ".tif"); f2 <- tempfile(fileext = ".tif")
  terra::writeRaster(tile1, f1, datatype = "INT1U")
  terra::writeRaster(tile2, f2, datatype = "INT1U")
  on.exit(unlink(c(f1, f2)), add = TRUE)

  mosaic <- GeoLink:::.landcover_combine_tiles(c(f1, f2), target_resolution = 1000,
                                               bbox = sf::st_bbox(.lc_cells()) + c(-0.1, -0.1, 0.1, 0.1))

  expect_true(terra::same.crs(mosaic, "EPSG:4326"))
  expect_equal(terra::res(mosaic), rep(1000 / 111320, 2), tolerance = 1e-9)
  pts <- terra::vect(cbind(c(5.5, 6.5, 8.1, 5.5), c(10.2, 10.2, 10.2, 10.45)), crs = "EPSG:4326")
  expect_equal(terra::extract(mosaic, pts)[[2]], c(2, 7, NA, NA))   # class 0 becomes NA

  shares <- GeoLink:::.landcover_class_shares(mosaic, .lc_cells(), .lc_values, .lc_names)

  expect_equal(names(shares), .lc_names)
  ## west cell: tile 1 only
  expect_equal(shares$trees[1], 100)
  expect_equal(shares$built_area[1], 0)
  expect_equal(shares$no_data[1], 0)
  ## east cell: tile 2 (the second tile) is used, its class comes through
  expect_equal(shares$built_area[2], 100)
  expect_equal(shares$trees[2], 0)
  expect_equal(shares$no_data[2], 0)
  ## the straddling cell gets both tiles
  expect_equal(shares$trees[3], 50, tolerance = 0.1)
  expect_equal(shares$built_area[3], 50, tolerance = 0.1)
  expect_equal(shares$no_data[3], 0)
  ## outside every tile: NA, not 0
  expect_true(all(is.na(unlist(shares[4, setdiff(.lc_names, "no_data")]))))
  expect_equal(shares$no_data[4], 100)
  ## half of the cell is class 0
  expect_equal(shares$trees[5], 50, tolerance = 0.1)
  expect_equal(shares$no_data[5], 50, tolerance = 0.1)
  expect_equal(shares$trees[5] + shares$no_data[5], 100, tolerance = 1e-6)
})

test_that("without resampling the tiles keep their native resolution", {

  tile1 <- .lc_tile(c(5, 6), c(10, 10.5), 32631, value = 2)
  tile2 <- .lc_tile(c(6, 7), c(10, 10.5), 32632, value = 5)
  bbox <- c(xmin = 5.85, ymin = 10.1, xmax = 6.15, ymax = 10.3)
  mosaic <- GeoLink:::.landcover_combine_tiles(list(tile1, tile2), target_resolution = NULL,
                                               bbox = bbox)
  expect_equal(terra::res(mosaic), rep(100 / 111320, 2), tolerance = 1e-9)
  expect_equal(sort(unique(terra::values(mosaic)[, 1])), c(2, 5))

  ## a study area that no tile covers
  expect_null(GeoLink:::.landcover_combine_tiles(list(tile1, tile2), target_resolution = 1000,
                                                 bbox = c(xmin = 20, ymin = 0, xmax = 21, ymax = 1)))
})
