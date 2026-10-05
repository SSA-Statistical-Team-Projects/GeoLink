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

## geolink_landcover() end to end, offline: the STAC search and the downloads are mocked and
## return two synthetic tiles of one year in different UTM zones, so the whole merge path of
## the function runs (download, one raster per year from all tiles, class shares per polygon).

.lc_mock_stac <- function(tile_files) {
  features <- lapply(tile_files, function(f) {
    list(properties = list(start_datetime = "2023-01-01T00:00:00Z"),
         assets = list(data = list(href = f)))
  })
  list(stac = function(...) "stac",
       stac_search = function(q, ...) q,
       get_request = function(q, ...) q,
       items_fetch = function(q, ...) q,
       items_sign = function(q, ...) list(features = features),
       sign_planetary_computer = function(...) NULL)
}

## httr::GET copies the "URL" (a local tile) to the requested path
.lc_mock_httr <- function() {
  list(write_disk = function(path, overwrite = FALSE) path,
       GET = function(url, path, ...) {
         file.copy(url, path, overwrite = TRUE)
         structure(list(status_code = 200L), class = "response")
       })
}

.lc_write_tiles <- function() {
  tile1 <- .lc_tile(c(5, 6), c(10, 10.5), 32631, value = 2, nodata_lat = 10.4)  # trees
  tile2 <- .lc_tile(c(6, 7), c(10, 10.5), 32632, value = 7)                      # built area
  f <- c(tempfile(fileext = ".tif"), tempfile(fileext = ".tif"))
  terra::writeRaster(tile1, f[1], datatype = "INT1U")
  terra::writeRaster(tile2, f[2], datatype = "INT1U")
  f
}

## the class shares of .lc_cells() when both tiles are merged (see the first test)
.lc_expect_merged <- function(res) {
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 5)
  expect_equal(res$year, rep("2023", 5))
  expect_equal(res$trees[1], 100)                         # tile 1 only
  expect_equal(res$built_area[2], 100)                    # tile 2 only: the second tile is used
  expect_equal(res$trees[3], 50, tolerance = 0.1)         # both tiles
  expect_equal(res$built_area[3], 50, tolerance = 0.1)
  expect_true(is.na(res$trees[4]))                        # no tile
  expect_equal(res$no_data[4], 100)
  expect_equal(res$no_data[5], 50, tolerance = 0.1)       # half class 0
}

test_that("geolink_landcover (python = FALSE) merges all tiles of a year end to end", {
  tiles <- .lc_write_tiles()
  on.exit(unlink(tiles), add = TRUE)
  do.call(local_mocked_bindings, .lc_mock_stac(tiles))
  do.call(local_mocked_bindings, c(.lc_mock_httr(), .package = "httr"))

  res <- suppressMessages(utils::capture.output(out <- geolink_landcover(
    start_date = "2023-01-01", end_date = "2023-12-31", shp_dt = .lc_cells(),
    use_resampling = TRUE, target_resolution = 1000)))
  .lc_expect_merged(out)
})

test_that("geolink_landcover (python = TRUE) calls the sourced Python utilities with every tile", {
  tiles <- .lc_write_tiles()
  on.exit(unlink(tiles), add = TRUE)
  do.call(local_mocked_bindings, .lc_mock_stac(tiles))
  do.call(local_mocked_bindings, c(.lc_mock_httr(), .package = "httr"))

  ## stand-ins for the Python functions of inst/python_scripts/raster_utils.py, defined where
  ## reticulate::source_python() is asked to define them; they record their calls
  calls <- new.env()
  fake_source <- function(file, envir = parent.frame(), ...) {
    expect_equal(basename(file), "raster_utils.py")
    assign("resample_rasters", function(input_files, output_folder, target_resolution, ...) {
      calls$resample <- input_files
      as.list(input_files)
    }, envir = envir)
    assign("mosaic_rasters", function(input_files, output_file = NULL) {
      calls$mosaic <- input_files
      m <- GeoLink:::.landcover_combine_tiles(input_files, target_resolution = 1000)
      out <- tempfile(fileext = ".tif")
      terra::writeRaster(m, out)
      out
    }, envir = envir)
    invisible(NULL)
  }
  local_mocked_bindings(geolink_setup_python = function(...) invisible(TRUE))
  local_mocked_bindings(use_condaenv = function(...) invisible(TRUE),
                        py_run_string = function(...) invisible(NULL),
                        source_python = fake_source, .package = "reticulate")

  res <- suppressMessages(utils::capture.output(out <- geolink_landcover(
    start_date = "2023-01-01", end_date = "2023-12-31", shp_dt = .lc_cells(),
    use_resampling = TRUE, target_resolution = 1000, python = TRUE)))
  expect_length(calls$mosaic, 2)                          # both tiles reach the mosaic
  .lc_expect_merged(out)

  ## a failing Python mosaic falls back to the R merge with a warning, not to the first tile
  fake_source_fail <- function(file, envir = parent.frame(), ...) {
    fake_source(file, envir)
    assign("mosaic_rasters", function(input_files, output_file = NULL) stop("rasterio error"),
           envir = envir)
  }
  local_mocked_bindings(source_python = fake_source_fail, .package = "reticulate")
  expect_warning(
    res <- suppressMessages(utils::capture.output(out <- geolink_landcover(
      start_date = "2023-01-01", end_date = "2023-12-31", shp_dt = .lc_cells(),
      use_resampling = TRUE, target_resolution = 1000, python = TRUE))),
    "Python mosaic failed")
  .lc_expect_merged(out)
})
