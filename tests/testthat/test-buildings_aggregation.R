## geolink_buildings: per-layer aggregation of WorldPop building-pattern rasters
## (pixels without buildings are NoData), column-name protection and the download folder.
## Offline: synthetic rasters and a mocked download.

## 4 x 4 pixels of 100 m in UTM 31N; row 1 is the top row
.bld_raster <- function(vals) {
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 500000, xmax = 500400,
                   ymin = 1000000, ymax = 1000400, crs = "EPSG:32631")
  terra::values(r) <- vals
  r
}

.bld_square <- function(x0, x1, y0, y1) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))))
}

## built pixels: (1,1) (1,2) (1,4) (2,2) (2,3) (3,3)
.bld_count <- c(1,  2,  NA, 4,
                NA, 3,  5,  NA,
                NA, NA, 6,  NA,
                NA, NA, NA, NA)
.bld_layers <- list(
  count      = .bld_count,
  total_area = .bld_count * 50,
  density    = .bld_count * 10,
  urban      = c(0, 0, NA, 1,  NA, 1, 1, NA,  NA, NA, 1, NA,  NA, NA, NA, NA),
  mean_area  = c(40, 60, NA, 80,  NA, 100, 120, NA,  NA, NA, 140, NA,  NA, NA, NA, NA)
)

.bld_grid <- function() {
  sf::st_sf(
    cell  = c("A", "B", "C", "D"),
    urban = c("Yes", "No", "No", "No"),
    geometry = sf::st_sfc(
      .bld_square(500000, 500200, 1000200, 1000400),  # A: 2 x 2 pixels, fully covered
      .bld_square(500150, 500350, 1000200, 1000400),  # B: half pixels on both sides
      .bld_square(500000, 500200, 1000000, 1000200),  # C: no buildings
      .bld_square(600000, 600100, 1000000, 1000100),  # D: outside the rasters
      crs = 32631))
}

test_that("totals are coverage-weighted sums, shares are zero-filled means, attributes are means over built pixels", {

  rasters <- lapply(.bld_layers, .bld_raster)
  expect_message(
    out <- GeoLink:::.buildings_zonalstats(shp_dt = .bld_grid(),
                                           raster_objs = unname(rasters),
                                           extract_fun = NULL,
                                           name_set = names(.bld_layers)),
    "urban_buildings")

  ## count: A = 1+2+3; B = 0.5*2 + 0.5*4 + 0.5*3 + 1*5 (partial pixels split); C = 0, not NaN
  expect_equal(out$count, c(6, 9.5, 0, NA))
  expect_equal(out$total_area, c(300, 475, 0, NA))

  ## density: mean over the whole cell with no-building pixels = 0
  expect_equal(out$density, c((10 + 20 + 30) / 4, (10 + 20 + 15 + 50) / 4, 0, NA))
  ## urban: share of the cell that is urban
  expect_equal(out$urban_buildings, c(1 / 4, (0.5 + 0.5 + 1) / 4, 0, NA))

  ## per-building attribute: mean over built pixels only, NA where there is no building
  expect_equal(out$mean_area,
               c((40 + 60 + 100) / 3, (0.5 * 60 + 0.5 * 80 + 0.5 * 100 + 120) / 2.5, NA, NA),
               tolerance = 1e-6)

  ## the input urban column survives
  expect_equal(out$urban, c("Yes", "No", "No", "No"))
  expect_false(any(c("count_buildings", "density_buildings") %in% names(out)))
})

test_that("postdownload_processor uses the buildings aggregation", {

  rasters <- lapply(.bld_layers[c("count", "urban")], .bld_raster)
  out <- suppressMessages(
    postdownload_processor(shp_dt = .bld_grid(), raster_objs = unname(rasters),
                           extract_fun = NULL, name_set = c("count", "urban"),
                           grid_size = NULL, return_raster = FALSE, weight_raster = NULL,
                           zonalstats_fun = GeoLink:::.buildings_zonalstats))
  expect_equal(out$count, c(6, 9.5, 0, NA))
  expect_equal(out$urban, c("Yes", "No", "No", "No"))
  expect_equal(out$urban_buildings, c(0.25, 0.5, 0, NA))
})

test_that("a user-chosen extract_fun applies to every layer and still keeps input columns", {

  rasters <- lapply(.bld_layers[c("count", "urban")], .bld_raster)
  out <- suppressMessages(
    GeoLink:::.buildings_zonalstats(shp_dt = .bld_grid(), raster_objs = unname(rasters),
                                    extract_fun = "max", name_set = c("count", "urban")))
  expect_equal(out$count[1:2], c(3, 5))
  expect_equal(out$urban, c("Yes", "No", "No", "No"))
  expect_equal(out$urban_buildings[1:2], c(1, 1))
})

test_that("layer names are taken from the WorldPop file names", {
  expect_equal(GeoLink:::.buildings_layer_names(
    c("x/NGA_buildings_v1_1_count.tif", "NGA_buildings_v1_1_cv_area.tif",
      "NGA_buildings_v2_0_total_length.tif")),
    c("count", "cv_area", "total_length"))
})

test_that("only the rasters of this download are used, and failed downloads stop", {

  skip_if_not_installed("zip")

  ## a stray raster in the session temp folder must not be picked up
  stray <- file.path(tempdir(), "stray_layer.tif")
  terra::writeRaster(.bld_raster(.bld_count), stray, overwrite = TRUE)
  on.exit(unlink(stray), add = TRUE)

  src <- file.path(tempfile("bld_src_")); dir.create(src)
  terra::writeRaster(.bld_raster(.bld_count), file.path(src, "TST_buildings_v1_1_count.tif"))
  zipfile <- file.path(src, "TST_buildings_v1_1.zip")
  zip::zip(zipfile, "TST_buildings_v1_1_count.tif", root = src)

  dest <- file.path(tempdir(), "geolink_buildings_TST_v1.1")
  testthat::local_mocked_bindings(
    GET = function(url, config, ...) {
      file.copy(zipfile, config$output$path, overwrite = TRUE)
      structure(list(url = url, status_code = 200L), class = "response")
    },
    .package = "httr")
  tifs <- suppressMessages(GeoLink:::.buildings_download("v1.1", "TST", dest))
  expect_equal(basename(tifs), "TST_buildings_v1_1_count.tif")
  expect_equal(normalizePath(dirname(tifs)), normalizePath(dest))
  unlink(dest, recursive = TRUE)

  ## HTTP error
  testthat::local_mocked_bindings(
    GET = function(url, config, ...) structure(list(url = url, status_code = 404L), class = "response"),
    .package = "httr")
  expect_error(suppressMessages(GeoLink:::.buildings_download("v1.1", "TST", dest)), "HTTP status 404")

  ## network failure
  testthat::local_mocked_bindings(
    GET = function(url, config, ...) stop("could not resolve host"),
    .package = "httr")
  expect_error(GeoLink:::.buildings_download("v1.1", "TST", dest), "could not resolve host")

  ## not a zip archive
  testthat::local_mocked_bindings(
    GET = function(url, config, ...) {
      writeLines("<html>not found</html>", config$output$path)
      structure(list(url = url, status_code = 200L), class = "response")
    },
    .package = "httr")
  expect_error(suppressMessages(GeoLink:::.buildings_download("v1.1", "TST", dest)), "Unzipping")
  unlink(dest, recursive = TRUE)
})
