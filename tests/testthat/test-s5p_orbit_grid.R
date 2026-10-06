# Sentinel-5P Level-2 files are orbit swaths: every pixel carries its own latitude and longitude, and
# the array order (scanline x ground pixel) says nothing about where a pixel is. .s5p_orbit_grid()
# must place each pixel by its own coordinates. These tests write a synthetic orbit in the layout of
# the real files (group PRODUCT with the indicator, qa_value, latitude and longitude, dimensions
# time x scanline x ground_pixel), with coordinates unrelated to the array order, and compare the
# grid with one computed independently by binning the pixels.

write_swath <- function(path, lat, lon, val, qa, layer = "ozone_total_vertical_column", qa_prec = "double") {
  nscan <- nrow(lat); npix <- ncol(lat)
  d_pix  <- ncdf4::ncdim_def("ground_pixel", "", seq_len(npix))
  d_scan <- ncdf4::ncdim_def("scanline", "", seq_len(nscan))
  d_time <- ncdf4::ncdim_def("time", "", 1L)
  dims <- list(d_pix, d_scan, d_time)
  vars <- list(
    lat = ncdf4::ncvar_def("PRODUCT/latitude", "degrees_north", dims, prec = "double"),
    lon = ncdf4::ncvar_def("PRODUCT/longitude", "degrees_east", dims, prec = "double"),
    val = ncdf4::ncvar_def(paste0("PRODUCT/", layer), "mol m-2", dims, missval = 9.96921e36, prec = "double"),
    qa  = ncdf4::ncvar_def("PRODUCT/qa_value", "1", dims, prec = qa_prec))
  nc <- ncdf4::nc_create(path, vars, force_v4 = TRUE)
  on.exit(ncdf4::nc_close(nc))
  put <- function(v, m) ncdf4::ncvar_put(nc, v, array(t(m), dim = c(npix, nscan, 1)))
  put(vars$lat, lat); put(vars$lon, lon); put(vars$val, val); put(vars$qa, qa)
  invisible(path)
}

## the expected grid, by plain binning: mean of the good pixels whose coordinates fall in each cell
bin_mean <- function(lat, lon, val, qa, qa_min, xmin, ymax, res, nrow, ncol) {
  ok <- qa >= qa_min & abs(val) < 1e30 & lon >= xmin & lon <= xmin + ncol * res & lat <= ymax & lat >= ymax - nrow * res
  col <- floor((lon[ok] - xmin) / res) + 1; row <- floor((ymax - lat[ok]) / res) + 1
  cell <- (row - 1) * ncol + col
  out <- rep(NA_real_, nrow * ncol)
  m <- tapply(val[ok], cell, mean)
  out[as.integer(names(m))] <- m
  out
}

make_orbit <- function(seed = 1, nscan = 12, npix = 5) {
  set.seed(seed)
  lat <- matrix(runif(nscan * npix, -0.3, 1.3), nscan)        # the study area is [0, 1] x [0, 1]
  lon <- matrix(runif(nscan * npix, -0.3, 1.3), nscan)
  lat[c(1, 2, nscan), ] <- runif(3 * npix, 40, 50)             # scanlines far from the study area
  val <- matrix(runif(nscan * npix, 0.10, 0.14), nscan)
  qa  <- matrix(sample(c(0.3, 0.6, 0.9), nscan * npix, replace = TRUE), nscan)
  val[5, 2] <- 9.96921e36; qa[5, 2] <- 1                       # a fill value with good qa
  list(lat = lat, lon = lon, val = val, qa = qa)
}

template <- function() terra::rast(terra::ext(0, 1, 0, 1), resolution = 0.25, crs = "EPSG:4326")

test_that("each pixel lands in the cell of its own coordinates, good-quality pixels only", {
  skip_if_not_installed("ncdf4")
  o <- make_orbit()
  f <- write_swath(tempfile(fileext = ".nc"), o$lat, o$lon, o$val, o$qa)
  g <- GeoLink:::.s5p_orbit_grid(f, layer = "ozone_total_vertical_column", qa_min = 0.5, template = template())
  expect_s4_class(g, "SpatRaster")
  expect_equal(c(terra::nrow(g), terra::ncol(g)), c(4L, 4L))
  expected <- bin_mean(o$lat, o$lon, o$val, o$qa, qa_min = 0.5, xmin = 0, ymax = 1, res = 0.25, nrow = 4, ncol = 4)
  expect_gt(sum(!is.na(expected)), 8)                           # the test covers most cells
  expect_equal(terra::values(g, mat = FALSE), expected, tolerance = 1e-12)
  ## the fill value and the low-quality pixels are excluded: every cell lies within the valid range
  expect_true(all(terra::values(g, mat = FALSE) <= 0.14, na.rm = TRUE))
})

test_that("qa_value stored unscaled (0 to 100) gives the same grid", {
  skip_if_not_installed("ncdf4")
  o <- make_orbit()
  f1 <- write_swath(tempfile(fileext = ".nc"), o$lat, o$lon, o$val, o$qa)
  f2 <- write_swath(tempfile(fileext = ".nc"), o$lat, o$lon, o$val, round(100 * o$qa), qa_prec = "integer")
  g1 <- GeoLink:::.s5p_orbit_grid(f1, "ozone_total_vertical_column", 0.5, template())
  g2 <- GeoLink:::.s5p_orbit_grid(f2, "ozone_total_vertical_column", 0.5, template())
  expect_equal(terra::values(g2, mat = FALSE), terra::values(g1, mat = FALSE))
})

test_that("placement follows the coordinates, not the array order", {
  skip_if_not_installed("ncdf4")
  o <- make_orbit(seed = 2)
  ## the same pixels in reversed scanline and ground-pixel order give the same grid
  rv <- function(m) m[nrow(m):1, ncol(m):1]
  f1 <- write_swath(tempfile(fileext = ".nc"), o$lat, o$lon, o$val, o$qa)
  f2 <- write_swath(tempfile(fileext = ".nc"), rv(o$lat), rv(o$lon), rv(o$val), rv(o$qa))
  g1 <- terra::values(GeoLink:::.s5p_orbit_grid(f1, "ozone_total_vertical_column", 0.5, template()), mat = FALSE)
  g2 <- terra::values(GeoLink:::.s5p_orbit_grid(f2, "ozone_total_vertical_column", 0.5, template()), mat = FALSE)
  expect_equal(g2, g1, tolerance = 1e-12)
  ## whereas stretching the array over the extent, as geolink_pollution() did before, does not
  r <- suppressWarnings(terra::rast(paste0('HDF5:"', normalizePath(f1, winslash = "/"), '"://PRODUCT/ozone_total_vertical_column')))
  terra::ext(r) <- c(0, 1, 0, 1)
  stretched <- terra::values(terra::resample(r, template(), method = "average"), mat = FALSE)
  expect_false(isTRUE(all.equal(stretched, g1, tolerance = 1e-6)))
})

test_that("an orbit that does not cross the study area returns NULL", {
  skip_if_not_installed("ncdf4")
  o <- make_orbit()
  o$lat[] <- runif(length(o$lat), 40, 50)
  f <- write_swath(tempfile(fileext = ".nc"), o$lat, o$lon, o$val, o$qa)
  expect_null(GeoLink:::.s5p_orbit_grid(f, "ozone_total_vertical_column", 0.5, template()))
})

test_that("geolink_pollution signs each orbit just before reading it, not all at the search", {
  ## A Planetary Computer token lasts about 45 minutes; reading 14 months over Nigeria takes
  ## longer, so URLs signed at the search expired before the later months were read (every
  ## orbit after that failed as "file does not exist"). Offline: STAC search, signer and reader
  ## are mocked; two months of two orbits each.
  events <- character()
  n_sign <- 0
  item <- function(id) list(assets = list(no2 = list(href = paste0("https://acc.blob.core.windows.net/c/", id, ".nc"))),
                            properties = list(`s5p:processing_mode` = "OFFL"))
  local_mocked_bindings(
    stac = function(...) "stac",
    stac_search = function(q, ...) q,
    get_request = function(q, ...) q,
    items_fetch = function(q, ...) list(features = list(item("a"), item("b"))),
    sign_planetary_computer = function(...) function(it) {
      n_sign <<- n_sign + 1
      events <<- c(events, paste0("sign", n_sign))
      it$assets$no2$href <- paste0(it$assets$no2$href, "?sig=", n_sign)
      it
    },
    .s5p_orbit_grid = function(href, layer, qa_min, template) {
      events <<- c(events, paste0("read", sub("^.*sig=", "", href)))
      terra::init(template, 0.1)
    })
  shp <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_polygon(list(
    rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))), crs = 4326))
  res <- NULL
  utils::capture.output(res <- suppressWarnings(suppressMessages(geolink_pollution(
    start_date = "2019-01-01", end_date = "2019-02-28", indicator = "no2", shp_dt = shp,
    grid_res = 0.25))))
  ## each read uses the signature made just before it
  expect_equal(events, c("sign1", "read1", "sign2", "read2", "sign3", "read3", "sign4", "read4"))
  expect_true(all(c("no2_y2019_m1", "no2_y2019_m2") %in% names(res)))
})
