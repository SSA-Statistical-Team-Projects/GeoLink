## geolink_electaccess: HREA has one item per country and year. Every country's file must be
## kept and the countries of an (asset, year) merged before extraction. Offline: synthetic
## rasters, mocked STAC search and download.

## two bordering "countries" at the 1 degree meridian; each country's raster covers a bbox that
## reaches into the neighbour, where it is NA (as HREA country rasters are)
.ea_country_raster <- function(country) {
  if (country == "A") {
    r <- terra::rast(xmin = 0, xmax = 1.5, ymin = 0, ymax = 1, resolution = 0.1, crs = "EPSG:4326")
    terra::values(r) <- ifelse(terra::xFromCell(r, seq_len(terra::ncell(r))) < 1, 10, NA)
  } else {
    r <- terra::rast(xmin = 0.5, xmax = 3, ymin = 0, ymax = 1, resolution = 0.1, crs = "EPSG:4326")
    terra::values(r) <- ifelse(terra::xFromCell(r, seq_len(terra::ncell(r))) > 1, 20, NA)
  }
  r
}

.ea_assets <- c("lightscore", "light_composite", "night_proportion", "estimated_brightness")

.ea_url_list <- function() {
  lapply(c("A", "B"), function(cc) {
    item <- as.list(paste0("https://example.invalid/hrea/", cc, "/", .ea_assets, "_2019.tif"))
    names(item) <- .ea_assets
    c(item, list(year = 2019L, id = paste0("HREA_", cc, "_2019")))
  })
}

.ea_box <- function(x0, x1, y0, y1) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))))
}

.ea_polys <- function() {
  sf::st_sf(id = c("in_A", "in_B"),
            geometry = sf::st_sfc(.ea_box(0.2, 0.4, 0.2, 0.4), .ea_box(2.2, 2.4, 0.2, 0.4),
                                  crs = 4326))
}

test_that("every country's file is kept under its own name", {
  dest <- file.path(tempdir(), "ea_test_paths")
  dl <- GeoLink:::.electaccess_dest_paths(.ea_url_list(), dest)

  expect_equal(nrow(dl), 8)
  expect_equal(anyDuplicated(dl$path), 0)
  expect_setequal(dl$key, paste0(.ea_assets, "_2019"))
  expect_equal(as.vector(table(dl$key)), rep(2, 4))
  expect_true(all(grepl("HREA_A_2019|HREA_B_2019", basename(dl$path))))
})

test_that("the countries of an asset and year are merged before extraction", {
  dest <- file.path(tempdir(), "ea_test_combine")
  dir.create(dest, showWarnings = FALSE)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)
  dl <- GeoLink:::.electaccess_dest_paths(.ea_url_list(), dest)

  ## mocked download: country A's raster for A's items, B's for B's
  for (k in seq_len(nrow(dl))) {
    terra::writeRaster(.ea_country_raster(sub("HREA_(.)_2019", "\\1", dl$item[k])), dl$path[k],
                       overwrite = TRUE)
  }
  expect_true(all(file.exists(dl$path)))

  combined <- GeoLink:::.electaccess_combine(dl)
  expect_equal(names(combined), paste0(.ea_assets, "_2019"))

  r <- combined[["lightscore_2019"]]
  expect_equal(as.vector(terra::ext(r)), c(xmin = 0, xmax = 3, ymin = 0, ymax = 1))
  pts <- terra::vect(cbind(c(0.3, 2.3), c(0.3, 0.3)), crs = "EPSG:4326")
  expect_equal(terra::extract(r, pts)[[2]], c(10, 20))

  vals <- exactextractr::exact_extract(r, .ea_polys(), "mean", progress = FALSE)
  expect_equal(vals, c(10, 20))

  ## cropping to the study area keeps both countries
  combined_bb <- GeoLink:::.electaccess_combine(dl, bbox = c(xmin = 0.1, ymin = 0.1, xmax = 2.5, ymax = 0.5))
  expect_equal(terra::extract(combined_bb[["night_proportion_2019"]], pts)[[2]], c(10, 20))
})

test_that("geolink_electaccess end to end (mocked STAC and download) gives each polygon its country's value", {

  features <- lapply(.ea_url_list(), function(item) {
    hrefs <- unlist(item[.ea_assets])
    list(id = item$id,
         properties = list(datetime = "2019-01-01T00:00:00Z"),
         assets = setNames(lapply(hrefs, function(h) list(href = h)),
                           c("lightscore", "light-composite", "night-proportion",
                             "estimated-brightness")))
  })

  testthat::local_mocked_bindings(
    stac = function(...) "stac",
    stac_search = function(q, ...) q,
    get_request = function(q, ...) q,
    items_fetch = function(items, ...) items,
    items_sign = function(items, ...) list(features = features),
    sign_planetary_computer = function(...) NULL,
    .package = "GeoLink")
  testthat::local_mocked_bindings(
    GET = function(url, ...) {
      cfg <- Filter(function(x) !is.null(x$output$path), list(...))[[1]]
      country <- sub(".*/hrea/(.)/.*", "\\1", url)
      terra::writeRaster(.ea_country_raster(country), cfg$output$path, overwrite = TRUE)
      structure(list(url = url, status_code = 200L), class = "response")
    },
    .package = "httr")

  out <- suppressWarnings(suppressMessages(capture.output(
    res <- geolink_electaccess(start_date = "2019-01-01", end_date = "2019-12-31",
                               shp_dt = .ea_polys()))))

  expect_true(all(paste0(.ea_assets, "_2019") %in% names(res)))
  expect_equal(sum(grepl("^lightscore", names(res))), 1)
  expect_equal(res$lightscore_2019, c(10, 20))
  expect_equal(res$estimated_brightness_2019, c(10, 20))
})
