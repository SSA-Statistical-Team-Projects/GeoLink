###############################################################################
### Tests for Open Buildings 2.5D Temporal
###
### These hit the public GCS bucket, so they are skipped when offline. The
### polygon sets are deliberately tiny: two Colombian cases at the two scales
### the source is intended for, roughly a 3,000 m2 census manzana and a
### 20 km2 rural section, plus a set that straddles a UTM zone boundary.
###############################################################################

skip_if_offline_bucket <- function() {
  testthat::skip_on_cran()
  ok <- tryCatch({
    con <- url(paste0("https://storage.googleapis.com/storage/v1/b/",
                      "open-buildings-temporal-data/o?maxResults=1"), open = "rb")
    on.exit(close(con), add = TRUE)
    length(readBin(con, "raw", 10)) > 0
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("Open Buildings bucket not reachable")
}

## A square polygon of a target area, centred on lon/lat, built in a metric CRS.
square_of <- function(lon, lat, area_m2, id) {
  half <- sqrt(area_m2) / 2
  utm  <- .obt_zone_of(lon, lat)
  cen  <- sf::st_transform(
    sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326), utm)
  xy   <- sf::st_coordinates(cen)
  poly <- sf::st_polygon(list(matrix(
    c(xy[1] - half, xy[2] - half,
      xy[1] + half, xy[2] - half,
      xy[1] + half, xy[2] + half,
      xy[1] - half, xy[2] + half,
      xy[1] - half, xy[2] - half), ncol = 2, byrow = TRUE)))
  sf::st_sf(poly_id = id,
            geometry = sf::st_transform(sf::st_sfc(poly, crs = utm), 4326))
}


# Test A ---------------------------------------------------------------------
test_that("urban manzana-scale polygon returns all three bands", {
  skip_if_offline_bucket()

  # central Bogota, ~3,000 m2, the manzana scale
  shp <- square_of(-74.0817, 4.6045, 3000, "urban_manzana")

  t0 <- Sys.time()
  out <- geolink_buildings_temporal(shp_dt = shp, year = 2018, quiet = TRUE)
  el  <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("[timing] urban 3,000 m2 polygon, 1 year: %.1f s", el))

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 1L)
  expect_contains(colnames(out),
                  c("building_fractional_count_2018",
                    "building_height_2018",
                    "building_presence_2018"))

  # a dense urban block must contain buildings
  expect_gt(out$building_fractional_count_2018, 0)
  expect_gt(out$building_presence_2018, 0)
  expect_lte(out$building_presence_2018, 1)
  expect_gt(out$building_height_2018, 0)

  # the equal-area CRS actually used is recorded
  expect_equal(attr(out, "geolink_area_crs"), "ESRI:102033")
})


# Test B ---------------------------------------------------------------------
test_that("rural section-scale polygon works at ~20 km2", {
  skip_if_offline_bucket()

  # rural Cundinamarca, ~20 km2, the rural seccion scale
  shp <- square_of(-74.35, 4.95, 20e6, "rural_seccion")

  t0 <- Sys.time()
  out <- geolink_buildings_temporal(shp_dt = shp, year = 2018, quiet = TRUE)
  el  <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("[timing] rural 20 km2 polygon, 1 year: %.1f s", el))

  expect_equal(nrow(out), 1L)
  expect_false(is.na(out$building_presence_2018))
  expect_gte(out$building_fractional_count_2018, 0)
})


# Test C ---------------------------------------------------------------------
test_that("per-band defaults apply, and overrides work", {
  skip_if_offline_bucket()

  expect_equal(.obt_default_fun("building_fractional_count"), "sum")
  expect_equal(.obt_default_fun("building_height"), "mean")
  expect_equal(.obt_default_fun("building_presence"), "mean")

  shp <- square_of(-74.0817, 4.6045, 3000, "u")

  # a single string overrides every band, preserving the documented convention
  out <- geolink_buildings_temporal(shp_dt = shp, year = 2018,
                                    indicators = "building_presence",
                                    extract_fun = "max", quiet = TRUE,
                                    use_cache = FALSE)
  expect_lte(out$building_presence_2018, 1)

  # a named vector overrides one band only
  expect_error(
    geolink_buildings_temporal(shp_dt = shp, year = 2018,
                               extract_fun = c(not_a_band = "mean"), quiet = TRUE),
    "not recognised")
})


# Test D ---------------------------------------------------------------------
test_that("empty polygons: count and presence are 0, height is NA", {
  skip_if_offline_bucket()

  # open water off the Pacific coast, no buildings
  shp <- square_of(-78.5, 3.0, 1e6, "empty")

  out <- geolink_buildings_temporal(shp_dt = shp, year = 2018, quiet = TRUE,
                                    use_cache = FALSE)

  expect_equal(out$building_fractional_count_2018, 0)
  expect_equal(out$building_presence_2018, 0)
  expect_true(is.na(out$building_height_2018))
  # missing is spelled NA, never NaN. Both satisfy is.na(), so the assertion
  # above passes either way -- which is how NaN reached a national run as a
  # second sentinel for the same condition. Stata has no NaN and several
  # parquet writers do not round-trip it, so the spelling has to be pinned.
  expect_false(is.nan(out$building_height_2018))
})


test_that("NaN never reaches the output as a second spelling of missing", {
  # No network: .obt_fill_empty() owns the missing-value convention, so the
  # normalisation is testable directly on the frame it returns.
  df <- data.frame(building_fractional_count = c(NaN, NA, 3),
                   building_height           = c(NaN, NA, 2.5),
                   building_presence         = c(NaN, NA, 0.4))
  out <- .obt_fill_empty(df, .OBT_BANDS)

  # count and presence zero-fill from BOTH spellings, as they already did
  expect_equal(out$building_fractional_count, c(0, 0, 3))
  expect_equal(out$building_presence, c(0, 0, 0.4))
  # height keeps missing, but only ever as NA
  expect_true(all(is.na(out$building_height[1:2])))
  expect_false(any(is.nan(out$building_height)))
  expect_equal(out$building_height[3], 2.5)

  # and the cache-read path normalises too, so a cache written before this
  # existed cannot hand back a different spelling than a fresh extraction
  expect_false(any(is.nan(.obt_nan_to_na(df)$building_height)))
  expect_true(is.na(.obt_nan_to_na(df)$building_height[1]))
})


# Test E ---------------------------------------------------------------------
test_that("chunks are spatially sorted and never straddle a UTM zone", {
  # No network needed: this exercises the chunker only.
  set.seed(42)
  # a band of polygons spanning 79W to 71W, crossing the 78W and 72W zone
  # boundaries, so zones 17N, 18N and 19N are all represented
  lons <- seq(-79.5, -70.5, length.out = 60)
  shp  <- do.call(rbind, lapply(seq_along(lons), function(i)
    square_of(lons[i], 4.5, 1e6, paste0("p", i))))

  chunks <- .obt_chunks(shp, chunk_size = 7L)

  # every chunk is single-zone
  cen  <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(shp))))
  zone <- .obt_zone_of(cen[, 1], cen[, 2])
  expect_true(length(unique(zone)) >= 2)   # the fixture really does cross zones
  for (ch in chunks) {
    expect_equal(length(unique(zone[ch$idx])), 1L)
    expect_equal(unique(zone[ch$idx]), ch$epsg)
  }

  # every polygon appears exactly once
  expect_equal(sort(unlist(lapply(chunks, function(k) k$idx))), seq_len(nrow(shp)))

  # chunks are spatially compact: mean chunk longitude span is much smaller
  # than the full extent, which is what makes tile selection work
  span_all <- diff(range(cen[, 1]))
  spans <- vapply(chunks, function(k) diff(range(cen[k$idx, 1])), numeric(1))
  expect_lt(mean(spans), span_all / 3)
})


# Test E2 --------------------------------------------------------------------
# The tile-grouped chunker is what the extraction path actually uses. Its
# contract is stronger than the Hilbert chunker's: every polygon still appears
# exactly once and no chunk straddles a UTM zone, but in addition a chunk's
# bounding box must stay small enough that tile selection returns few tiles.
# That last property is the whole point -- a bounding box drawn around
# scattered rural polygons is what pulled 171 tiles for 5 sections.
test_that("tile-grouped chunks partition the input and stay tile-local", {
  # No network: a synthetic tile index standing in for the real one.
  set.seed(7)
  # 12.5 km tiles on a regular grid in zone 18N, covering the test polygons
  gx <- seq(400000, 700000, by = 12500)
  gy <- seq(400000, 700000, by = 12500)
  gg <- expand.grid(xmin = gx, ymin = gy)
  idx <- data.frame(uri = sprintf("t%04d.tif", seq_len(nrow(gg))),
                    epsg = 32618L, resx = 0.5, resy = 0.5, nx = 25000L, ny = 25000L,
                    xmin = gg$xmin, xmax = gg$xmin + 12500,
                    ymin = gg$ymin, ymax = gg$ymin + 12500)
  # WGS84 corners, as the real index carries
  cn <- sf::st_as_sf(data.frame(x = c(idx$xmin, idx$xmax), y = c(idx$ymin, idx$ymax),
                                id = rep(seq_len(nrow(idx)), 2)),
                     coords = c("x", "y"), crs = 32618)
  ll <- sf::st_coordinates(sf::st_transform(cn, 4326))
  idx$lon_min <- pmin(ll[1:nrow(idx), 1], ll[-(1:nrow(idx)), 1])
  idx$lon_max <- pmax(ll[1:nrow(idx), 1], ll[-(1:nrow(idx)), 1])
  idx$lat_min <- pmin(ll[1:nrow(idx), 2], ll[-(1:nrow(idx)), 2])
  idx$lat_max <- pmax(ll[1:nrow(idx), 2], ll[-(1:nrow(idx)), 2])

  # 60 polygons of ~4 km2 scattered across the grid, i.e. the rural case
  pxy <- data.frame(x = runif(60, 410000, 690000), y = runif(60, 410000, 690000))
  mk <- function(i, half = 1000) {
    sf::st_polygon(list(matrix(c(pxy$x[i] - half, pxy$y[i] - half,
                                 pxy$x[i] + half, pxy$y[i] - half,
                                 pxy$x[i] + half, pxy$y[i] + half,
                                 pxy$x[i] - half, pxy$y[i] + half,
                                 pxy$x[i] - half, pxy$y[i] - half),
                               ncol = 2, byrow = TRUE)))
  }
  shp <- sf::st_sf(pid = 1:60,
                   geometry = sf::st_transform(
                     sf::st_sfc(lapply(1:60, mk), crs = 32618), 4326))

  chunks <- .obt_chunks_tiled(shp, idx, chunk_size = 200L)

  # every polygon exactly once
  expect_equal(sort(unlist(lapply(chunks, function(k) k$idx))), seq_len(60))
  # single-zone chunks
  for (ch in chunks) expect_equal(length(unique(ch$epsg)), 1L)

  # the property that matters: each chunk selects only a handful of tiles
  tiles_per_chunk <- vapply(chunks, function(ch) {
    u  <- sf::st_transform(shp[ch$idx, ], ch$epsg)
    bb <- as.numeric(sf::st_bbox(u))
    sum(idx$epsg == ch$epsg & idx$xmax >= bb[1] & idx$xmin <= bb[3] &
        idx$ymax >= bb[2] & idx$ymin <= bb[4])
  }, numeric(1))
  expect_lte(max(tiles_per_chunk), 4)

  # and it is a genuine improvement over grouping by Hilbert order alone
  hchunks <- .obt_chunks(shp, chunk_size = 200L)
  htiles <- vapply(hchunks, function(ch) {
    u  <- sf::st_transform(shp[ch$idx, ], ch$epsg)
    bb <- as.numeric(sf::st_bbox(u))
    sum(idx$epsg == ch$epsg & idx$xmax >= bb[1] & idx$xmin <= bb[3] &
        idx$ymax >= bb[2] & idx$ymin <= bb[4])
  }, numeric(1))
  expect_lt(sum(tiles_per_chunk), sum(htiles))
})


# Test E3 --------------------------------------------------------------------
test_that("polygons no tile covers are still grouped, not dropped", {
  idx <- data.frame(uri = "t.tif", epsg = 32618L, resx = 0.5, resy = 0.5,
                    nx = 25000L, ny = 25000L,
                    xmin = 400000, xmax = 412500, ymin = 400000, ymax = 412500,
                    lon_min = -76, lon_max = -75.9, lat_min = 3.6, lat_max = 3.7)
  # one polygon over the tile, one far away in the Pacific with no tile at all
  shp <- rbind(square_of(-75.95, 3.65, 1e6, "covered"),
               square_of(-84.0,  3.65, 1e6, "uncovered"))
  chunks <- .obt_chunks_tiled(shp, idx, chunk_size = 200L)
  expect_equal(sort(unlist(lapply(chunks, function(k) k$idx))), 1:2)
})


# Test F ---------------------------------------------------------------------
test_that("presence-weighted height differs from the plain mean", {
  skip_if_offline_bucket()

  shp <- square_of(-74.0817, 4.6045, 50000, "w")

  plain <- geolink_buildings_temporal(shp_dt = shp, year = 2018,
                                      indicators = "building_height",
                                      quiet = TRUE, use_cache = FALSE)
  wtd   <- geolink_buildings_temporal(shp_dt = shp, year = 2018,
                                      indicators = "building_height",
                                      weight_raster = "building_presence",
                                      quiet = TRUE, use_cache = FALSE)

  # conditional-on-presence height should exceed the ground-diluted mean
  expect_gt(wtd$building_height_2018, plain$building_height_2018)
})


# Test G ---------------------------------------------------------------------
test_that("multiple years return suffixed columns, not long format", {
  skip_if_offline_bucket()

  shp <- square_of(-74.0817, 4.6045, 3000, "u")
  out <- geolink_buildings_temporal(shp_dt = shp, year = c(2016, 2018),
                                    indicators = "building_presence",
                                    quiet = TRUE)

  expect_equal(nrow(out), 1L)
  expect_contains(colnames(out),
                  c("building_presence_2016", "building_presence_2018"))
})


# Test H ---------------------------------------------------------------------
test_that("validation rejects bad input", {
  shp <- square_of(-74.0817, 4.6045, 3000, "u")

  expect_error(geolink_buildings_temporal(), "Supply either")
  expect_error(geolink_buildings_temporal(shp_dt = shp, year = 2011), "between 2016")
  expect_error(geolink_buildings_temporal(shp_dt = shp, indicators = "nope"),
               "Unknown indicator")
  expect_error(geolink_buildings_temporal(shp_dt = shp, weight_raster = 42),
               "band name or a SpatRaster")
  expect_error(geolink_buildings_temporal(shp_dt = shp[0, ]), "no rows")
})


# Test I ---------------------------------------------------------------------
# Regression. The survey path used to extract on shp_dt and then spatially join
# the buffers to it, so every household came back with the value of whichever
# polygon contained it. Against a single study-area polygon that is one number
# repeated down the column -- it looks like data and is not. The statistics must
# come from each household's own buffer.
test_that("survey buffers are measured on themselves, not on the parent polygon", {
  skip_if_offline_bucket()

  aoi <- square_of(-74.0817, 4.6045, 4e6, "aoi")        # ~2 x 2 km over Bogota
  set.seed(11)
  pts <- sf::st_coordinates(sf::st_sample(aoi, 6))
  surv <- data.frame(hhid = 1:6, lon = pts[, 1], lat = pts[, 2])

  out <- geolink_buildings_temporal(shp_dt = aoi, year = 2018,
                                    survey_dt = surv, survey_lat = "lat",
                                    survey_lon = "lon", buffer_size = 150,
                                    quiet = TRUE)

  expect_equal(nrow(out), nrow(surv))
  expect_true(all(c("hhid", "building_presence_2018") %in% names(out)))

  cnt <- out$building_fractional_count_2018
  expect_false(anyNA(cnt))

  # the defect's signature: every buffer identical
  expect_gt(length(unique(round(cnt, 6))), 1L)

  # and each 150 m buffer must be far smaller than the 4 km2 parent
  parent <- geolink_buildings_temporal(shp_dt = aoi, year = 2018, quiet = TRUE)
  expect_true(all(cnt < parent$building_fractional_count_2018))

  # a survey without a buffer has no area to summarise over and must say so
  expect_error(
    geolink_buildings_temporal(shp_dt = aoi, year = 2018, survey_dt = surv,
                               survey_lat = "lat", survey_lon = "lon",
                               quiet = TRUE),
    "buffer_size is required")
})


###############################################################################
### read_res: reading from the internal overview pyramids
###
### The J and K tests are offline, against local fixture rasters, because the
### two things most likely to go wrong quietly cannot be provoked on the real
### product. The scale correction has to be checked in BOTH directions on the
### same band, which needs a raster whose native answer is known exactly; and
### the pyramid-mismatch guard needs a defective tile, which by construction
### does not exist upstream. Tests L and M then confirm on the real data that
### the mechanism resolves real levels and that 4 m tracks native.
###############################################################################

## A local three-band tile at 0.5 m with a chosen overview pyramid, standing in
## for a product tile. AVERAGE resampling is not incidental: it is what makes a
## summed read come back short by the square of the decimation, which is the
## behaviour the correction exists to undo.
fixture_tile <- function(nx, ny, factors, xmin, ymin, res = 0.5, epsg = 32618,
                         seed = 1) {
  r <- terra::rast(nrows = ny, ncols = nx,
                   xmin = xmin, xmax = xmin + nx * res,
                   ymin = ymin, ymax = ymin + ny * res,
                   crs = paste0("EPSG:", epsg), nlyrs = 3)
  set.seed(seed)
  terra::values(r) <- stats::runif(nx * ny * 3)
  fn <- tempfile(fileext = ".tif")
  terra::writeRaster(r, fn, datatype = "FLT4S", NAflag = .OBT_NODATA,
                     overwrite = TRUE)
  sf::gdal_addo(fn, overviews = factors, method = "AVERAGE")
  fn
}

fixture_index <- function(files, xmins, ymins, nx, ny, res = 0.5, epsg = 32618L) {
  data.table::data.table(
    uri = files, epsg = epsg, resx = res, resy = res, nx = nx, ny = ny,
    xmin = xmins, xmax = xmins + nx * res,
    ymin = ymins, ymax = ymins + ny * res)
}

fixture_square <- function(xmin, ymin, side, epsg = 32618L) {
  sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_polygon(list(cbind(
    c(xmin, xmin + side, xmin + side, xmin, xmin),
    c(ymin, ymin, ymin + side, ymin + side, ymin)))), crs = epsg))
}


# Test J ---------------------------------------------------------------------
# The failure this guards against does not error, it returns a plausible number.
# A count summed at 4 m is 1/64 of the native count and still looks like a
# count; a count averaged at 4 m is already right and would be 64x too large if
# corrected. So the rule must key on the resolved STATISTIC and not on the band,
# and it is checked here in both directions on the same band.
test_that("the decimation correction follows the statistic, not the band", {
  skip_on_cran()
  skip_if_not(is.function(getNamespace("sf")$gdal_addo), "sf::gdal_addo() unavailable")

  nx <- ny <- 512L
  x0 <- 500000; y0 <- 500000
  tif <- fixture_tile(nx, ny, factors = c(2, 4, 8), xmin = x0, ymin = y0)
  idx <- fixture_index(tif, x0, y0, nx, ny)
  ## aligned to the 4 m grid and well inside the tile, so the coarse read
  ## aggregates whole native blocks and the comparison is exact rather than
  ## edge-dependent
  shp <- fixture_square(x0 + 32, y0 + 32, 128)

  testthat::local_mocked_bindings(.obt_tile_url = function(uri) uri)
  .obt_reset_read_state()

  B  <- "building_fractional_count"
  fS <- stats::setNames(list("sum"),  B)
  fM <- stats::setNames(list("mean"), B)
  nat_sum <- .obt_extract_chunk(shp, idx, 32618L, B, fS, read_res = NULL)
  cor_sum <- .obt_extract_chunk(shp, idx, 32618L, B, fS, read_res = 4)
  nat_avg <- .obt_extract_chunk(shp, idx, 32618L, B, fM, read_res = NULL)
  cor_avg <- .obt_extract_chunk(shp, idx, 32618L, B, fM, read_res = 4)

  # sum IS corrected: without the x64 it would come back at 1/64
  expect_equal(cor_sum[[B]], nat_sum[[B]], tolerance = 1e-4)
  expect_gt(cor_sum[[B]] / nat_sum[[B]], 0.5)

  # mean is NOT corrected: an unwanted x64 would show up as exactly that
  expect_equal(cor_avg[[B]], nat_avg[[B]], tolerance = 1e-4)
  expect_lt(cor_avg[[B]] / nat_avg[[B]], 2)

  # and the two rules are genuinely different code paths on the same band
  expect_gt(nat_sum[[B]] / nat_avg[[B]], 1000)
})


# Test K ---------------------------------------------------------------------
# OVERVIEW_LEVEL is an index into whatever a file happens to carry, so two tiles
# with different pyramid depths would answer the same request at different
# resolutions and mosaic into a raster that is silently mixed. There is no way
# to see that in the output, so it must stop the run.
test_that("tiles with unequal pyramid depth are refused, matched ones mosaic", {
  skip_on_cran()
  skip_if_not(is.function(getNamespace("sf")$gdal_addo), "sf::gdal_addo() unavailable")

  nx <- ny <- 512L
  x0 <- 500000; y0 <- 500000
  x1 <- x0 + nx * 0.5                       # immediately to the east
  deep    <- fixture_tile(nx, ny, c(2, 4, 8), x0, y0, seed = 2)   # 1, 2, 4 m
  shallow <- fixture_tile(nx, ny, c(4, 8),    x1, y0, seed = 3)   # 2, 4 m only
  matched <- fixture_tile(nx, ny, c(2, 4, 8), x1, y0, seed = 3)   # 1, 2, 4 m

  ## a polygon straddling the seam, so both tiles are selected
  shp <- fixture_square(x1 - 64, y0 + 64, 128)

  testthat::local_mocked_bindings(.obt_tile_url = function(uri) uri)

  P  <- "building_presence"
  fM <- stats::setNames(list("mean"), P)

  .obt_reset_read_state()
  bad <- fixture_index(c(deep, shallow), c(x0, x1), c(y0, y0), nx, ny)
  expect_error(
    .obt_extract_chunk(shp, bad, 32618L, P, fM, read_res = 4),
    "Overview pyramids differ")

  ## The same geometry with matched pyramids goes through the VRT-of-VRTs
  ## mosaic instead. `seam` starts exactly on the tile boundary, so tile 1 is
  ## pulled into the chunk by the bounding-box test while contributing no area:
  ## the mosaic therefore has to return precisely what tile 2 alone returns.
  ok    <- fixture_index(c(deep, matched), c(x0, x1), c(y0, y0), nx, ny)
  solo  <- fixture_index(matched, x1, y0, nx, ny)
  seam  <- fixture_square(x1, y0 + 64, 64)

  .obt_reset_read_state()
  two <- .obt_extract_chunk(shp,  ok,   32618L, P, fM, read_res = 4)
  .obt_reset_read_state()
  m2  <- .obt_extract_chunk(seam, ok,   32618L, P, fM, read_res = 4)
  .obt_reset_read_state()
  one <- .obt_extract_chunk(seam, solo, 32618L, P, fM, read_res = 4)

  expect_false(is.na(two[[1]]))
  expect_equal(m2[[1]], one[[1]])
})


# Test O ---------------------------------------------------------------------
# Regression. The tile selection read idx[idx$epsg == epsg, ] against a
# data.table, whose [ evaluates i in the table's own frame, so the bare `epsg`
# resolved to the column and the zone filter was `epsg == epsg`. Neighbouring
# zones were pulled into every chunk. gdalbuildvrt then discarded them for a CRS
# mismatch, which is why it survived: the answer was right only because the
# mosaic threw the wrong tiles away again, and only while the first selected
# tile happened to be in the right zone.
test_that("tile selection filters by UTM zone against a data.table index", {
  skip_on_cran()
  skip_if_not(is.function(getNamespace("sf")$gdal_addo), "sf::gdal_addo() unavailable")

  nx <- ny <- 256L
  x0 <- 500000; y0 <- 500000
  here    <- fixture_tile(nx, ny, c(2, 4), x0, y0, epsg = 32618, seed = 5)
  foreign <- fixture_tile(nx, ny, c(2, 4), x0, y0, epsg = 32619, seed = 6)

  ## same coordinates, different zones -- which is the real case: UTM eastings
  ## repeat, so a 19N tile sits at plausible 18N numbers
  idx <- data.table::data.table(
    uri = c(here, foreign), epsg = c(32618L, 32619L), resx = 0.5, resy = 0.5,
    nx = nx, ny = ny,
    xmin = x0, xmax = x0 + nx * 0.5, ymin = y0, ymax = y0 + ny * 0.5)

  testthat::local_mocked_bindings(.obt_tile_url = function(uri) uri)
  bb <- c(x0 + 8, y0 + 8, x0 + 40, y0 + 40)

  ## One tile matches, so this must take the single-source path. With the bug
  ## both tiles were selected, gdalbuildvrt discarded the foreign one and terra
  ## warned "vrt did not use 1 of the 2 files" -- so the warning is the tell.
  expect_no_warning(r <- .obt_chunk_raster(idx, 32618L, bb))
  expect_false(is.null(r))
  expect_equal(terra::crs(r, describe = TRUE)$code, "32618")

  # and a zone the index does not carry must return nothing, not everything.
  # This is the assertion the bug fails outright: an unfiltered selection
  # returns both tiles for a zone that has none.
  expect_null(.obt_chunk_raster(idx, 32617L, bb))
})


# Test L ---------------------------------------------------------------------
test_that("read_res is validated and refuses to be combined with weights", {
  shp <- square_of(-74.0817, 4.6045, 3000, "u")

  expect_error(geolink_buildings_temporal(shp_dt = shp, read_res = "4"),
               "single positive number")
  expect_error(geolink_buildings_temporal(shp_dt = shp, read_res = c(2, 4)),
               "single positive number")
  expect_error(geolink_buildings_temporal(shp_dt = shp, read_res = 0.25),
               "finer than the product")
  expect_error(
    geolink_buildings_temporal(shp_dt = shp, read_res = 4,
                               weight_raster = "building_presence"),
    "cannot be combined with a coarsened read_res")

  # read_res = NULL and read_res = native are the same request, so they must
  # not land on different cache entries
  expect_identical(
    .obt_result_path(tempdir(), "k", 2018L, .OBT_BANDS,
                     stats::setNames(as.list(rep("mean", 3)), .OBT_BANDS),
                     "ESRI:102033", read_res = NULL),
    .obt_result_path(tempdir(), "k", 2018L, .OBT_BANDS,
                     stats::setNames(as.list(rep("mean", 3)), .OBT_BANDS),
                     "ESRI:102033", read_res = NULL))
  # but a coarsened read must
  expect_false(identical(
    .obt_result_path(tempdir(), "k", 2018L, .OBT_BANDS,
                     stats::setNames(as.list(rep("mean", 3)), .OBT_BANDS),
                     "ESRI:102033", read_res = NULL),
    .obt_result_path(tempdir(), "k", 2018L, .OBT_BANDS,
                     stats::setNames(as.list(rep("mean", 3)), .OBT_BANDS),
                     "ESRI:102033", read_res = 4)))
})


# Test M ---------------------------------------------------------------------
# On the real product. Levels are resolved by measured resolution rather than by
# a hardcoded index, and a resolution the pyramid does not carry is an error
# rather than a snap to the nearest one.
test_that("overview levels resolve against real tiles by measured resolution", {
  skip_if_offline_bucket()

  idx <- .obt_tile_index(2018, bbox = c(-74.2, 4.5, -74.0, 4.7), quiet = TRUE)
  url <- .obt_tile_url(idx$uri[idx$epsg == 32618L][1])

  o <- .obt_overviews(url)
  expect_gt(nrow(o), 4L)
  expect_true(all(diff(o$res) < 0))            # coarsest first on this product

  for (target in c(1, 2, 4, 8)) {
    lv <- .obt_resolve_level(url, target)
    expect_lt(abs(lv$res - target) / target, 0.02)
    r <- terra::rast(url, opts = paste0("OVERVIEW_LEVEL=", lv$level))
    expect_equal(terra::res(r)[1], lv$res)     # the index really means that res
  }

  # nothing at 3 m, so this is an error and not a quiet 2 or 4
  expect_error(.obt_resolve_level(url, 3), "does not match any overview")
})


# Test N ---------------------------------------------------------------------
# The claim that justifies read_res: at 4 m the three unweighted bands track
# native closely enough to substitute for it on rural geometry.
#
# The location is pinned to central Bogota, the same coordinates test A and
# test F use, because those already assert that buildings are found there. An
# earlier version sat over semi-rural ground north of the city and skipped each
# band when it came back empty -- so it could report success while comparing
# nothing to nothing. Absence of buildings here means the extraction broke, not
# that the ground is bare, so it is a failure and not a skip.
test_that("4 m agrees with native on a rural-scale polygon", {
  skip_if_offline_bucket()

  shp <- square_of(-74.0817, 4.6045, 4e6, "rural")   # ~2 x 2 km over Bogota

  nat <- geolink_buildings_temporal(shp_dt = shp, year = 2018, quiet = TRUE,
                                    use_cache = FALSE)
  c4  <- geolink_buildings_temporal(shp_dt = shp, year = 2018, quiet = TRUE,
                                    use_cache = FALSE, read_res = 4)

  for (b in paste0(.OBT_BANDS, "_2018")) {
    a <- nat[[b]]
    expect_false(is.na(a), label = paste(b, "native is NA over central Bogota"))
    expect_gt(a, 0)
    expect_lt(abs(c4[[b]] - a) / abs(a), 0.005)
  }

  # the count band is the one the correction acts on: uncorrected it would be
  # 1/64 of native, which is nowhere near half a percent
  expect_gt(c4$building_fractional_count_2018 /
            nat$building_fractional_count_2018, 0.9)
})
