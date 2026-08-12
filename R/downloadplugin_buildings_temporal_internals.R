## Internals for geolink_buildings_temporal()
##
## Open Buildings 2.5D Temporal is distributed as ~12.5 km GeoTIFF tiles on a
## public GCS bucket, one file per (s2 cell x UTM zone x year), each carrying
## three bands. There is no served spatial index, but the Earth Engine ingestion
## manifests record `affineTransform` and `dimensions` per tile, so a tile index
## can be derived without reading any imagery. That is what .obt_tile_index()
## does.
##
## Nothing here downloads a tile. Extraction reads windows through /vsicurl/.

## ---------------------------------------------------------------------------
## Versioning of cached artefacts
##
## IMPORTANT: bump .OBT_EXTRACTION_VERSION whenever anything that can change an
## extracted VALUE changes. That includes the per-band default statistics, the
## nodata handling, the equal-area CRS used for density terms, the coverage
## weighting, and any fix to the windowed-read path. The result cache key is
## otherwise blind to these and will happily serve stale numbers. This is
## exactly the kind of constant that gets forgotten, so it is deliberately the
## first thing in the file.
##
## Likewise bump .OBT_INDEX_VERSION when the tile-index builder changes shape or
## semantics.
## ---------------------------------------------------------------------------
## NOT bumped for the tile-grouped chunker (.obt_chunks_tiled). Grouping decides
## which polygons share a windowed read, and the version guard exists for things
## that change a VALUE. This one provably does not, for three reasons that hold
## together and not separately: a polygon's UTM zone is still its own centroid's
## zone, so nothing is reprojected differently; .obt_chunk_raster() still selects
## every tile meeting the chunk's bounding box, so a polygon overhanging its
## assigned tile is still fully covered; and where tiles overlap they are
## complementary rather than contradictory, with the VRT merge measured
## order-independent, so the union of covering tiles yields the same pixels
## whichever tile a polygon is grouped under. Verified empirically as well as
## argued: the 20 Cundinamarca rural sections return bit-identical values under
## both chunkers. Keeping the version lets existing cached results stand.
## 1.1.0 - `read_res` added: extraction can read from a GeoTIFF's internal
##         overview pyramid instead of the native 0.5 m grid. This DOES change
##         values, deliberately, so the bump is real and the resolution is part
##         of the result cache key as well. Measured on Colombian rural sections,
##         4 m agrees with native to within 0.52% on all three bands read
##         unweighted, and to 0.044% on genuinely built-up sections, for roughly
##         a 24-fold reduction in pixels. It is NOT free everywhere: a
##         presence-weighted height drifts 4.6% to 13.9% at 4 m, which is why
##         weighting plus a coarsened read is refused rather than warned about.
##         Also in 1.1.0, and independent of read_res: .obt_extract_chunk() no
##         longer extracts building_presence twice on a request that includes
##         height. That one is value-identical and would not have needed a bump.
##
##         And a real fix that would have needed one on its own. The tile
##         selection in .obt_chunk_raster() was written as idx[idx$epsg == epsg,]
##         against a data.table, where the bare `epsg` resolves to the column
##         instead of the argument, so the UTM zone filter never filtered. Tiles
##         from neighbouring zones were handed to gdalbuildvrt, which discarded
##         them for a CRS mismatch and said so in a warning that looked benign.
##         The numbers usually survived, because the discarded tiles were the
##         wrong ones anyway -- but only while the first selected tile belonged
##         to the right zone, and the selection order is index order, not
##         anything that guarantees that. Every 1.0.x entry is retired.
## 1.0.2 - windowed-read path rebuilt: tiles are combined with a VRT and the
##         nodata sentinel is set as the raster NA flag, instead of being
##         merged, cropped and classified. Measured identical to 1.0.1 to the
##         last representable digit on both single- and multi-tile chunks, so
##         this alone would not need a bump. The bump is for the fallback that
##         went with it: on a failed multi-tile merge the old code silently
##         continued with only the FIRST tile, so any polygon covered by the
##         others was scored against a raster that did not cover it. A cache
##         entry written down that path is wrong and indistinguishable from a
##         good one, so every 1.0.1 entry is retired rather than trusted.
##         Also in 1.0.2: the survey path extracted on shp_dt and spatially
##         joined the buffers to it, so every household inherited the value of
##         its containing polygon instead of its own buffer. With one study-area
##         polygon that is the same number on every row. It now extracts on the
##         buffered survey geometries, which is what postdownload_processor()
##         does for every other layer in the package.
## 1.0.1 - height is NA'd where presence is 0 (the bands store 0, not nodata,
##         over empty ground, so the mean was returning 0); weighted statistics
##         now map to exactextractr's weighted_* operations, which previously
##         ignored the weights silently.
.OBT_EXTRACTION_VERSION <- "1.1.0"
.OBT_INDEX_VERSION      <- "1.0.0"

.OBT_BUCKET_API <- "https://storage.googleapis.com/storage/v1/b/open-buildings-temporal-data/o"
.OBT_BASE       <- "https://storage.googleapis.com/open-buildings-temporal-data/"
.OBT_BANDS      <- c("building_fractional_count", "building_height", "building_presence")
.OBT_YEARS      <- 2016:2023
.OBT_NODATA     <- -99

## Native ground sample distance of the product, in metres. Used ONLY to decide
## whether a requested `read_res` is a coarsening at all, and for messages. The
## decimation factor that rescales summed statistics is never taken from here:
## it is measured, as the resolution GDAL actually reported for the overview
## divided by the resolution the tile index records for that tile. A hardcoded
## denominator would survive a change in the product and be wrong by a square.
.OBT_NATIVE_RES <- 0.5

## Statistics whose value scales with pixel AREA, and therefore need the
## decimation correction when read from an overview. Overviews store the MEAN of
## each parent block, so a sum over an n-times-coarser grid is 1/n^2 of the
## native sum: at 4 m the count band comes back at exactly 63/64 short.
##
## Everything not listed is treated as intensive and left alone. That is right
## for mean, median, min, max, quantile, mode, stdev and variance, whose units
## do not carry an area. It is deliberately keyed on the RESOLVED statistic, not
## on the band: a caller passing extract_fun = "sum" for height gets the
## correction, and one passing "mean" for the count band does not. Keying on the
## band instead would produce a plausible number rather than an error, which is
## the failure this whole mechanism is built to avoid.
.OBT_SUM_OPS <- c("sum", "weighted_sum")

## Statistics that are neither intensive nor a simple sum, so no correction is
## defined. exactextractr's "count" and "frac" family return pixel-coverage
## tallies, which change with resolution in a way that is not a rescaling of the
## same quantity. Rather than guess, a coarsened read refuses them.
.OBT_UNSCALABLE_OPS <- c("count", "weighted_count", "frac", "weighted_frac")

## exactextractr operations that actually consume `weights`. Anything else
## silently ignores them (it only warns), which is how a "weighted" result can
## come back identical to the unweighted one.
.OBT_WEIGHTED_OPS <- c("weighted_mean", "weighted_sum", "weighted_stdev",
                       "weighted_variance", "weighted_quantile", "weighted_frac")

## Per-band default statistic. A mean of fractional counts is buildings per
## pixel, which is not an interpretable quantity, so the count band sums.
.obt_default_fun <- function(band) {
  switch(band,
         building_fractional_count = "sum",
         building_height           = "mean",
         building_presence         = "mean",
         "mean")
}

## ---------------------------------------------------------------------------
## Cache
## ---------------------------------------------------------------------------

#' Resolve the GeoLink cache directory for this source
#'
#' @param cache_dir character or NULL; NULL uses rappdirs::user_cache_dir("GeoLink")
#' @return the cache path, created if missing
#' @keywords internal
.obt_cache_dir <- function(cache_dir = NULL) {
  root <- if (is.null(cache_dir)) rappdirs::user_cache_dir("GeoLink") else cache_dir
  p <- file.path(root, "buildings_temporal")
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

#' A stable, cheap key for a polygon set
#'
#' Uses the feature count, the bounding box and summary statistics of the
#' centroid coordinates. Specific enough to distinguish polygon sets in
#' practice without hashing every vertex.
#' @keywords internal
.obt_shp_key <- function(shp_dt) {
  g   <- sf::st_transform(sf::st_geometry(shp_dt), 4326)
  cen <- suppressWarnings(sf::st_coordinates(sf::st_centroid(g)))
  bb  <- as.numeric(sf::st_bbox(g))
  v   <- c(nrow(shp_dt), round(bb, 6),
           round(sum(cen[, 1]), 4), round(sum(cen[, 2]), 4),
           round(stats::sd(cen[, 1]), 6), round(stats::sd(cen[, 2]), 6))
  v <- v[is.finite(v)]
  h <- abs(sum(as.integer(round(v * 1000)) * seq_along(v))) %% .Machine$integer.max
  paste0("n", nrow(shp_dt), "_", format(as.hexmode(h), width = 8))
}

#' Path of the cached result for one request
#'
#' `read_res` is part of the key. It has to be: the same polygons, year, bands
#' and statistics read at 4 m are a different set of numbers from the same
#' request read natively, and without it the second call would be served the
#' first call's answer at whichever resolution happened to run first.
#' @keywords internal
.obt_result_path <- function(cache_dir, shp_key, year, bands, funs, area_crs,
                             read_res = NULL) {
  rr <- if (is.null(read_res)) "native" else
    paste0("r", gsub("[^0-9]", "", format(round(read_res, 3), nsmall = 3)))
  fn <- sprintf("res_v%s_%s_%d_%s_%s_%s_%s.rds",
                .OBT_EXTRACTION_VERSION, shp_key, year,
                paste(substr(bands, 10, 13), collapse = ""),
                paste(unlist(funs[bands]), collapse = "-"),
                gsub("[^A-Za-z0-9]", "", area_crs), rr)
  file.path(.obt_cache_dir(cache_dir), fn)
}

## ---------------------------------------------------------------------------
## Tile index
## ---------------------------------------------------------------------------

#' List manifest object names for a year
#' @keywords internal
.obt_manifest_names <- function(year) {
  out <- character(0); tok <- NULL
  repeat {
    u <- paste0(.OBT_BUCKET_API, "?prefix=v1/manifests/&maxResults=1000",
                if (!is.null(tok)) paste0("&pageToken=", tok) else "")
    j <- jsonlite::fromJSON(u, simplifyVector = FALSE)
    out <- c(out, vapply(j$items, function(x) basename(x$name), character(1)))
    tok <- j$nextPageToken
    if (is.null(tok)) break
  }
  out <- out[grepl("\\.json$", out)]
  out[grepl(paste0("_", year, "_06_30\\.json$"), out)]
}

#' Parse one manifest into a table of tile footprints
#' @keywords internal
.obt_parse_manifest <- function(nm) {
  m    <- jsonlite::fromJSON(paste0(.OBT_BASE, "v1/manifests/", nm), simplifyVector = FALSE)
  epsg <- as.integer(sub(".*EPSG_(\\d+)_.*", "\\1", nm))
  ## uriPrefix and the per-source uri concatenate with NO separator. A normal
  ## file.path() join produces a 404.
  pref <- sub("^gs://open-buildings-temporal-data/", "", m$uriPrefix)
  srcs <- m$tilesets[[1]]$sources
  data.table::rbindlist(lapply(srcs, function(s) {
    a <- s$affineTransform; d <- s$dimensions
    data.table::data.table(
      uri  = paste0(pref, s$uris[[1]]),
      epsg = epsg,
      resx = a$scaleX, resy = abs(a$scaleY),
      nx   = d$width,  ny   = d$height,
      xmin = a$translateX,
      xmax = a$translateX + d$width * a$scaleX,
      ymax = a$translateY,
      ymin = a$translateY - d$height * abs(a$scaleY))
  }))
}

#' Add WGS84 bounding boxes to a tile table
#' @keywords internal
.obt_to_wgs84 <- function(dt) {
  data.table::rbindlist(lapply(split(dt, dt$epsg), function(g) {
    pts <- sf::st_as_sf(
      data.frame(x  = c(g$xmin, g$xmax, g$xmin, g$xmax),
                 y  = c(g$ymin, g$ymin, g$ymax, g$ymax),
                 id = rep(seq_len(nrow(g)), 4)),
      coords = c("x", "y"), crs = g$epsg[1])
    ll <- sf::st_coordinates(sf::st_transform(pts, 4326))
    d  <- data.table::data.table(id = pts$id, X = ll[, 1], Y = ll[, 2])
    b  <- d[, list(lon_min = min(X), lon_max = max(X),
                   lat_min = min(Y), lat_max = max(Y)), by = "id"]
    data.table::setorderv(b, "id")
    cbind(g, b[, !"id", with = FALSE])
  }))
}

#' UTM zone EPSG codes that a WGS84 bounding box can fall in
#' @keywords internal
.obt_zones_for_bbox <- function(bbox) {
  z <- floor((c(bbox[1], bbox[3]) + 180) / 6) + 1
  z <- seq(min(z), max(z))
  out <- integer(0)
  if (bbox[4] >= 0) out <- c(out, 32600L + z)
  if (bbox[2] <  0) out <- c(out, 32700L + z)
  sort(unique(as.integer(out)))
}

#' EPSG of the UTM zone containing a lon/lat
#' @keywords internal
.obt_zone_of <- function(lon, lat) {
  z <- floor((lon + 180) / 6) + 1
  as.integer(ifelse(lat >= 0, 32600L + z, 32700L + z))
}

#' Build, or load from cache, the tile index for one year
#'
#' Keyed by year, by the UTM zones required, and by .OBT_INDEX_VERSION so a
#' change to the builder invalidates cached indices rather than silently
#' reusing them.
#'
#' @param year integer
#' @param cache_dir character or NULL
#' @param bbox numeric c(xmin, ymin, xmax, ymax) in WGS84, or NULL for a global build
#' @param quiet logical
#' @return a data.table of tile footprints
#' @keywords internal
.obt_tile_index <- function(year, cache_dir = NULL, bbox = NULL, quiet = FALSE) {
  cd  <- .obt_cache_dir(cache_dir)
  zon <- if (is.null(bbox)) "all" else paste(.obt_zones_for_bbox(bbox), collapse = "-")
  fn  <- file.path(cd, sprintf("tile_index_v%s_%d_%s.rds", .OBT_INDEX_VERSION, year, zon))
  if (file.exists(fn)) {
    if (!quiet) message("Using cached tile index: ", basename(fn))
    return(readRDS(fn))
  }
  if (!quiet) message("Building tile index for ", year,
                      " (cached after the first call) ...")
  nms <- .obt_manifest_names(year)
  if (!is.null(bbox)) {
    z   <- .obt_zones_for_bbox(bbox)
    nms <- nms[grepl(paste0("EPSG_(", paste(z, collapse = "|"), ")_"), nms)]
  }
  if (!length(nms)) stop("No Open Buildings Temporal manifests found for year ", year)
  idx <- data.table::rbindlist(lapply(nms, .obt_parse_manifest))
  idx <- .obt_to_wgs84(idx)
  saveRDS(idx, fn)
  if (!quiet) message("  indexed ", format(nrow(idx), big.mark = ","), " tiles from ",
                      length(nms), " manifests")
  idx
}

## ---------------------------------------------------------------------------
## Spatial ordering and chunking
## ---------------------------------------------------------------------------

#' Hilbert curve index for points on a 2^bits grid
#'
#' Standard xy2d. Returns the curve distance, so order() on the result gives a
#' spatially coherent ordering.
#' @keywords internal
.obt_hilbert_d <- function(xy, bits = 16L) {
  n  <- bitwShiftL(1L, bits)
  rx0 <- range(xy[, 1]); ry0 <- range(xy[, 2])
  sc <- function(v, r) {
    if (!is.finite(diff(r)) || diff(r) == 0) return(rep(0L, length(v)))
    as.integer(pmin(n - 1L, pmax(0L, floor((v - r[1]) / diff(r) * (n - 1L)))))
  }
  x <- sc(xy[, 1], rx0); y <- sc(xy[, 2], ry0)
  d <- numeric(length(x))
  s <- bitwShiftR(n, 1L)
  while (s > 0L) {
    rx <- as.integer(bitwAnd(x, s) > 0L)
    ry <- as.integer(bitwAnd(y, s) > 0L)
    d  <- d + as.numeric(s) * as.numeric(s) * bitwXor(3L * rx, ry)
    ## rotate quadrant (note: reflection uses n - 1, not s - 1)
    sw <- ry == 0L
    if (any(sw)) {
      fl <- sw & rx == 1L
      if (any(fl)) {
        x[fl] <- n - 1L - x[fl]
        y[fl] <- n - 1L - y[fl]
      }
      tmp <- x[sw]; x[sw] <- y[sw]; y[sw] <- tmp
    }
    s <- bitwShiftR(s, 1L)
  }
  d
}

#' Order polygons spatially, then split into single-zone chunks
#'
#' Sorting is not optional. An unsorted chunk drawn from a national polygon set
#' has a near-national bounding box and would select almost every tile,
#' defeating tile selection entirely.
#'
#' The split by UTM zone happens AFTER the sort. Hilbert ordering on WGS84
#' centroids does not respect zone boundaries, so a spatially compact chunk near
#' 78 degrees west can contain polygons in both zone 17N and 18N. Colombia spans
#' 17N to 19N, so this is a real case rather than a theoretical one. Splitting
#' after the sort keeps chunks both compact and single-CRS.
#'
#' This is the fallback grouping, used by .obt_chunks_tiled() for polygons that
#' no tile covers. Hilbert order alone is not the primary strategy: see
#' .obt_chunks_tiled() for why.
#'
#' @param shp_dt an sf object in any CRS
#' @param chunk_size integer, maximum polygons per chunk
#' @param rows integer row indices of shp_dt to group; NULL for all
#' @return list of lists with elements `idx` (row indices into shp_dt) and `epsg`
#' @keywords internal
.obt_chunks <- function(shp_dt, chunk_size = 200L, rows = NULL) {
  g    <- sf::st_transform(sf::st_geometry(shp_dt), 4326)
  cen  <- suppressWarnings(sf::st_coordinates(sf::st_centroid(g)))
  epsg <- .obt_zone_of(cen[, 1], cen[, 2])
  keep <- if (is.null(rows)) seq_len(nrow(cen)) else rows
  if (!length(keep)) return(list())
  ## Hilbert distance is computed on the retained subset only, so the fallback
  ## groups compactly among themselves rather than inheriting a global ordering.
  ord  <- keep[order(.obt_hilbert_d(cen[keep, , drop = FALSE]))]

  out <- list()
  for (z in unique(epsg[ord])) {
    ids <- ord[epsg[ord] == z]
    sp  <- split(ids, ceiling(seq_along(ids) / chunk_size))
    for (s in sp) out[[length(out) + 1L]] <- list(idx = s, epsg = z)
  }
  out
}

#' Assign each polygon to a primary tile
#'
#' Returns a row index into `idx` for every polygon, or NA where no tile in the
#' polygon's own UTM zone contains its centroid.
#'
#' Tiles are 12.5 km squares, axis-aligned in native UTM, but they are NOT one
#' regular grid per zone: the product is tiled per (S2 cell x UTM zone), so
#' several origin grids coexist within a zone and tiles from adjacent S2 cells
#' overlap, by as much as 143 of a tile's 156.25 km2. Overlapping tiles are
#' complementary rather than contradictory -- where two cover the same ground
#' at most one carries data and the other is empty -- which is why the union of
#' covering tiles gives the same pixels regardless of which tile a polygon is
#' nominally assigned to.
#'
#' Lookup is by a coarse 12.5 km hash rather than a scan. A tile is registered
#' under every coarse cell it touches (at most four, since it is exactly one
#' cell wide and its origin need not be aligned), so any tile containing a point
#' is registered under that point's cell. The hash is therefore exact, not a
#' heuristic prefilter, and the whole assignment is O(polygons + tiles).
#'
#' @param cen two-column matrix of WGS84 centroid coordinates
#' @param epsg integer vector, the UTM zone EPSG of each polygon
#' @param idx the tile index
#' @return integer vector of row indices into `idx`, NA where uncovered
#' @keywords internal
.obt_primary_tile <- function(cen, epsg, idx) {
  n   <- nrow(cen)
  out <- rep(NA_integer_, n)
  cs  <- 12500                      # tile side in metres

  for (z in unique(epsg)) {
    pr <- which(epsg == z)
    tr <- which(idx$epsg == z)
    if (!length(tr)) next

    ## centroids into the zone's native UTM
    pts <- sf::st_as_sf(data.frame(x = cen[pr, 1], y = cen[pr, 2]),
                        coords = c("x", "y"), crs = 4326)
    pxy <- sf::st_coordinates(sf::st_transform(pts, z))

    txmin <- idx$xmin[tr]; txmax <- idx$xmax[tr]
    tymin <- idx$ymin[tr]; tymax <- idx$ymax[tr]

    ## register each tile under every coarse cell it touches
    cxa <- floor(txmin / cs); cxb <- floor((txmax - 1e-6) / cs)
    cya <- floor(tymin / cs); cyb <- floor((tymax - 1e-6) / cs)
    reg <- data.table::rbindlist(lapply(0:1, function(dx)
      data.table::rbindlist(lapply(0:1, function(dy)
        data.table::data.table(key_ = paste(pmin(cxa + dx, cxb),
                                            pmin(cya + dy, cyb)),
                               tl   = tr)))))
    reg <- unique(reg)

    pt <- data.table::data.table(
      key_ = paste(floor(pxy[, 1] / cs), floor(pxy[, 2] / cs)),
      pid  = pr, px = pxy[, 1], py = pxy[, 2])

    cand <- merge(pt, reg, by = "key_", allow.cartesian = TRUE)
    if (!nrow(cand)) next

    ## exact containment, then the tile whose centre is nearest. Nearest-centre
    ## keeps the polygon as far inside its tile as possible, which minimises the
    ## overhang that pulls neighbouring tiles into the chunk's bounding box.
    keep <- cand$px >= txmin[match(cand$tl, tr)] &
            cand$px <  txmax[match(cand$tl, tr)] &
            cand$py >= tymin[match(cand$tl, tr)] &
            cand$py <  tymax[match(cand$tl, tr)]
    cand <- cand[keep]
    if (!nrow(cand)) next

    m  <- match(cand$tl, tr)
    d2 <- (cand$px - (txmin[m] + txmax[m]) / 2)^2 +
          (cand$py - (tymin[m] + tymax[m]) / 2)^2
    ## uri breaks ties so the choice does not depend on index row order
    o  <- order(cand$pid, d2, idx$uri[cand$tl])
    cand <- cand[o]
    first <- !duplicated(cand$pid)
    out[cand$pid[first]] <- cand$tl[first]
  }
  out
}

#' Group polygons by tile, then split into chunks
#'
#' The primary chunking strategy. Hilbert ordering alone groups by position
#' along a space-filling curve, which is the right thing when polygons are small
#' and dense but not when they are large and scattered. A chunk's tile set is
#' selected from the chunk's BOUNDING BOX, and a bounding box drawn around a few
#' scattered rural sections covers vastly more ground than the sections do: five
#' Colombian rural sections totalling 11.5 km2 of actual polygon pulled 171
#' tiles that way. Grouping by tile makes the bounding box a subset of one tile
#' by construction, so the tile set collapses to that tile plus whatever a
#' polygon overhanging its edge genuinely needs.
#'
#' A polygon spanning several tiles is assigned to ONE primary tile and read
#' whole, rather than being read once per tile and recombined. Recombination
#' would have to know how to merge each statistic across partial reads, which is
#' well defined for "sum" but not for "mean" without carrying coverage areas,
#' and not at all for the quantile and mode operations that `extract_fun`
#' accepts. Primary assignment needs none of that: .obt_chunk_raster() already
#' expands the tile set to cover the chunk's bounding box, so an overhanging
#' polygon still sees every tile it needs and is scored exactly once.
#'
#' Zone assignment is unchanged from the Hilbert path -- it remains the UTM zone
#' of the polygon's own centroid, never the zone of the assigned tile. That
#' matters: tiles from a neighbouring zone can overlap this one, and inheriting
#' a tile's zone would silently reproject some polygons and change their values.
#'
#' @param shp_dt an sf object in any CRS
#' @param idx the tile index
#' @param chunk_size integer, maximum polygons per chunk
#' @return list of lists with elements `idx` (row indices into shp_dt) and `epsg`
#' @keywords internal
.obt_chunks_tiled <- function(shp_dt, idx, chunk_size = 200L) {
  g    <- sf::st_transform(sf::st_geometry(shp_dt), 4326)
  cen  <- suppressWarnings(sf::st_coordinates(sf::st_centroid(g)))
  epsg <- .obt_zone_of(cen[, 1], cen[, 2])
  tile <- .obt_primary_tile(cen, epsg, idx)

  out <- list()

  ## Tile groups, visited in Hilbert order of tile centres so that consecutive
  ## chunks are spatially adjacent and /vsicurl keeps hitting warm ranges.
  have <- which(!is.na(tile))
  if (length(have)) {
    ut  <- sort(unique(tile[have]))
    tc  <- cbind((idx$lon_min[ut] + idx$lon_max[ut]) / 2,
                 (idx$lat_min[ut] + idx$lat_max[ut]) / 2)
    ut  <- ut[order(.obt_hilbert_d(tc))]
    for (t in ut) {
      ids <- have[tile[have] == t]
      ## Within a tile the polygons still get a spatial ordering, which matters
      ## once a dense urban tile holds more polygons than chunk_size.
      ids <- ids[order(.obt_hilbert_d(cen[ids, , drop = FALSE]))]
      for (z in unique(epsg[ids])) {
        zi <- ids[epsg[ids] == z]
        sp <- split(zi, ceiling(seq_along(zi) / chunk_size))
        for (s in sp) out[[length(out) + 1L]] <- list(idx = s, epsg = z)
      }
    }
  }

  ## Polygons no tile covers: outside the product's footprint, or in a zone the
  ## index does not carry. They must still be attempted, and they still return
  ## all-NA from .obt_chunk_raster() if truly uncovered, so they fall back to
  ## the Hilbert grouping rather than being dropped.
  miss <- which(is.na(tile))
  if (length(miss)) {
    out <- c(out, .obt_chunks(shp_dt, chunk_size = chunk_size, rows = miss))
  }
  out
}

## ---------------------------------------------------------------------------
## Reduced-resolution reads
##
## The tiles carry an internal overview pyramid, and GDAL exposes it through the
## OVERVIEW_LEVEL open option. Reading a rural section at 4 m instead of 0.5 m
## touches a sixty-fourth of the pixels, which is the single largest lever on
## extraction cost for large polygons.
##
## Two things make this more delicate than passing an option through.
##
## First, OVERVIEW_LEVEL is an INDEX into whatever overviews a particular file
## happens to carry, not a resolution, and on this product the ordering runs
## coarsest first. The index that means 4 m therefore depends on how deep that
## file's pyramid is, and a file with one fewer level would answer the same
## index with a different resolution. Nothing here hardcodes an index: levels
## are resolved by opening candidates and reading back the resolution GDAL
## reports.
##
## Second, resolving levels is not free. Probing a pyramid costs one open per
## level, each a range request through /vsicurl, and re-probing per call was
## previously measured to dominate the runtime it was meant to save. Results are
## therefore cached per URL for the session.
## ---------------------------------------------------------------------------

#' GDAL path for one tile uri from the index
#'
#' The single place that knows how an index uri becomes something GDAL can
#' open. Kept as its own function so the resolution machinery can be tested
#' against local fixture rasters without a network round trip, which matters
#' most for the pyramid-mismatch guard: that path cannot be provoked on the real
#' product without finding a defective tile.
#' @keywords internal
.obt_tile_url <- function(uri) paste0("/vsicurl/", .OBT_BASE, uri)

## url -> data.frame(level, res); populated once per tile per session
.obt_ovr_cache <- new.env(parent = emptyenv())
## (url, level) -> path of the single-source VRT that wraps it
.obt_vrt_cache <- new.env(parent = emptyenv())
## the (depth, resolution) every tile in this extraction must agree on
.obt_read_state <- new.env(parent = emptyenv())

#' Reset the cross-chunk resolution invariant
#'
#' Called once per extraction. See .obt_chunk_raster() for what the invariant
#' is and why a per-chunk check alone would not catch the failure it guards.
#' @keywords internal
.obt_reset_read_state <- function() {
  rm(list = ls(.obt_read_state), envir = .obt_read_state)
  invisible(NULL)
}

#' Enumerate a tile's overview pyramid
#'
#' Opens OVERVIEW_LEVEL 0, 1, 2 ... until GDAL refuses, and records the
#' resolution each one reports. Cached per URL: this is header traffic, but it
#' is one request per level and it is not worth paying twice.
#'
#' @param url a /vsicurl/ URL
#' @param max_levels integer, a hard stop
#' @return data.frame with columns `level` and `res`, zero rows if the file
#'   carries no overviews
#' @keywords internal
.obt_overviews <- function(url, max_levels = 32L) {
  hit <- .obt_ovr_cache[[url]]
  if (!is.null(hit)) return(hit)
  o <- data.frame(level = integer(0), res = numeric(0))
  for (lv in seq_len(max_levels) - 1L) {
    ## The probe that finds the end of the pyramid always fails, by design, and
    ## terra reports GDAL's "cannot open overview level" as a warning rather
    ## than an error. Left alone it surfaces one spurious warning per tile.
    r <- suppressWarnings(
      tryCatch(terra::rast(url, opts = paste0("OVERVIEW_LEVEL=", lv)),
               error = function(e) NULL))
    if (is.null(r)) break
    rs <- terra::res(r)[1]
    ## A driver that clamps an out-of-range index instead of erroring would
    ## otherwise loop to max_levels and report a pyramid deeper than the file
    ## has. Repeating the previous resolution is the signature of that.
    if (nrow(o) && isTRUE(all.equal(rs, o$res[nrow(o)]))) break
    o <- rbind(o, data.frame(level = lv, res = rs))
  }
  .obt_ovr_cache[[url]] <- o
  o
}

#' Resolve the overview level of one tile that best matches a target resolution
#'
#' @param url a /vsicurl/ URL
#' @param target_res numeric, metres
#' @param tol numeric, the fractional distance from the target that still counts
#'   as a match. Overview dimensions round up, so the level nominally at 8 m is
#'   actually 7.997 m and an exact test would reject it.
#' @return list(level, res, depth)
#' @keywords internal
.obt_resolve_level <- function(url, target_res, tol = 0.02) {
  o <- .obt_overviews(url)
  if (!nrow(o)) {
    stop("read_res was requested but this tile carries no internal overviews, ",
         "so there is nothing to read at a reduced resolution: ", url,
         call. = FALSE)
  }
  ## nearest on a log scale, so 3 m sits between 2 and 4 rather than snapping to
  ## the arithmetically closer 4
  i <- which.min(abs(log(o$res / target_res)))
  if (abs(o$res[i] - target_res) / target_res > tol) {
    stop("read_res = ", target_res, " m does not match any overview of this ",
         "tile. Available resolutions are ",
         paste(sprintf("%.4g", sort(o$res)), collapse = ", "),
         " m (plus native). Pick one of those rather than the nearest, since ",
         "silently snapping would return numbers computed at a resolution you ",
         "did not ask for. Tile: ", url, call. = FALSE)
  }
  list(level = o$level[i], res = o$res[i], depth = nrow(o))
}

#' Wrap one tile, opened at a chosen overview, in a single-source VRT
#'
#' `terra::vrt()` builds a mosaic from filenames and opens each source with
#' default options, so there is no way to hand it an OVERVIEW_LEVEL. The two
#' available routes are to hand-write the mosaic VRT with per-source
#' `<OpenOptions>`, or to wrap each tile in its own tiny VRT that carries the
#' open option and let `terra::vrt()` mosaic those normally.
#'
#' The second is used here. Both produce the same GDAL structure, but the first
#' requires computing the union grid, the per-source destination windows and the
#' alignment between them by hand, which is exactly the arithmetic that
#' `gdalbuildvrt` exists to get right. Wrapping per tile keeps the hand-written
#' XML to a single source at full extent, where every offset is zero and every
#' size is the source's own, and leaves the mosaicking to the tool. The
#' alternative of opening each tile separately and combining with `merge()` or
#' `mosaic()` was rejected outright: both are eager, and the whole windowed-read
#' design exists to avoid pulling a chunk into memory.
#'
#' @param url a /vsicurl/ URL
#' @param level integer, the resolved OVERVIEW_LEVEL
#' @param epsg integer, the tile's UTM zone
#' @return path to a .vrt file
#' @keywords internal
.obt_overview_vrt <- function(url, level, epsg) {
  key <- paste0(url, "|", level)
  hit <- .obt_vrt_cache[[key]]
  if (!is.null(hit) && file.exists(hit)) return(hit)

  r  <- terra::rast(url, opts = paste0("OVERVIEW_LEVEL=", level))
  e  <- as.vector(terra::ext(r))
  rs <- terra::res(r)
  X  <- terra::ncol(r); Y <- terra::nrow(r); nb <- terra::nlyr(r)

  band <- vapply(seq_len(nb), function(b) sprintf(paste0(
    '  <VRTRasterBand dataType="Float32" band="%d">\n',
    '    <NoDataValue>%s</NoDataValue>\n',
    '    <SimpleSource>\n',
    '      <SourceFilename relativeToVRT="0">%s</SourceFilename>\n',
    '      <OpenOptions><OOI key="OVERVIEW_LEVEL">%d</OOI></OpenOptions>\n',
    '      <SourceBand>%d</SourceBand>\n',
    '      <SrcRect xOff="0" yOff="0" xSize="%d" ySize="%d"/>\n',
    '      <DstRect xOff="0" yOff="0" xSize="%d" ySize="%d"/>\n',
    '    </SimpleSource>\n',
    '  </VRTRasterBand>'),
    b, format(.OBT_NODATA), url, level, b, X, Y, X, Y), character(1))

  xml <- sprintf(paste0(
    '<VRTDataset rasterXSize="%d" rasterYSize="%d">\n',
    '  <SRS>EPSG:%d</SRS>\n',
    '  <GeoTransform>%.10f, %.12f, 0, %.10f, 0, %.12f</GeoTransform>\n',
    '%s\n</VRTDataset>\n'),
    X, Y, epsg, e[1], rs[1], e[4], -rs[2], paste(band, collapse = "\n"))

  fn <- tempfile(pattern = "obt_ovr_", fileext = ".vrt")
  writeLines(xml, fn)
  .obt_vrt_cache[[key]] <- fn
  fn
}

## ---------------------------------------------------------------------------
## Windowed extraction
## ---------------------------------------------------------------------------

#' Open the tiles covering a chunk as one SpatRaster
#'
#' Tiles are opened through /vsicurl/ and never downloaded. Where more than one
#' tile is needed they are combined with a VRT, which is a metadata-only
#' wrapper. Nothing is cropped and nothing is read here: the returned raster is
#' lazy, and exactextractr issues its own per-feature windowed reads.
#'
#' The earlier implementation merged the tiles, cropped to the chunk bbox and
#' classified the nodata value. All three force the whole chunk window into
#' memory. A 200-manzana chunk spanning a few kilometres is tens of thousands
#' of pixels square across three bands, which measured at over 10 GB resident
#' and dominated the wall clock. The lazy form returns identical values.
#'
#' With `read_res` set the tiles are opened from their overview pyramids
#' instead. Each tile's level is resolved from its own pyramid, and every tile
#' read anywhere in the extraction is checked against the first one: same
#' pyramid depth, same resolved resolution. That invariant is deliberately
#' sticky across chunks rather than checked within one. A per-chunk check would
#' pass happily on a product where one region's tiles carry fewer overviews than
#' another's, and the result would be a national extraction silently computed at
#' two resolutions, with no error and nothing in the output to say which rows
#' came from which. A mismatch stops the run.
#'
#' @param read_res numeric or NULL; target read resolution in metres
#' @keywords internal
.obt_chunk_raster <- function(idx, epsg, bbox_utm, read_res = NULL) {
  ## The row selection is built OUTSIDE the subscript, deliberately.
  ##
  ## `idx` is a data.table, and [.data.table evaluates its `i` expression inside
  ## the table's own frame. Written as idx[idx$epsg == epsg & ...], the bare
  ## `epsg` on the right of that comparison resolves to the COLUMN rather than
  ## to this function's argument, so the zone filter silently became
  ## `epsg == epsg` and admitted every tile whose bounding box overlapped,
  ## whatever zone it was in. UTM eastings repeat across zones, so a chunk in
  ## 18N routinely picked up 19N tiles sitting at similar coordinates.
  ##
  ## It went unnoticed because gdalbuildvrt refuses sources whose CRS differs
  ## from the first one and drops them, warning "vrt did not use N of the M
  ## files" -- a message that reads like a tiling quirk. The result was usually
  ## right, but only because the mosaic threw the foreign tiles away again, and
  ## only as long as the FIRST selected tile is one of the correct ones.
  ## Measured on one Cundinamarca chunk: 4 tiles selected, 3 of them 32619.
  rows <- which(idx$epsg == epsg &
                idx$xmax >= bbox_utm[1] & idx$xmin <= bbox_utm[3] &
                idx$ymax >= bbox_utm[2] & idx$ymin <= bbox_utm[4])
  if (!length(rows)) return(NULL)
  tl <- idx[rows, ]
  urls <- vapply(tl$uri, .obt_tile_url, character(1), USE.NAMES = FALSE)

  if (!is.null(read_res)) {
    lv  <- lapply(urls, .obt_resolve_level, target_res = read_res)
    lvl <- vapply(lv, function(x) x$level, integer(1))
    dep <- vapply(lv, function(x) x$depth, integer(1))
    got <- vapply(lv, function(x) x$res,   numeric(1))

    if (is.null(.obt_read_state$depth)) {
      .obt_read_state$depth <- dep[1]
      .obt_read_state$res   <- got[1]
    }
    exp_dep <- .obt_read_state$depth
    exp_res <- .obt_read_state$res
    bad <- dep != exp_dep | abs(got - exp_res) > 1e-6 * exp_res
    if (any(bad)) {
      stop("Overview pyramids differ between tiles, so a reduced-resolution ",
           "read would mix resolutions. Expected depth ", exp_dep, " at ",
           sprintf("%.4g", exp_res), " m; got depth ", dep[which(bad)[1]],
           " at ", sprintf("%.4g", got[which(bad)[1]]), " m for ", sum(bad),
           " of ", length(urls), " tile(s), first: ", urls[which(bad)[1]],
           ". Re-run without read_res rather than accepting a ",
           "mixed-resolution mosaic.", call. = FALSE)
    }
    ## Each tile becomes a single-source VRT carrying its own OVERVIEW_LEVEL, so
    ## the mosaic below is an ordinary VRT-of-VRTs. See .obt_overview_vrt().
    urls <- mapply(.obt_overview_vrt, urls, lvl,
                   MoreArgs = list(epsg = epsg), USE.NAMES = FALSE)
  }

  r <- if (length(urls) == 1L) {
    tryCatch(terra::rast(urls), error = function(e) NULL)
  } else {
    ## Do NOT fall back to a subset of the tiles on failure. A raster covering
    ## only some of the chunk returns plausible numbers for the polygons it
    ## happens to cover and silently wrong ones for the rest. All-NA for the
    ## chunk is the honest outcome, and the caller already treats NULL that way.
    vrt_fn <- tempfile(fileext = ".vrt")
    tryCatch(terra::vrt(urls, filename = vrt_fn, overwrite = TRUE),
             error = function(e) {
               warning("Could not build a virtual mosaic of the ", length(urls),
                       " tiles covering this chunk: ", conditionMessage(e),
                       ". The chunk is returned as NA rather than from a subset ",
                       "of its tiles.", call. = FALSE)
               NULL
             })
  }
  if (is.null(r)) return(NULL)

  names(r) <- .OBT_BANDS[seq_len(terra::nlyr(r))]
  ## The GeoTIFFs DO declare nodata: gdalinfo reports "NoData Value=-99" on all
  ## three bands. An earlier comment here claimed otherwise on the strength of
  ## terra::NAflag() returning NaN, which was a mismeasurement -- NAflag() does
  ## not surface the per-band GDAL nodata for these files, so it says nothing
  ## about what the header holds.
  ##
  ## The assignment therefore stays, but as belt and braces rather than as the
  ## only thing masking the sentinel: since terra does not report the declared
  ## value, it cannot be assumed to be applying it either, and a leaked -99
  ## would pull a mean far negative without erroring. Setting it is metadata
  ## only, so the insurance is free.
  tryCatch(terra::NAflag(r) <- .OBT_NODATA, error = function(e) NULL)
  r
}

#' Extract one chunk of polygons
#'
#' @param shp_chunk sf, polygons for this chunk, any CRS
#' @param idx tile index
#' @param epsg integer, the chunk's UTM zone
#' @param bands character vector of band names
#' @param funs named list, statistic per band
#' @param weight_band character or NULL; a band name used as extraction weights
#' @param read_res numeric or NULL; target read resolution in metres
#' @return data.frame with one column per band
#' @keywords internal
.obt_extract_chunk <- function(shp_chunk, idx, epsg, bands, funs,
                               weight_band = NULL, read_res = NULL) {
  n   <- nrow(shp_chunk)
  res <- as.data.frame(matrix(NA_real_, nrow = n, ncol = length(bands),
                              dimnames = list(NULL, bands)))
  shp_u <- sf::st_transform(shp_chunk, epsg)
  bb    <- as.numeric(sf::st_bbox(shp_u))
  r     <- .obt_chunk_raster(idx, epsg, bb, read_res = read_res)
  if (is.null(r)) return(res)

  ## The decimation factor is MEASURED, never assumed: what GDAL reported for
  ## the raster actually opened, over what the tile index records as this zone's
  ## native pixel. A request for 4 m that quietly resolved to 8 m would then
  ## still be corrected by the right square rather than by the requested one.
  nat   <- min(idx$resx[idx$epsg == epsg], na.rm = TRUE)
  decim <- terra::res(r)[1] / nat
  if (!is.finite(decim) || decim <= 0) decim <- 1
  coarse <- !isTRUE(all.equal(decim, 1, tolerance = 1e-6))

  ## Belt and braces for the guard in geolink_buildings_temporal(). The public
  ## function is the place that explains this to a user, but .obt_extract_years()
  ## is the shared path and a future caller reaching it directly must not be able
  ## to combine weights with a coarsened read by omission.
  if (coarse && !is.null(weight_band)) {
    stop("Weighted extraction from a reduced-resolution read is refused. ",
         "Coarsening destroys the within-polygon variation the weights exist ",
         "to exploit: presence-weighted height measured 4.6% to 13.9% away ",
         "from native at 4 m, and that persists on built-up polygons rather ",
         "than being an artefact of near-empty ones. Read weighted statistics ",
         "at native resolution.", call. = FALSE)
  }

  ## Nodata is handled by .obt_chunk_raster() via the raster's NA flag, which is
  ## a metadata operation. It used to be done here with classify(), which reads
  ## every cell of the chunk window.

  wt <- NULL
  if (!is.null(weight_band) && weight_band %in% names(r)) {
    ## The presence band sits in the SAME file as the height band, so using it
    ## as weights costs no additional read.
    wt <- r[[weight_band]]
  }

  ## The height rule below needs an UNWEIGHTED MEAN of building_presence. When
  ## presence is itself a requested band under that same statistic, the loop has
  ## already computed exactly that vector, so it is captured here and reused
  ## rather than extracted a second time. The duplicate read cost roughly 30% of
  ## the native pass (2.03 s/km2 through the pipeline against 1.40 s/km2
  ## extracting the same sections directly).
  ##
  ## The reuse is conditional and deliberately narrow. `funs[[b]]` is
  ## user-overridable, so a caller asking for a summed or quantile presence
  ## produces a vector that is NOT the mean the rule wants; and with weights set
  ## the loop computes weighted_mean, which is a different number again. In
  ## either case `pres` stays NULL and the rule falls back to its own extraction.
  ## Substituting the wrong statistic here would not error, it would silently
  ## move which heights become NA.
  pres <- NULL

  for (b in bands) {
    if (!b %in% names(r)) next
    fn <- funs[[b]]
    use_wt <- !is.null(wt)
    if (use_wt) {
      ## exactextractr ignores `weights` unless the operation is a weighted_*
      ## one, and only warns. Map to the weighted variant, or drop the weights
      ## for this band rather than returning an unweighted number that looks
      ## weighted.
      wfn <- paste0("weighted_", fn)
      if (wfn %in% .OBT_WEIGHTED_OPS) {
        fn <- wfn
      } else {
        warning("No weighted variant of '", fn, "' exists; extracting band ", b,
                " unweighted.", call. = FALSE)
        use_wt <- FALSE
      }
    }

    ## `fn` is now the RESOLVED statistic, which is what the scale rules key on.
    if (coarse && fn %in% .OBT_UNSCALABLE_OPS) {
      stop("Statistic '", fn, "' for band ", b, " has no defined correction at ",
           "a reduced resolution: it counts pixels rather than measuring a ",
           "quantity, so it does not rescale. Either read this band natively ",
           "or choose a statistic that does.", call. = FALSE)
    }

    failed <- FALSE
    v <- tryCatch({
      if (use_wt) {
        exactextractr::exact_extract(r[[b]], shp_u, fun = fn, weights = wt,
                                     progress = FALSE)
      } else {
        exactextractr::exact_extract(r[[b]], shp_u, fun = fn, progress = FALSE)
      }
    }, error = function(e) {
      warning("Extraction failed for band ", b, ": ", conditionMessage(e), call. = FALSE)
      failed <<- TRUE
      rep(NA_real_, n)
    })
    res[[b]] <- as.numeric(v)

    ## Overviews hold the mean of each parent block, so an extensive statistic
    ## read from one is short by the square of the decimation. This is the whole
    ## correctness content of read_res: get it wrong and the number still looks
    ## like a building count.
    if (coarse && fn %in% .OBT_SUM_OPS) res[[b]] <- res[[b]] * decim^2

    ## `failed` matters: on error the vector is all-NA, and reusing that would
    ## turn a failed presence read into "presence is not positive anywhere",
    ## quietly disabling the height rule instead of retrying it.
    if (identical(b, "building_presence") && !use_wt &&
        identical(fn, "mean") && !failed) {
      pres <- res[[b]]
    }
  }

  ## Height needs the empty-polygon rule applied here rather than downstream.
  ## The bands store 0 over empty ground, not nodata, so a plain mean returns 0
  ## for a polygon with no buildings. Zero metres is not a height measurement,
  ## so it becomes NA. Presence is read from the same open raster, so this
  ## costs no additional I/O.
  if ("building_height" %in% bands && "building_presence" %in% names(r)) {
    if (is.null(pres)) {
      pres <- tryCatch(
        as.numeric(exactextractr::exact_extract(r[["building_presence"]], shp_u,
                                                fun = "mean", progress = FALSE)),
        error = function(e) rep(NA_real_, n))
    }
    res[["building_height"]][!is.na(pres) & pres <= 0] <- NA_real_
  }

  ## Only now, with a tile confirmed present, is a missing count or presence
  ## genuinely "no buildings" rather than "no data". The early return above
  ## leaves all-NA when no tile covers the chunk, which must stay NA.
  .obt_fill_empty(res, bands)
}

#' Run the chunked extraction over every requested year
#'
#' Factored out so that the polygon path and the buffered-survey path run the
#' SAME extraction rather than one of them inheriting the other's numbers.
#'
#' @param target sf, the geometries statistics are actually computed on
#' @return `target` with one column per band and year appended
#' @keywords internal
.obt_extract_years <- function(target, year, bands, funs, area_crs, cache_dir,
                               use_cache, chunk_size, weight_band, quiet,
                               read_res = NULL) {
  shp_key <- .obt_shp_key(target)
  bbox    <- as.numeric(sf::st_bbox(sf::st_transform(target, 4326)))
  ## The pyramid invariant is per extraction, not per session: a second call
  ## asking for a different read_res must establish its own expectation.
  .obt_reset_read_state()

  for (yr in year) {
    cache_fn <- .obt_result_path(cache_dir, shp_key, yr, bands, funs, area_crs,
                                 read_res = read_res)
    if (use_cache && file.exists(cache_fn)) {
      if (!quiet) message("Using cached results for ", yr, ": ", basename(cache_fn))
      vals <- readRDS(cache_fn)
    } else {
      idx    <- .obt_tile_index(yr, cache_dir = cache_dir, bbox = bbox, quiet = quiet)
      chunks <- .obt_chunks_tiled(target, idx, chunk_size = chunk_size)
      if (!quiet) {
        message("Extracting ", yr, ": ", nrow(target), " polygons in ",
                length(chunks), " chunks across ",
                length(unique(vapply(chunks, function(k) k$epsg, integer(1)))),
                " UTM zone(s)")
      }
      vals <- as.data.frame(matrix(NA_real_, nrow = nrow(target), ncol = length(bands),
                                   dimnames = list(NULL, bands)))
      pb <- if (!quiet) utils::txtProgressBar(min = 0, max = length(chunks), style = 3) else NULL
      for (k in seq_along(chunks)) {
        ch <- chunks[[k]]
        v  <- .obt_extract_chunk(target[ch$idx, ], idx, ch$epsg, bands, funs,
                                 weight_band = weight_band, read_res = read_res)
        vals[ch$idx, ] <- v
        if (!is.null(pb)) utils::setTxtProgressBar(pb, k)
      }
      if (!is.null(pb)) close(pb)
      ## The empty-polygon convention is applied per chunk, inside
      ## .obt_extract_chunk(), where a covering tile is known to exist. Rows
      ## still NA here fell outside the product's coverage and stay NA.
      if (use_cache) saveRDS(vals, cache_fn)
    }
    ## suffixed wide columns, matching geolink_population's paste0(name, "_", year)
    for (b in bands) target[[paste0(b, "_", yr)]] <- vals[[b]]
  }
  target
}

#' Apply the empty-polygon convention
#'
#' A polygon with no buildings genuinely has zero buildings and zero built
#' share, so the count and presence bands are set to 0. A mean height over
#' ground with no buildings is not a measurement, so height stays NA. The
#' asymmetry is deliberate: do not collapse it into a single rule.
#'
#' Call this only once a covering tile is known to exist. A polygon outside the
#' product's coverage must keep NA throughout, because "no data" and "no
#' buildings" are different claims.
#' @keywords internal
.obt_fill_empty <- function(df, bands) {
  for (b in intersect(bands, c("building_fractional_count", "building_presence"))) {
    df[[b]][is.na(df[[b]])] <- 0
  }
  df
}

#' Count polygons smaller than a given number of native pixels
#' @keywords internal
.obt_small_polygons <- function(shp_dt, area_crs, pixel_m2 = 0.25, threshold = 10) {
  a <- suppressWarnings(as.numeric(sf::st_area(sf::st_transform(shp_dt, area_crs))))
  sum(a / pixel_m2 < threshold, na.rm = TRUE)
}
