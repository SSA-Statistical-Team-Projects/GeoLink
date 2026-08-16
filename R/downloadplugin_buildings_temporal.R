#' Download and extract Google Open Buildings 2.5D Temporal metrics
#'
#' Extracts annual building presence, fractional building count and building
#' height from Google's Open Buildings 2.5D Temporal dataset onto a set of
#' polygons or a geocoded survey. The product publishes one layer per year from
#' 2016 to 2023, which makes it the right source when the analysis year is not
#' the year of the latest inference. It covers roughly 58 million square
#' kilometres across Africa, South and South-East Asia, Latin America and the
#' Caribbean.
#'
#' The data are read anonymously over public HTTPS from Google Cloud Storage.
#' No Earth Engine account, credential or payment is involved.
#'
#' @section Bands and their default statistics:
#' The three bands are not interchangeable, and a single statistic is wrong for
#' at least one of them. With `extract_fun = NULL` each band gets the statistic
#' that makes it interpretable:
#' \describe{
#'   \item{`building_fractional_count`}{`"sum"`. Pixels hold fractional counts,
#'     so the sum over a polygon is a building count. A mean would be buildings
#'     per pixel, which is not a quantity anyone wants.}
#'   \item{`building_height`}{`"mean"`. Metres.}
#'   \item{`building_presence`}{`"mean"`. A 0 to 1 fraction, so the mean is the
#'     built share of the polygon.}
#' }
#' Passing a single string applies it to every band, and a named character
#' vector overrides individual bands, so the usual GeoLink convention remains
#' available.
#'
#' @section Presence-weighted height:
#' A plain mean of `building_height` averages tall buildings together with empty
#' ground, so it comes out close to height multiplied by built share and is
#' nearly collinear with `building_presence`. For mean height conditional on
#' buildings being present, pass the presence band as the weights. Because both
#' bands live in the same file, this costs no extra reads. See the examples.
#'
#' @section Empty polygons:
#' A polygon containing no buildings returns 0 for `building_fractional_count`
#' and `building_presence`, because zero buildings and zero built share are true
#' observations. It returns `NA` for `building_height`, because a mean height
#' over ground with no buildings is not a measurement. This asymmetry is
#' deliberate.
#'
#' @section Reduced-resolution reads:
#' The tiles carry internal overview pyramids, and `read_res` reads from one
#' instead of the native 0.5 m grid. Google's own documentation puts the
#' effective inference resolution near 4 m, upsampled onto the 0.5 m grid, and
#' that holds up when measured: on Colombian rural sections a 4 m read agrees
#' with native to within 0.52 percent on all three bands, and to within 0.044
#' percent on genuinely built-up ones, while touching a sixty-fourth of the
#' pixels. Only resolutions the pyramid actually carries are accepted; a value
#' between two levels is an error rather than a silent snap to the nearer one.
#'
#' Two consequences are worth knowing before using it.
#'
#' Summed statistics are rescaled. An overview stores the mean of each parent
#' block, so a sum read at 4 m is a sixty-fourth of the native sum. The
#' correction is applied wherever the resolved statistic is a sum, which
#' includes `extract_fun = "sum"` on the height band, and not applied where it
#' is a mean, which includes `extract_fun = "mean"` on the count band.
#'
#' Weighting is refused. Presence-weighted height drifts 4.6 to 13.9 percent at
#' 4 m, because coarsening averages away the within-polygon presence variation
#' the weights exist to exploit. That is structural rather than a property of
#' near-empty polygons, so `weight_raster` together with a coarsened `read_res`
#' is an error. Run weighted statistics natively, in a separate call.
#'
#' @section Caching:
#' Unlike the other GeoLink plugins, which stage into `tempdir()` and unlink,
#' this function caches to `rappdirs::user_cache_dir("GeoLink")`. The reason is
#' not bandwidth, since no imagery is ever downloaded. It is that re-extracting
#' several hundred thousand polygons is expensive, and a session-scoped
#' temporary directory cannot serve that. Two artefacts are cached, the tile
#' index and the extracted results, and both keys carry a version string so a
#' change to the extraction logic invalidates them. Clear the cache with
#' [geolink_clear_cache()].
#'
#' @param shp_dt An object of class 'sf', 'data.frame' containing polygons or
#'   multipolygons representing the study area.
#' @param shp_fn A character, file path for the shapefile (.shp) to be read
#'   (for STATA users only).
#' @param year An integer vector of one or more years between 2016 and 2023.
#'   Defaults to the most recent available year. Several years return suffixed
#'   columns, for example `building_presence_2018`, rather than long format.
#' @param indicators A character vector selecting bands, or "ALL" (the default)
#'   for all three. Valid values are "building_fractional_count",
#'   "building_height" and "building_presence".
#' @param extract_fun A character, the statistic used in extraction. `NULL`
#'   (the default) applies the per-band defaults described above. A single
#'   string applies that statistic to every band. A named character vector
#'   overrides individual bands.
#' @param grid_size A numeric, the grid size in metres used to tile the study
#'   area before extraction (optional).
#' @param survey_dt An object of class "sf", "data.frame", a geocoded household
#'   survey with latitude and longitude values (optional).
#' @param survey_fn A character, file path for a geocoded survey (.dta format)
#'   (for STATA users only) (optional).
#' @param survey_lat A character, latitude variable from the survey (for STATA
#'   users only) (optional).
#' @param survey_lon A character, longitude variable from the survey (for STATA
#'   users only) (optional).
#' @param buffer_size A numeric, buffer size in metres around each survey point
#'   (optional).
#' @param survey_crs An integer, the CRS for the survey data. Default 4326.
#' @param chunk_size An integer, the maximum number of polygons processed per
#'   windowed read. Default 200.
#' @param chunk_area_km2 A numeric, the maximum summed polygon area per windowed
#'   read, in square kilometres measured in `area_crs`. Default 60. A chunk
#'   closes when either this or `chunk_size` would be exceeded, so the two
#'   strata bind on different caps without being told apart: dense urban
#'   manzanas hit the count cap and never the area cap, large sparse rural
#'   sections hit the area cap and never the count cap. `NULL` disables the area
#'   cap and restores pure count-based chunking.
#'
#'   The area cap exists because `chunk_size` cannot control rural work at all.
#'   Chunks are formed within a tile, and a rural tile holds a median of three
#'   sections, so there is usually nothing for a count cap to split: measured on
#'   Colombian rural sections, every `chunk_size` from 5 to 400 produced
#'   identical chunks and identical wall clock to within 4 percent. Extraction
#'   cost tracks summed area and spatial scatter, not polygon count.
#'
#'   Changing this changes only which polygons share a windowed read. It cannot
#'   change an extracted value, so it is deliberately absent from the result
#'   cache key: results cached under one setting remain valid under another.
#' @param cache_dir A character, an override for the cache root. `NULL` uses
#'   `rappdirs::user_cache_dir("GeoLink")`.
#' @param area_crs A character, the equal-area CRS used for any area-denominated
#'   quantity and for the small-polygon report. Defaults to "ESRI:102033", South
#'   America Albers Equal Area Conic. Use a projection appropriate to your
#'   region. A national conformal CRS is not a substitute.
#' @param weight_raster Either a band name such as "building_presence", which is
#'   read from the same file at no extra cost, or a raster object of class
#'   `SpatRaster`. `NULL` (the default) applies no weighting.
#' @param read_res A numeric, the resolution in metres to read at. `NULL` (the
#'   default) reads the native 0.5 m grid. Any other value must match one of the
#'   resolutions the tiles' internal overviews actually carry, which on this
#'   product includes 1, 2, 4 and 8 metres; the error message lists them. See
#'   the reduced-resolution section above for the rescaling of summed statistics
#'   and for why this cannot be combined with `weight_raster`.
#' @param use_cache A logical, whether to read and write cached results.
#'   Default TRUE.
#' @param quiet A logical, suppress progress messages. Default FALSE.
#'
#' @return An `sf` object, `shp_dt` with one appended column per band and year.
#'
#' @seealso [geolink_buildings()] for WorldPop's building footprint rasters,
#'   which are derived from Ecopia Digitize Africa and therefore cover
#'   sub-Saharan Africa only. Use that function for African applications and
#'   this one elsewhere, or where an annual series is needed.
#' @seealso [geolink_clear_cache()] to clear cached tile indices and results.
#'
#' @examples
#' \dontrun{
#' \donttest{
#'
#' # all three bands, 2018
#' dt <- geolink_buildings_temporal(shp_dt = shp_dt, year = 2018)
#'
#' # several years, suffixed columns
#' dt <- geolink_buildings_temporal(shp_dt = shp_dt,
#'                                  year = c(2016, 2018, 2023),
#'                                  indicators = "building_presence")
#'
#' # mean height where buildings exist, rather than height x built share.
#' # The presence band is read from the same file, so nothing is fetched twice.
#' dt <- geolink_buildings_temporal(shp_dt        = shp_dt,
#'                                  year          = 2018,
#'                                  indicators    = "building_height",
#'                                  weight_raster = "building_presence")
#'
#' # large rural polygons read from the 4 m overview: a sixty-fourth of the
#' # pixels, and within half a percent of native on all three bands. The count
#' # band's sum is rescaled for you.
#' dt <- geolink_buildings_temporal(shp_dt   = rural_sections,
#'                                  year     = 2018,
#'                                  read_res = 4)
#'
#' }}
#' @export

geolink_buildings_temporal <- function(shp_dt = NULL,
                                       shp_fn = NULL,
                                       year = NULL,
                                       indicators = "ALL",
                                       extract_fun = NULL,
                                       grid_size = NULL,
                                       survey_dt = NULL,
                                       survey_fn = NULL,
                                       survey_lat = NULL,
                                       survey_lon = NULL,
                                       buffer_size = NULL,
                                       survey_crs = 4326,
                                       chunk_size = 200L,
                                       chunk_area_km2 = 60,
                                       cache_dir = NULL,
                                       area_crs = "ESRI:102033",
                                       weight_raster = NULL,
                                       read_res = NULL,
                                       use_cache = TRUE,
                                       quiet = FALSE) {

  ## ---- validation ---------------------------------------------------------
  if (is.null(shp_dt) && is.null(shp_fn)) {
    stop("Supply either shp_dt or shp_fn.")
  }
  if (!is.null(shp_fn)) shp_dt <- sf::read_sf(shp_fn)
  if (!inherits(shp_dt, "sf")) stop("shp_dt must be an sf object.")
  if (is.na(sf::st_crs(shp_dt))) stop("shp_dt has no CRS; set one before calling.")
  ## An empty sf otherwise reaches .obt_zones_for_bbox() with an all-NA bbox and
  ## fails there as "'from' must be a finite number", which says nothing about
  ## the actual problem. A filter that matched no rows is the usual cause.
  if (nrow(shp_dt) == 0L) stop("shp_dt has no rows; nothing to extract.")

  if (is.null(year)) year <- max(.OBT_YEARS)
  year <- as.integer(year)
  bad <- setdiff(year, .OBT_YEARS)
  if (length(bad)) {
    stop("year must be between ", min(.OBT_YEARS), " and ", max(.OBT_YEARS),
         "; got ", paste(bad, collapse = ", "))
  }

  bands <- if (identical(indicators, "ALL")) .OBT_BANDS else indicators
  bad <- setdiff(bands, .OBT_BANDS)
  if (length(bad)) {
    stop("Unknown indicator(s): ", paste(bad, collapse = ", "),
         ". Valid: ", paste(.OBT_BANDS, collapse = ", "))
  }

  ## per-band statistics
  funs <- stats::setNames(lapply(bands, .obt_default_fun), bands)
  if (!is.null(extract_fun)) {
    if (length(extract_fun) == 1L && is.null(names(extract_fun))) {
      funs <- stats::setNames(as.list(rep(extract_fun, length(bands))), bands)
    } else {
      if (is.null(names(extract_fun))) {
        stop("extract_fun must be a single string or a NAMED vector of band = statistic.")
      }
      bad <- setdiff(names(extract_fun), .OBT_BANDS)
      if (length(bad)) stop("extract_fun names not recognised: ", paste(bad, collapse = ", "))
      for (nm in names(extract_fun)) if (nm %in% bands) funs[[nm]] <- unname(extract_fun[[nm]])
    }
  }

  if (chunk_size < 1L) stop("chunk_size must be a positive integer.")
  if (!is.null(chunk_area_km2)) {
    if (!is.numeric(chunk_area_km2) || length(chunk_area_km2) != 1L ||
        !is.finite(chunk_area_km2) || chunk_area_km2 <= 0) {
      stop("chunk_area_km2 must be a single positive number of square ",
           "kilometres, or NULL to disable the area cap.")
    }
  }

  ## weight handling: a band name is resolved inside the chunk raster, a
  ## SpatRaster is passed through to exactextractr unchanged.
  weight_band <- NULL
  weight_obj  <- NULL
  if (!is.null(weight_raster)) {
    if (is.character(weight_raster)) {
      if (!weight_raster %in% .OBT_BANDS) {
        stop("weight_raster as a character must be one of: ",
             paste(.OBT_BANDS, collapse = ", "))
      }
      weight_band <- weight_raster
    } else if (inherits(weight_raster, "SpatRaster")) {
      weight_obj <- weight_raster
    } else {
      stop("weight_raster must be a band name or a SpatRaster.")
    }
  }

  ## ---- read resolution -----------------------------------------------------
  if (!is.null(read_res)) {
    if (!is.numeric(read_res) || length(read_res) != 1L || !is.finite(read_res) ||
        read_res <= 0) {
      stop("read_res must be a single positive number of metres, or NULL for ",
           "the native grid.")
    }
    if (read_res < .OBT_NATIVE_RES * (1 - 1e-6)) {
      stop("read_res = ", read_res, " m is finer than the product's native ",
           .OBT_NATIVE_RES, " m grid. There is nothing to read there; ",
           "upsampling would invent detail rather than recover it.")
    }
    if (read_res <= .OBT_NATIVE_RES * 1.02) {
      ## a request for native, spelled out. Take the native path so the cache
      ## key and the extraction agree with an ordinary read_res = NULL call.
      read_res <- NULL
    }
  }
  if (!is.null(read_res) && !is.null(weight_raster)) {
    stop("weight_raster cannot be combined with a coarsened read_res. ",
         "Weighting depends on the weight band varying within the polygon, and ",
         "coarsening averages exactly that variation away: presence-weighted ",
         "height measured 4.6% to 13.9% away from its native value at 4 m, on ",
         "built-up polygons as much as on empty ones. The bias is large, ",
         "one-directional and invisible in the output, so this is refused ",
         "rather than warned about. Run the weighted statistic in its own call ",
         "with read_res = NULL.")
  }

  ## ---- prepare polygons (grid_size handling, as in the other plugins) -----
  shp_dt <- zonalstats_prepshp(shp_dt = shp_dt, grid_size = grid_size)

  ## ---- choose what the statistics are actually computed on -----------------
  ## postdownload_processor() in zonalstats_plugin.R sets the convention: when a
  ## survey is supplied with a buffer, the statistics are computed ON the
  ## buffered survey geometries. Extracting on shp_dt and spatially joining the
  ## survey to it instead would give every household the value of whichever
  ## polygon contains it, so for a single study-area polygon every row would
  ## come back with the same number -- a plausible-looking column that is not
  ## the household's neighbourhood at all.
  survey_mode <- !is.null(survey_dt) || !is.null(survey_fn)

  if (survey_mode) {
    survey_dt <- zonalstats_prepsurvey(survey_dt = survey_dt,
                                       survey_fn = survey_fn,
                                       survey_lat = survey_lat,
                                       survey_lon = survey_lon,
                                       buffer_size = buffer_size,
                                       survey_crs = survey_crs)
    if (is.null(buffer_size)) {
      stop("buffer_size is required when a survey is supplied: these are areal ",
           "statistics and a zero-area point has nothing to summarise over. ",
           "Pass a radius in metres, e.g. buffer_size = 250.")
    }
    target <- survey_dt
  } else {
    target <- shp_dt
  }

  n_small <- .obt_small_polygons(target, area_crs)
  if (!quiet && n_small > 0) {
    message(n_small, " of ", nrow(target),
            " polygons are smaller than 10 native pixels. Coverage-fraction ",
            "weighting is used throughout (exactextractr), so these still ",
            "receive a value rather than falling back to centroid sampling.")
  }

  ## ---- extraction, per year ------------------------------------------------
  target <- .obt_extract_years(target, year = year, bands = bands, funs = funs,
                               area_crs = area_crs, cache_dir = cache_dir,
                               use_cache = use_cache, chunk_size = chunk_size,
                               weight_band = weight_band, quiet = quiet,
                               read_res = read_res,
                               chunk_area_km2 = chunk_area_km2)

  attr(target, "geolink_area_crs")           <- area_crs
  attr(target, "geolink_extraction_version") <- .OBT_EXTRACTION_VERSION
  attr(target, "geolink_read_res")           <- if (is.null(read_res)) .OBT_NATIVE_RES else read_res

  if (survey_mode && !is.null(survey_fn)) return(sf::st_drop_geometry(target))
  if (!survey_mode && !is.null(shp_fn))   return(sf::st_drop_geometry(target))

  if (!quiet) message("Process Complete!!!")
  target
}


#' Clear the GeoLink cache
#'
#' Removes cached tile indices and cached extraction results written by
#' [geolink_buildings_temporal()]. No imagery is ever cached, so this only
#' discards derived artefacts and any subsequent call simply recomputes them.
#'
#' Cache keys carry a version string, so an upgrade of GeoLink that changes the
#' extraction logic invalidates old entries automatically. Call this when you
#' want to reclaim disk space, or to force a rebuild after changing something
#' the key does not capture.
#'
#' @param cache_dir A character, an override for the cache root. `NULL` uses
#'   `rappdirs::user_cache_dir("GeoLink")`.
#' @param what A character, "all" (default), "index" for tile indices only, or
#'   "results" for extracted results only.
#' @param quiet A logical, suppress messages. Default FALSE.
#'
#' @return Invisibly, the number of files removed.
#'
#' @examples
#' \dontrun{
#' geolink_clear_cache()                  # everything
#' geolink_clear_cache(what = "results")  # keep the tile index
#' }
#' @export
geolink_clear_cache <- function(cache_dir = NULL, what = "all", quiet = FALSE) {
  what <- match.arg(what, c("all", "index", "results"))
  cd <- .obt_cache_dir(cache_dir)
  pat <- switch(what,
                all     = "^(tile_index|res)_v",
                index   = "^tile_index_v",
                results = "^res_v")
  fs <- list.files(cd, pattern = pat, full.names = TRUE)
  if (!length(fs)) {
    if (!quiet) message("Nothing to clear in ", cd)
    return(invisible(0L))
  }
  ok <- file.remove(fs)
  if (!quiet) message("Removed ", sum(ok), " cached file(s) from ", cd)
  invisible(sum(ok))
}
