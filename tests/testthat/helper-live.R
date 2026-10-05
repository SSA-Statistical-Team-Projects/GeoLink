## Tests that download from live servers (data providers, STAC catalogues, OpenStreetMap,
## GADM) run only when GEOLINK_LIVE_TESTS=true, as in the scheduled full-suite workflow
## (.github/workflows/live-tests.yaml). Without it (pull-request checks, a plain R CMD check)
## only the offline tests run.
skip_if_not_live <- function() {
  if (!identical(tolower(Sys.getenv("GEOLINK_LIVE_TESTS")), "true")) {
    testthat::skip("live-server test; set GEOLINK_LIVE_TESTS=true to run it")
  }
  invisible(TRUE)
}
