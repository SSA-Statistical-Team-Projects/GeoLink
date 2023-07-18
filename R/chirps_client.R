
#' Download monthly rainfall chirp data
#'
#' Pull rainfall data from the CHIRPS data at monthly intervals for a specified period
#' The data is downloaded in raster format (.tif)
#'
#' @param start_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#' @param end_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#' @param link_base the link to the daily chirps data (no need to change this)
#' @param resolution A character, gridded resolution of the data (options area "p05" i.e.
#'                   0.05 degree resolution and p25 for 0.25)
#' @param dsn data source name (a local folder where the data is to be stored)
#' @param cores the number of PC cores to employ in pulling the data in parallel
#'
#' @export
#'


get_month_chirps <- function(start_date,
                             end_date,
                             link_base = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/",
                             dsn = getwd(),
                             cores = 1L) {
  dt <- chirpname_monthly(
    start_date = start_date,
    end_date = end_date
  )

  dt[, sub_dir := "global_monthly/tifs/"]

  dt[, full_link := paste0(
    link_base,
    sub_dir,
    filename
  )]

  cl <- makePSOCKcluster(cores)
  on.exit(stopCluster(cl))

  ## check that the link exists
  dt$exist_status <- unlist(parLapply(
    cl = cl,
    X = dt$full_link,
    fun = checkurl_exist
  ))

  ## download the chirps
  parallel::parLapply(
    cl = cl,
    X = dt[exist_status == TRUE, full_link],
    fun = download_worker,
    dsn = dsn
  )
}



get_annual_chirps <- function(start_year,
                              end_year,
                              link_base = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/",
                              dsn = getwd(),
                              cores = 1L) {
  dt <- chirpname_annual(
    start_year = start_year,
    end_year = end_year
  )

  dt <- data.table(filename = dt)

  dt[, sub_dir := "global_annual/tifs/"]

  dt[, full_link := paste0(
    link_base,
    sub_dir,
    filename
  )]

  cl <- makePSOCKcluster(cores)
  on.exit(stopCluster(cl))

  ## check that the link exists
  dt$exist_status <- unlist(parLapply(
    cl = cl,
    X = dt$full_link,
    fun = checkurl_exist
  ))

  ## download the chirps
  parallel::parLapply(
    cl = cl,
    X = dt[exist_status == TRUE, full_link],
    fun = download_worker,
    dsn = dsn
  )
}
