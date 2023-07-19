#' Download Monthly Night Time Lights Data
#'
#' Functions to download night time lights data from NASA's Earth Observation
#' Group's (EOG) database. The downloaded rasters are collected for different
#' time intervals, daily, monthly and annually.
#'
#' @param year An integer, for the year of interest (yyyy)
#' @param month An integer, for the month of interest
#' @param version A string character, for now only "v10" is supported more to come later
#' @param username A character, username on NASA's Earth Observation Group database
#' @param password A character, password on NASA's Earth Observation Gruop database
#' @param slc_type A character, adjustment type made to the NTL data either "vcmcfg" or
#' "vcmslcfg"
#' @param indicator A character, specifying the specific indicator of interest. Options are
#' "avg_rade9h", "cf_cvg" or "cvg"
#' @param cores An integer, how many parallel cores to use for downloading to accelerate process
#'
#'
#' @export
#'
# username <- 'ctl1m14@soton.ac.uk'
# password <- 'Canon30v%'
# year <- "2021"
# month <- "12"
# version <- "v10"
# no_tile <- TRUE
# slc_type <- c("vcmcfg")
# indicator <- c("avg_rade9h")
# link_base <- "https://eogdata.mines.edu/nighttime_light"
# cores <- 1L

get_month_ntl <- function(username,
                          password,
                          year,
                          month,
                          version,
                          no_tile = TRUE,
                          slc_type = c("vcmcfg", "vcmslcfg"),
                          indicator = c("avg_rade9h", "cf_cvg", "cvg"),
                          link_base = "https://eogdata.mines.edu/nighttime_light",
                          cores = 4L) {
  ### create the full link
  url_link <- construct_month_link(
    year = year,
    month = month,
    version = version,
    slc_type = slc_type,
    no_tile = no_tile
  )


  if (no_tile == TRUE) {
    indicator <- match.arg(indicator, c("avg_rade9h", "cf_cvg", "cvg"), several.ok = FALSE)
  }


  cl <- makePSOCKcluster(cores)
  on.exit(stopCluster(cl))


  ### find the list of files in the url
  link_check <- valid_url(url_link)

  if (link_check == TRUE) {
    file_list <- as.data.table(readHTMLTable(content(GET(url_link), "text"))[[1]])

    file_list <- file_list[!grepl("tif.gz", Name), ]


    if (no_tile == TRUE) {
      file_list <- file_list[grepl(indicator, Name), Name]
    }

    file_list <- unlist(lapply(
      X = url_link,
      FUN = paste0,
      file_list
    ))

    ## file_list <- vrt(file_list, filename=(tools::file_path_sans_ext(file_list)), overwrite=TRUE)


    parallel::parLapply(
      cl = cl,
      X = file_list,
      fun = ntl_downloader,
      username = username,
      password = password,
      client_id = "eogdata_oidc",
      client_secret = "2677ad81-521b-4869-8480-6d05b9e57d48",
      grant_type = "password",
      token_url = "https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token"
    )
    return(file_list)
  }
}

#' Download Annual Night Time Lights Data
#'
#' Functions to download night time lights data from NASA's Earth Observation
#' Group's (EOG) database. The downloaded rasters are collected for different
#' time intervals, daily, monthly and annually.
#'
#' @param year An integer, for the year of interest (yyyy), one year at a time
#' @param version A string character, for now only "v10" is supported more to come later
#' @param username A character, username on NASA's Earth Observation Group database
#' @param password A character, password on NASA's Earth Observation Gruop database
#' @param indicator A character, specifying the specific indicator of interest. Options are
#' "average", "average_masked", "cf_cvg", "cvg", "lit_mask", "maximum", "median", "median_masked" and "minimum"
#' @param cores An integer, how many parallel cores to use for downloading to accelerate process
#'
#' @import rvest
#' @export
#'



get_annual_ntl <- function(username,
                           password,
                           year,
                           version,
                           link_base = "https://eogdata.mines.edu/nighttime_light/annual/",
                           indicator = c(
                             "average", "average_masked", "cf_cvg", "cvg",
                             "lit_mask", "maximum", "median", "median_masked",
                             "minimum"
                           ),
                           cores = 1L) {
  ### construct the link
  url_link <- construct_year_link(
    year = year,
    version = version
  )

  ### find the list of links on the page
  webpage <- rvest::read_html(url_link)

  download_links <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  download_links <- download_links[grep(
    "\\.tif.gz$|\\.TIF.GZ$",
    download_links
  )]

  download_links <- unique(download_links)

  ## select files
  indicator_list <- paste0("\\", indicator, ".tif.gz")

  search_regex <- paste(indicator, collapse = "|")

  download_links <- download_links[grep(search_regex, download_links)]

  ### create full link
  download_links <- paste0(url_link, "/", download_links)

  ### run the downloader
  cl <- makePSOCKcluster(cores)
  on.exit(stopCluster(cl))

  parallel::parLapply(
    cl = cl,
    X = download_links,
    fun = ntl_downloader,
    username = username,
    password = password,
    client_id = "eogdata_oidc",
    client_secret = "2677ad81-521b-4869-8480-6d05b9e57d48",
    grant_type = "password",
    token_url = "https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token"
  )




  return(download_links)
}
