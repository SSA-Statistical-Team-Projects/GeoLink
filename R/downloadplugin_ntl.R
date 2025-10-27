# globals
utils::globalVariables(c(
  "ntl_date"
))

#' Download Monthly Night Time Lights Data
#'
#' Functions to download night time lights data from NASA's Earth Observation
#' Group's (EOG) database. The downloaded rasters are collected for different
#' time intervals, daily, monthly and annually.
#'
#' @param start_date A character, the starting date of interest
#' @param end_date A character, the ending date of interest
#' @param version A string character, for now only "v10" is supported more to come later
#' @param slc_type A character, adjustment type made to the NTL data either "vcmcfg" or
#' "vcmslcfg"
#' @param indicator A character, specifying the specific indicator of interest. Options are
#' "avg_rade9h", "avg_rade9h.masked", "cf_cvg" or "cvg"
#' @param link_base character, the base link to the EOG webpage/database
#' @param no_tile character, tile or not (see usage within the upstream NTL function)
#'
#' @import rvest
#'
#' @export
#'

get_month_ntl <- function(start_date,
                          end_date,
                          version,
                          no_tile,
                          slc_type,
                          indicator = c("avg_rade9h",
                                        "cf_cvg",
                                        "cvg",
                                        "avg_rade9h.masked"),
                          link_base = "https://eogdata.mines.edu/nighttime_light") {

  ### read in the data
  date_dt <- data.table::data.table(ntl_date = seq(start_date, end_date, "day"))

  date_dt[, c("year",
              "month",
              "day") := tstrsplit(ntl_date,
                                  "-",
                                  fixed = TRUE)]

  date_dt <- unique(date_dt[ ,c("year", "month")])

  ### match slc_type
  slc_type <- match.arg(slc_type,
                        c("vcmcfg", "vcmslcfg"),
                        several.ok = FALSE)

  url_link <- mapply(FUN = construct_month_link,
                     year = date_dt$year,
                     month = date_dt$month,
                     version = rep(version, nrow(date_dt)),
                     slc_type = rep(slc_type, nrow(date_dt)),
                     no_tile = rep(no_tile, nrow(date_dt)),
                     SIMPLIFY = FALSE)



  if (no_tile == TRUE) {

    indicator <- match.arg(indicator, c("avg_rade9h",
                                        "cf_cvg",
                                        "cvg",
                                        "avg_rade9h.masked"),
                           several.ok = FALSE)
  }

  #### select the set of indicators we want
  indicator <- paste0(indicator, ".tif")

  url_link <- lapply(url_link,
                     function(x){

                       base_x <- basename(x)

                       y <- base_x[grepl(indicator, base_x)]

                       if (is.null(y)){

                         date_chr <- substr(base_x[[1]],
                                            11,
                                            27)

                         warning(paste0(indicator,
                                        " is missing from the EOG NTL database",
                                        " for the month in",
                                        date_chr))

                       }

                       full_link <- x[grepl(y, x)]

                       return(full_link)

                     })

  url_link <- unlist(url_link)

  ### download the data


  print("Downloading rasters...")
  raster_list <-
    lapply(X = url_link,
           FUN = ntl_download_reader)


  return(raster_list)



}

#' Download Annual Night Time Lights Data
#'
#' Functions to download night time lights data from NASA's Earth Observation
#' Group's (EOG) database. The downloaded rasters are collected for different
#' time intervals, daily, monthly and annually.
#'
#' @param year An integer, for the year of interest (yyyy), one year at a time
#' @param version A string character, for now only "v10" is supported more to come later
#' @param indicator A character, specifying the specific indicator of interest. Options are
#' "average", "average_masked", "cf_cvg", "cvg", "lit_mask", "maximum", "median",
#' "median_masked" and "minimum"
#' @param link_base character, the link to the base website or database
#' @export
#'



get_annual_ntl <- function(year,
                           version,
                           link_base = "https://eogdata.mines.edu/nighttime_light/annual/",
                           indicator = c("average", "average_masked", "cf_cvg", "cvg",
                                         "lit_mask", "maximum", "median", "median_masked",
                                         "minimum")) {
  ### construct the link
  url_link <- construct_year_link(
    year = year,
    version = version
  )

  ### find the list of links on the page
  webpage <- rvest::read_html(httr::GET(url_link, config = httr::config(ssl_verifypeer = FALSE)))

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

  raster_list <-
  lapply(X = download_links,
         FUN = ntl_download_reader)


  return(raster_list)
  unlink(tempdir(), recursive = TRUE)
}
