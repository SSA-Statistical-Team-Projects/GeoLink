#' An deprecated function used to download NTL rasters from the EOG website
#' This function was useful when EOG needed a username and password. Therefore
#' this is not really in use anymore unless the opportunity necessitates
#'
#' @param username character, username for your eogdata account
#' @param password character, password for your egodata account
#' @param grant_type character, the grant type
#' @param token_url character, the token url
#' @param data_url character, the data url
#' @param shp_dt `sf`, `data.frame`, the shapefile object
#' @param client_id the client ID
#' @param client_secret the client secret created on the website
#'
#' @import jsonlite

ntl_downloader <- function(client_id,
                           client_secret,
                           username,
                           password,
                           grant_type,
                           token_url,
                           data_url,
                           shp_dt) {
  params <- list(
    client_id = client_id,
    client_secret = client_secret,
    username = username,
    password = password,
    grant_type = grant_type
  )

  response <- httr::POST(token_url,
    body = params,
    encode = "form"
  )

  access_token_list <- jsonlite::fromJSON(httr::content(response,
    as = "text",
    encoding = "UTF-8"
  ))

  access_token <- access_token_list$access_token
  # Submit request with token bearer and write to output file
  ## Change data_url variable to the file you want to download

  auth <- paste("Bearer", access_token)

  raster_obj <-
  ntl_download_reader(url = data_url,
                  headers = list(Authorization = auth),
                  shp_dt = shp_dt)


  return(raster_obj)


}



#' Construct URL to the month night time lights data
#'
#' @param year an integer, the year
#' @param month an integer, the month
#' @param version a character, the NTL version "v10"
#' @param slc_type a character, "vcmslcfg"
#' @param no_tile a logical, always set to FALSE
#' @param link_base a character, the base link to the NTL EOG database

construct_month_link <- function(year,
                                 month,
                                 version,
                                 slc_type,
                                 no_tile,
                                 link_base = "https://eogdata.mines.edu/nighttime_light") {

  slc_type <- match.arg(slc_type,
                        c("vcmcfg", "vcmslcfg"),
                        several.ok = FALSE)

  tile_type <- ifelse(no_tile == TRUE, "monthly_notile", "monthly")

  year <- as.integer(year)

  month <- sprintf("%02d", as.integer(month))

  ### construct link
  url_link <- paste(link_base,
                    tile_type,
                    version,
                    year,
                    paste0(year, month),
                    slc_type,
                    "",
                    sep = "/")

  full_link <- get_url_data(url = url_link)

  full_link <- unique(full_link)

  full_link <- paste0(url_link, full_link)

  return(full_link)

}

#' Construct URL to the annual night time lights data
#'
#' @param year an integer, the year
#' @param version a character, the NTL version ("v10", "v20", "v21" and "v22")
#' @param link_base a character, the base link to the NTL EOG database

construct_year_link <- function(year,
                                version,
                                link_base = "https://eogdata.mines.edu/nighttime_light/annual/") {
  year <- as.integer(year)

  url_link <- paste0(
    link_base,
    version,
    "/",
    year
  )

  return(url_link)
}


#' Check if the url actually works and contains data
#'
#' @param url character, the url


valid_url <- function(url,
                      t = 2) {
  con <- url(url)

  check <- suppressWarnings(try(
    open.connection(con,
      open = "rt",
      timeout = t
    ),
    silent = T
  )[1])

  suppressWarnings(try(close.connection(con),
    silent = T
  ))

  result <- ifelse(is.null(check),
    TRUE,
    FALSE
  )


  return(result)
}





##### a function to read links

#' @import stringr

get_url_data <- function(url,
                         file_type = "tif"){

  page <- read_html(url)

  data_file_links <-
    page %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset(paste0(".", file_type, "$"))

  return(data_file_links)


}






















