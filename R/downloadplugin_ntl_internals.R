#' @import jsonlite

ntl_downloader <- function(client_id,
                           client_secret,
                           username,
                           password,
                           grant_type,
                           token_url,
                           data_url) {
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
  download_reader(url = data_url,
                  headers = list(Authorization = auth))


  return(raster_obj)


}





##### internal functions to help pull the monthly notile data
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



#### internal functions to help pull the annual data

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






















