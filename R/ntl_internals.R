ntl_downloader <- function(client_id,
                           client_secret,
                           username,
                           password,
                           grant_type,
                           token_url,
                           data_url){
  
  params <- list(client_id = client_id,
                 client_secret = client_secret,
                 username = username,
                 password = password,
                 grant_type = grant_type)
  
  response <- httr::POST(token_url,
                         body = params,
                         encode = "form")
  
  access_token_list <- jsonlite::fromJSON(httr::content(response,
                                                        as = "text",
                                                        encoding = "UTF-8"))
  
  access_token <- access_token_list$access_token
  # Submit request with token bearer and write to output file
  ## Change data_url variable to the file you want to download
  
  auth <- paste('Bearer', access_token)
  ## You can either define the output file name directly
  # output_file <- 'EOG_sensitive_contents.txt'
  ## Or get the filename from the data_url variable
  
  output_file <- basename(data_url)
  
  download.file(data_url,
                output_file,
                mode = "wb",
                headers = list(Authorization = auth))
  
}



##### internal functions to help pull the monthly notile data
construct_month_link <- function(year,
                                 month,
                                 version,
                                 slc_type,
                                 no_tile,
                                 link_base = "https://eogdata.mines.edu/nighttime_light"){
  
  
  slc_type <- match.arg(slc_type, c("vcmcfg", "vcmslcfg"), several.ok = FALSE)
  
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
  
  return(url_link)
  
}




valid_url <- function(url,
                      t=2){
  con <- url(url)
  
  check <- suppressWarnings(try(open.connection(con,
                                                open = "rt",
                                                timeout = t),
                                silent=T)[1])
  
  suppressWarnings(try(close.connection(con),
                       silent=T))
  
  result <- ifelse(is.null(check),
                   TRUE,
                   FALSE)
  
  
  return(result)
  
}



#### internal functions to help pull the annual data

construct_year_link <- function(year,
                                version,
                                link_base = "https://eogdata.mines.edu/nighttime_light/annual/"){
  
  year <- as.integer(year)
  
  url_link <- paste0(link_base,
                     version,
                     "/",
                     year)
  
  return(url_link)
  
}

