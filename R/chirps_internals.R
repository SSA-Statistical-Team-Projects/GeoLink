chirpname_annual <- function(start_year,
                             end_year) {
  time_list <- check_valid_annual(
    start_year = start_year,
    end_year = end_year
  )

  ### create the names
  year_list <- seq(start_year, end_year, 1)

  year_dt <- data.table::data.table(
    year = year_list,
    version = "chirps-v2.0.",
    ext = ".tif"
  )

  year_dt[, full_link := paste0(version, year, ext)]

  return(year_dt$full_link)
}



### download function for getting the actual files
download_worker <- function(dsn,
                            url) {
  download.file(
    url = url,
    destfile = paste(dsn, basename(url), sep = "/"),
    mode = "wb"
  )

  R.utils::gunzip(paste(dsn, basename(url), sep = "/"),
    remove = TRUE,
    overwrite = TRUE
  )
}

## check that the url actually exists
checkurl_exist <- function(url) {
  HTTP_STATUS_OK <- 200

  hd <- httr::HEAD(url)

  status <- hd$all_headers[[1]]$status

  test_result <- list(
    exists = status == HTTP_STATUS_OK,
    status = status
  )


  return(test_result$exists)
}


################################################################################

#' @import data.table

#### functions for the monthly chirps pulls
chirpname_monthly <- function(start_date,
                              end_date,
                              repo_interval = "month",
                              filename_tag = "chirps-v2.0.",
                              file_ext = ".tif.gz") {
  time_list <- check_valid_month(
    start_date = start_date,
    end_date = end_date
  )

  #### create list of query times from start time to end time
  dt <- data.table(pull_date = seq(start_date,
    end_date,
    by = repo_interval
  ))

  parse_list <- c("filename_tag", "year", "month")

  dt[, (parse_list) := list(
    filename_tag,
    year(pull_date),
    sprintf("%02d", month(pull_date))
  )]

  dt[, year_month := paste(year, month, sep = ".")]

  dt[, filename := paste0(
    filename_tag,
    year_month,
    file_ext
  )]

  return(dt)
}


check_valid_month <- function(start_date,
                              end_date) {
  ### first make sure start_date and end_date are dates
  if (lubridate::is.Date(start_date) == FALSE) {
    stop("start_date argument is not a Date, did you specify it in the form as.Date('yyyy-mm-dd')")
  }

  if (lubridate::is.Date(end_date) == FALSE) {
    stop("end_date argument is not a Date, did you specify it in the form as.Date('yyyy-mm-dd')")
  }


  if (start_date > end_date) stop("Invalid time range, start time exceeds end time!")

  ### put together the list of year-months to be pulled
  start_month <- lubridate::format_ISO8601(as.Date(start_date), precision = "ym")
  end_month <- lubridate::format_ISO8601(as.Date(end_date), precision = "ym")



  ### creates the date-times
  return(list(
    start_time = start_month,
    end_time = end_month
  ))
}


#################################################################################
#### functions for the annual chirps pulls
check_valid_annual <- function(start_year,
                               end_year) {
  if (as.integer(start_year) == FALSE | as.integer(end_year) == FALSE) {
    stop("start and end year must be integers")
  }

  if (start_year > end_year) stop("Invalid time range, start year exceeds end year")

  return(list(
    start_year = start_year,
    end_year = end_year
  ))
}
