#' @importFrom lubridate wday second isoweek yday hour year month week minute mday quarter
#' @importFrom data.table data.table

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
  dt <- data.table(pull_date = seq(as.Date(start_date),
                                   as.Date(end_date),
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


ambiguous_date_check <- function(date_chr){

  tryCatch({

    date_check <- as.Date(date_chr, format = "%Y-%m-%d")

    ## check if the parsing is successful
    if (is.na(date_check)){

      stop("Date format error")

    }

  }, error = function(e){

    stop("a date argument you inputted is in the wrong format, it should be ('yyyy-mm-dd')")

  })

  return(date_chr)
}

check_valid_month <- function(start_date,
                              end_date) {



  ambiguous_date_check(start_date)
  ambiguous_date_check(end_date)


  if (start_date > end_date) {

    stop("Invalid time range, start time exceeds end time!")

  }

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
