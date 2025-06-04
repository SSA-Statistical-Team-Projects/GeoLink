#' Download monthly rainfall chirp data
#'
#' Pull rainfall data from the CHIRPS data at monthly intervals for a specified period
#' The data is downloaded in raster format (.tif)
#'
#' @param start_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#' @param end_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#' @param link_base the link to the daily chirps data (no need to change this)
#'
#' @import data.table parallel raster
#' @export
#'
get_month_chirps <- function(start_date,
                             end_date,
                             link_base = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/") {

  # Generate filenames for the date range
  dt <- chirpname_monthly(
    start_date = start_date,
    end_date = end_date
  )

  # Update filenames from v3.0 to v2.0
  dt[, filename := gsub("chirps-v3\\.0", "chirps-v2.0", filename)]

  # Set the correct subdirectory for v2.0
  dt[, sub_dir := "global_monthly/tifs/"]

  # Build full URLs
  dt[, full_link := paste0(
    link_base,
    sub_dir,
    filename
  )]

  # Check which URLs exist
  dt$exist_status <- unlist(lapply(X = dt$full_link,
                                   FUN = checkurl_exist))

  # Download and read the rasters (removed shp_dt parameter)
  raster_objs <- lapply(X = dt[exist_status == TRUE, full_link],
                        FUN = download_reader)

  # If no files found with checkurl_exist, try direct download
  if (length(raster_objs) == 0) {
    warning("No files found with URL check, attempting direct download...")
    raster_objs <- list()

    for (i in 1:nrow(dt)) {
      tryCatch({
        raster_obj <- download_reader(dt$full_link[i])
        if (!is.null(raster_obj)) {
          raster_objs[[length(raster_objs) + 1]] <- raster_obj
        }
      }, error = function(e) {
        warning(paste("Failed to download:", dt$filename[i]))
      })
    }
  }

  return(raster_objs)
}

#' Download annual rainfall chirp data
#'
#' Pull rainfall data from the CHIRPS data at annual intervals for a specified period
#' The data is downloaded in raster format (.tif)
#'
#' @param start_year An integer for the first year CHIRPS data to download
#' @param end_year An integer for the final year in the series of annual rasters to download
#' @param link_base the link to the daily chirps data (no need to change this)
#' @param cores the number of PC cores to employ in pulling the data in parallel
#'
#' @import data.table parallel raster
#' @export
#'
get_annual_chirps <- function(start_year,
                              end_year,
                              link_base = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/",
                              cores = 1L) {

  # Generate filenames for the year range
  dt <- chirpname_annual(
    start_year = start_year,
    end_year = end_year
  )

  dt <- data.table(filename = dt)

  # Update filenames from v3.0 to v2.0
  dt[, filename := gsub("chirps-v3\\.0", "chirps-v2.0", filename)]

  # Set the correct subdirectory for v2.0
  dt[, sub_dir := "global_annual/tifs/"]

  # Build full URLs
  dt[, full_link := paste0(
    link_base,
    sub_dir,
    filename
  )]

  # Check which URLs exist
  dt$exist_status <- unlist(lapply(X = dt$full_link,
                                   FUN = checkurl_exist))

  # Download and read the rasters (removed shp_dt parameter)
  raster_objs <- lapply(X = dt[exist_status == TRUE, full_link],
                        FUN = download_reader)

  # If no files found with checkurl_exist, try direct download
  if (length(raster_objs) == 0) {
    warning("No files found with URL check, attempting direct download...")
    raster_objs <- list()

    for (i in 1:nrow(dt)) {
      tryCatch({
        raster_obj <- download_reader(dt$full_link[i])
        if (!is.null(raster_obj)) {
          raster_objs[[length(raster_objs) + 1]] <- raster_obj
        }
      }, error = function(e) {
        warning(paste("Failed to download:", dt$filename[i]))
      })
    }
  }

  return(raster_objs)
}
