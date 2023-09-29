################################################################################
################## INTERNAL FUNCTIONS TO SUPPORT DOWNLOADING ###################
################################################################################

#' Basic function for downloading
#' @param dsn a folder location
#' @param url a link URL
#'

### download function for getting any files from website
download_worker <- function(dsn,
                            url) {

  download.file(url = url,
                destfile = paste(dsn, basename(url), sep = "/"),
                mode = "wb")

}

#' A function to create dictionary of file extensions and the functions that
#' read them so that a do.call can be applied to read in the results
#'

download_dictionary <- function(){

  dict_dt <- data.table(file_ext = c("tiff", "tif", "gz"),
                        opener_function = c("raster", "raster", "gunzip_reader"))

  return(dict_dt)
}

#' A function to download and read in a file from the internet
#'
#' @param url a link URL
#' @param shp_dt a shapefile to be used to crop raster
#' @param ... additional arguments to be used in `download.file()`
#'

download_reader <- function(url,
                            shp_dt,
                            ...) {

  ext <- paste0(".", tools::file_ext(url))

  temp_file <- tempfile(fileext = ext)

  download.file(url = url,
                destfile = temp_file,
                mode = "wb",
                ...)

  dict_dt <- download_dictionary()

  opener_chr <- dict_dt[file_ext == tools::file_ext(temp_file), opener_function]

  raster_obj <- do.call(opener_chr, list(temp_file))

  raster_obj <- crop(raster_obj, extent(shp_dt))

  return(raster_obj)

}


#' A function to unzip a download
#'
#' @param filename the filenames of the downloads
#'

gunzip_reader <- function(filename){

  unzip_file <- R.utils::gunzip(filename, remove = FALSE)

  raster_obj <- raster(unzip_file)

  return(raster_obj)

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
