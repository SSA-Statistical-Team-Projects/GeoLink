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
#' @param ... additional arguments to be used in `download.file()`
#'

download_reader <- function(url,
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

  # raster_obj <- crop(raster_obj, extent(shp_dt))

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

# Create a function to scrape file names from the directory listing
get_file_list <- function(url) {
  # Read the HTML content from the URL
  page <- read_html(url)

  # Extract the file names from the links
  files <- page %>% html_nodes("a") %>% html_attr("href")

  # Filter out directories and parent directory link
  files <- files[grepl("\\.tif$", files)]

  # Return the full URLs of the files
  return(paste0(url, files))
}

# Function to try downloading files from a URL
try_download <- function(url) {
  file_urls <- NULL
  try({
    file_urls <- get_file_list(url)
    if (length(file_urls) == 0) stop("No files found at URL")
  }, silent = TRUE)
  return(file_urls)
}

# Function to download files from a list of URLs
download_files_worldpop <- function(file_urls, UN_adjst, file_location) {  # Added file_location argument
  for (file_url in file_urls) {
    if (!is.null(UN_adjst) && UN_adjst == "Y") {
      # Download only files containing 'UNadj'
      if (grepl("UNadj", basename(file_url))) {
        destfile <- file.path(file_location, basename(file_url))  # Save to specified location
        download.file(url = file_url, destfile = destfile, mode = "wb")
      }
    } else {
      # Download all files except those containing 'UNadj'
      if (!grepl("UNadj", basename(file_url))) {
        destfile <- file.path(file_location, basename(file_url))  # Save to specified location
        download.file(url = file_url, destfile = destfile, mode = "wb")
      }
    }}}


# Function to read OpenCellID data and assign column names
read_opencellid_data <- function(file_path) {
  # Read the compressed CSV file
  cell_towers <- fread(file_path, sep = ",", header = FALSE)

  # Assign proper column names
  colnames(cell_towers) <- c("radio", "mcc", "net", "area", "cell", "unit",
                             "lon", "lat", "range", "samples", "changeable",
                             "created", "updated", "averageSignal")

  # Convert lon and lat to numeric (to handle any non-numeric values)
  cell_towers$lon <- as.numeric(cell_towers$lon)
  cell_towers$lat <- as.numeric(cell_towers$lat)

  # Remove rows with missing or invalid values in lon or lat
  cell_towers <- cell_towers[!is.na(cell_towers$lon) & !is.na(cell_towers$lat), ]

  return(cell_towers)
}
