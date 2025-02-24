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


#' A function to download and read in a file from the internet for Night Time lights
#'
#' @param url a link URL
#' @param ego_username username for your eogdata account
#' @param ego_password password for your egodata account
#' @param ... additional arguments to be used in `download.file()`
#' @param client_id the client ID
#' @param client_secret the client secret created on the website
#' @param max_retries the maximum number of retries before function time out
#' @import httr jsonlite progress tools
#'

ntl_download_reader <- function(url,
                                ego_username = "102398test@gmail.com",
                                ego_password = "Test1029384756!",
                                client_id = "eogdata_oidc",
                                client_secret = "2677ad81-521b-4869-8480-6d05b9e57d48",
                                max_retries = 3, ...) {

  # Token URL
  token_url <- "https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token"

  # Step 1: Retrieve Access Token
  get_access_token <- function() {
    params <- list(
      client_id = client_id,
      client_secret = client_secret,
      username = ego_username,
      password = ego_password,
      grant_type = "password"
    )
    response <- POST(token_url, body = params, encode = "form")
    if (status_code(response) != 200) {
      stop("Authentication failed: Unable to retrieve the access token. Check your username and password.")
    }
    return(fromJSON(content(response, as = "text", encoding = "UTF-8"))$access_token)
  }

  # Step 2: Function to refresh the access token
  refresh_access_token <- function(refresh_token) {
    params <- list(
      client_id = client_id,
      client_secret = client_secret,
      refresh_token = refresh_token,
      grant_type = "refresh_token"
    )
    response <- POST(token_url, body = params, encode = "form")
    if (status_code(response) != 200) {
      stop("Failed to refresh token.")
    }
    return(fromJSON(content(response, as = "text", encoding = "UTF-8"))$access_token)
  }

  # Get initial access token
  access_token <- get_access_token()

  # Step 3: Extract File Extension and Create Temporary File
  ext <- paste0(".", tools::file_ext(url))
  temp_file <- tempfile(fileext = ext)

  # Step 4: Initialize Progress Bar
  pb <- progress_bar$new(
    format = "  downloading [:bar] :percent in :elapsed",
    total = NA, # Total unknown, so we will manually update
    clear = FALSE,
    width = 60
  )

  # Step 5: Download with Retry Logic and Token Refresh
  attempt <- 1
  while (attempt <= max_retries) {
    try({
      req <- httr::GET(
        url,
        add_headers(Authorization = paste("Bearer", access_token)),
        write_disk(temp_file, overwrite = TRUE),
        progress()
      )

      if (status_code(req) == 200) {
        pb$terminate() # Terminate the progress bar after completion
        break
      } else if (status_code(req) == 401) {  # Token expired (401 Unauthorized)
        message("Access token expired, refreshing...")
        access_token <- refresh_access_token(access_token)  # Refresh the token
        message("Retrying download with new token...")
      } else {
        stop("Download failed with status code: ", status_code(req))
      }
    }, silent = FALSE)

    attempt <- attempt + 1
    message(paste("Retrying download (attempt", attempt, "of", max_retries, ")..."))
  }

  if (attempt > max_retries) {
    stop("Download failed after ", max_retries, " attempts.")
  }

  # Step 6: Identify and Use Appropriate Opener Function
  dict_dt <- download_dictionary()

  file_opener <- dict_dt[file_ext == tools::file_ext(temp_file), opener_function]

  if (is.null(file_opener) || file_opener == "") {
    stop("No suitable opener function found for the file extension.")
  }

  # Use the identified opener function to load the raster object
  raster_obj <- do.call(file_opener, list(temp_file))

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


# Helper function to read OpenCellID data with caching and spatial indexing
read_opencellid_data <- function(file_path) {
  if (!grepl("\\.csv$|\\.csv\\.gz$", file_path)) {
    stop("Unsupported file format. Please provide a CSV file (plain or gzipped)")
  }

  # Read data without headers
  cell_towers <- fread(file_path, header = FALSE)

  # Assign proper column names
  colnames(cell_towers) <- c("radio", "mcc", "net", "area", "cell", "unit",
                             "lon", "lat", "range", "samples", "changeable",
                             "created", "updated", "averageSignal")

  # Ensure lon and lat are numeric
  cell_towers[, `:=`(
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  )]

  # Filter invalid coordinates
  cell_towers <- cell_towers[!is.na(lon) & !is.na(lat) &
                               lon >= -180 & lon <= 180 &
                               lat >= -90 & lat <= 90]

  return(cell_towers)
}

#' Helper function to read survey data
#'
#' @param file_path character, a file location path
#'
#' @importFrom data.table as.data.table
#'
read_survey_data <- function(file_path) {
  if (grepl("\\.dta$", file_path)) {
    # Read Stata file
    dt <- as.data.table(haven::read_dta(file_path))
  } else if (grepl("\\.csv$", file_path)) {
    # Read CSV file
    dt <- fread(file_path)
  } else {
    stop("Unsupported file format. Please provide .dta or .csv file")
  }
  return(dt)
}


ensure_crs_4326 <- function(gdf) {
  # Check if input is NULL or NA
  if (is.null(gdf) || length(gdf) == 0) {
    warning("Input geodataframe is NULL or empty. Returning NULL.")
    return(NULL)
  }

  # Check if input is a valid sf or sfc object
  if (!inherits(gdf, c("sf", "sfc"))) {
    # Attempt to convert to sf if possible
    tryCatch({
      gdf <- sf::st_as_sf(gdf)
    }, error = function(e) {
      stop("Input must be an sf or sfc object and cannot be converted.")
    })
  }

  # Check current CRS
  current_crs <- sf::st_crs(gdf)

  # If CRS is completely missing
  if (is.na(current_crs)) {
    message("CRS is missing. Setting to EPSG:4326.")
    return(sf::st_set_crs(gdf, 4326))
  }

  # If CRS is not 4326
  if (is.na(current_crs$epsg) || current_crs$epsg != 4326) {
    # Handle cases with unknown or different CRS
    message(paste("Reprojecting from",
                  ifelse(is.na(current_crs$epsg),
                         "unknown CRS",
                         as.character(current_crs$epsg)),
                  "to EPSG:4326."))

    # Safely attempt transformation
    tryCatch({
      return(sf::st_transform(gdf, 4326))
    }, error = function(e) {
      stop("Unable to transform CRS. Check your spatial data.")
    })
  }

  # If already in 4326
  message("CRS is already EPSG:4326.")
  return(gdf)
}

fetch_planetary_data <- function(collection, start_date, end_date, shp_dt) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  it_obj <- s_obj %>%
    stac_search(
      collections = collection,
      bbox = sf::st_bbox(shp_dt),
      datetime = paste(start_date, end_date, sep = "/")
    ) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())

  return(it_obj)
}

crop_and_clean_raster <- function(raster_obj,
                                  shp_dt = NULL,
                                  survey_dt = NULL) {

  # Check if shp_dt is NULL and survey_dt is provided
  if (is.null(shp_dt) && !is.null(survey_dt)) {
    # If shp_dt is NULL, use survey_dt to get the extent
    cropped_raster <- terra::crop(raster_obj, terra::ext(survey_dt))
    message("Using survey_dt for cropping.")
  } else if (!is.null(shp_dt)) {
    # Otherwise, use shp_dt as the shapefile for cropping
    cropped_raster <- terra::crop(raster_obj, terra::ext(shp_dt))
    message("Using shp_dt for cropping.")
  } else {
    stop("Both shp_dt and survey_dt are NULL. Cannot proceed with cropping.")
  }

  return(cropped_raster)
}


# Generic function to convert data.table/data.frame to sf
convert_to_sf <- function(survey_dt,
                          geometry_col = "geometry",
                          crs = 4326) {
  # Check if geometry column exists
  if (!geometry_col %in% colnames(survey_dt)) {
    stop(paste("The column",
               geometry_col,
               "is not present in the input data.table/data.frame"))
  }

  # Ensure the geometry column is properly formatted
  # Case 1: If the geometry column contains WKT (Well-Known Text) strings
  if (is.character(survey_dt[[geometry_col]])) {
    # Check if the strings are valid WKT (e.g., "POINT(x y)")
    tryCatch({
      survey_dt[[geometry_col]] <- st_as_sfc(survey_dt[[geometry_col]])
      message("Geometry column converted from WKT to sfc.")
    }, error = function(e) {
      stop("Error in converting WKT strings to sfc:", e$message)
    })
  }

  # Case 2: If the geometry column contains coordinates (e.g., longitude, latitude in a list)
  if (is.list(survey_dt[[geometry_col]])) {
    tryCatch({
      # Convert each row's coordinates into an sf point
      survey_dt[[geometry_col]] <- st_sfc(lapply(survey_dt[[geometry_col]],
                                                 function(x) st_point(c(x[1],
                                                                        x[2]))))
      message("Geometry column converted from list of coordinates to sfc.")
    }, error = function(e) {
      stop("Error in converting list of coordinates to sfc:", e$message)
    })
  }

  # Case 3: If the geometry column is already in sfc format, we do nothing
  if (inherits(survey_dt[[geometry_col]], "sfc")) {
    message("Geometry column is already in sfc format.")
  }

  # Convert the entire data.table/data.frame to an sf object
  tryCatch({
    sf_object <- st_as_sf(survey_dt, crs = crs)
    message("Data.table/data.frame successfully converted to sf object.")
    return(sf_object)
  }, error = function(e) {
    stop("Error in converting data.table/data.frame to sf object:", e$message)
  })
}

