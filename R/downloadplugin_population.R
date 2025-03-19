
check_url_status <- function(url) {
  tryCatch({
    response <- httr::HEAD(
      url,
      httr::config(ssl_verifypeer = FALSE),  # Disable SSL verification
      httr::timeout(10)                      # Set timeout to 10 seconds
    )

    status <- httr::status_code(response)

    if (status == 200) {
      return(TRUE)
    } else {
      # Don't immediately fail - try a regular GET request
      message(paste("URL check with HEAD returned status code:", status, "for URL:", url))

      # Try a GET request as fallback for validation
      response_get <- tryCatch({
        httr::GET(
          url,
          httr::config(ssl_verifypeer = FALSE),
          httr::timeout(10)
        )

        get_status <- httr::status_code(response_get)
        if (get_status == 200) {
          return(TRUE)
        } else {
          message(paste("URL check with GET returned status code:", get_status, "for URL:", url))
          return(FALSE)
        }
      }, error = function(e) {
        message(paste("URL check with GET failed:", e$message, "for URL:", url))
        return(FALSE)
      })

      return(response_get)
    }
  }, error = function(e) {
    # Even if HEAD request fails, try to be optimistic - URL might still work with direct download
    message(paste("URL status check failed:", e$message, "for URL:", url))
    return(TRUE)  # Return TRUE anyway to allow download attempt
  })
}

httr_download <- function(url, dest_file) {
  tryCatch({
    message(paste("Downloading:", url))

    # Try to get file size for progress indication
    head_response <- tryCatch({
      httr::HEAD(url, httr::config(ssl_verifypeer = FALSE))
    }, error = function(e) {
      return(NULL)
    })

    # Perform the download without progress tracking to avoid errors
    message("Downloading file...")
    response <- httr::GET(
      url,
      httr::config(ssl_verifypeer = FALSE),
      httr::write_disk(dest_file, overwrite = TRUE)
    )

    # Check if download was successful
    if (httr::status_code(response) == 200) {
      message(paste("Successfully downloaded:", basename(dest_file)))
      return(TRUE)
    } else {
      message(paste("Download failed with status code:", httr::status_code(response)))
      return(FALSE)
    }
  }, error = function(e) {
    message(paste("Error downloading file:", e$message))
    return(FALSE)
  })
}

download_files_worldpop <- function(file_urls, file_location) {
  # Set up a progress tracker for multiple files
  total_files <- length(file_urls)
  if (total_files > 1) {
    message(paste("Processing", total_files, "files"))
  }

  for (i in seq_along(file_urls)) {
    url <- file_urls[i]
    file_name <- basename(url)
    dest_file <- file.path(file_location, file_name)

    if (total_files > 1) {
      message(paste("\nFile", i, "of", total_files, ":", file_name))
    }

    if (!file.exists(dest_file)) {
      download_status <- httr_download(url, dest_file)

      if (!download_status) {
        warning(paste("Failed to download file from URL:", url))
      }
    } else {
      message(paste("File already exists:", dest_file))
    }
  }

  if (total_files > 1) {
    message("\nAll downloads completed")
  }
}

try_download <- function(url, UN_adjst = NULL) {
  tryCatch({
    # Check URL availability - but even if it fails, we'll try direct URL construction
    url_status <- check_url_status(url)

    # Even if status check fails, we'll still try to construct direct URLs as fallback
    if (url_status) {
      # Try to get file listing using httr
      success <- FALSE

      tryCatch({
        # Use httr::GET to fetch directory listing
        response <- httr::GET(
          url,
          httr::config(ssl_verifypeer = FALSE),
          httr::timeout(20)
        )

        if (httr::status_code(response) == 200) {
          content <- httr::content(response, "text", encoding = "UTF-8")

          # Get all TIF files using regex pattern matching
          # Make sure we're only getting valid filenames
          all_tifs <- regmatches(content, gregexpr(paste0(url, "[a-zA-Z0-9_\\.-]+\\.tif"), content))[[1]]

          # If that fails, try a more general pattern but ensure we only get valid filenames
          if (length(all_tifs) == 0) {
            # Try to find any .tif file references - restrict to valid characters
            all_tifs <- regmatches(content, gregexpr("[a-zA-Z0-9_\\./:-]+\\.tif", content))[[1]]

            # Add URL if needed (for relative paths)
            all_tifs <- ifelse(grepl("^http", all_tifs), all_tifs, paste0(url, all_tifs))
          }

          # Apply UN_adjst filter if specified
          if (!is.null(UN_adjst)) {
            if (UN_adjst == "Y") {
              file_urls <- all_tifs[grepl("UNadj", all_tifs)]
            } else if (UN_adjst == "N") {
              file_urls <- all_tifs[!grepl("UNadj", all_tifs)]
            }
          } else {
            file_urls <- all_tifs
          }

          if (length(file_urls) > 0) {
            success <- TRUE
            return(file_urls)
          }
        }
      }, error = function(e) {
        message(paste("HTTR method failed:", e$message))
      })
    }

    # If the httr method fails or URL is not available, try direct URL construction
    message("Using direct URL construction as fallback...")

    # Extract year from URL if possible
    year_match <- regexpr("\\d{4}", url)
    year <- if (year_match > 0) substr(url, year_match, year_match + 3) else NULL

    if (!is.null(year) && !is.null(UN_adjst)) {
      # Lowercase country code
      iso_lower <- tolower(gsub(".*/([A-Za-z]+)/?$", "\\1", url))

      if (nchar(iso_lower) > 3) {
        # If extraction failed, use the last 3 characters
        iso_parts <- strsplit(url, "/")[[1]]
        for (part in rev(iso_parts)) {
          if (nchar(part) == 3) {
            iso_lower <- tolower(part)
            break
          }
        }
      }

      # Construct a single direct URL based on the pattern
      if (UN_adjst == "Y") {
        constructed_url <- paste0(url, iso_lower, "_ppp_", year, "_UNadj.tif")
      } else {
        constructed_url <- paste0(url, iso_lower, "_ppp_", year, ".tif")
      }

      # Return the constructed URL
      return(constructed_url)
    }

    # If we couldn't construct a URL either, return NULL
    return(NULL)
  }, error = function(e) {
    message(paste("Error in try_download:", e$message))
    return(NULL)
  })
}
