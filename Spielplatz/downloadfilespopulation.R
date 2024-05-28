# Load necessary libraries
library(rvest)
library(httr)

# Define the URL of the directory
url <- "https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/maxar_v1/NGA/"

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

# Get the list of file URLs
file_urls <- get_file_list(url)

# Define the directory to save the files
dest_dir <- tempdir()

# Download each file
for (file_url in file_urls) {
  # Define the destination file path
  destfile <- file.path(dest_dir, basename(file_url))

  # Download the file
  download.file(file_url, destfile, method = "auto")
}

# List the downloaded files
downloaded_files <- list.files(dest_dir, pattern = "\\.tif$", full.names = TRUE)
print(downloaded_files)

