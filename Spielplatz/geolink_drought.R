library(httr)

# URL of the raster file
url <- "http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_PDSI_2023.nc"

# Destination file path
destination <- file.path(tempdir(), "pdsi_2023.tif")

timeout_seconds <- 7200

# Make the HTTP GET request with increased timeout
response <- GET(url, timeout(timeout_seconds))

# Check if the request is successful
if (http_status(response)$status_code == 200) {
  # Write the content to the file
  writeBin(content(response, "raw"), destination)
  print("File downloaded successfully.")
} else {
  print("Error downloading the file.")
}
