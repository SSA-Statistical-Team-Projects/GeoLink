library(sf)
library(data.table)

# Function to read shapefile and extract cell tower data within its area
geolink_opencellid <- function(shp_dt,
                               csv_file = "data/cell_towers.csv.gz",
                               shapefile_crs = 4326) {

  # Ensure the CRS is defined
  if (is.na(st_crs(shp_dt))) {
    st_crs(shp_dt) <- 4326  # Assign a default CRS (WGS 84) if none is defined
  }

  # Transform the shapefile to CRS 4326 if it is not already in that CRS
  if (st_crs(shp_dt)$epsg != 4326) {
    shp_dt <- st_transform(shp_dt, crs = 4326)
  }

  # Read the cell tower data from the compressed CSV file
  cell_towers <- fread(csv_file)

  # Ensure the cell tower data has the correct column names
  # Assuming the columns are named "lon", "lat", etc.

  # Convert the data.table to an sf object
  cell_towers_sf <- st_as_sf(cell_towers, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

  # Filter the cell towers to include only those within the shapefile area
  cell_towers_in_area <- st_intersection(cell_towers_sf, shp_dt)

  return(cell_towers_in_area)
}

# Example usage:
# shp_dt <- st_read("path_to_your_shapefile.shp")
cell_towers_in_area <- geolink_opencellid(shp_dt[shp_dt$ADM1_EN == "Abia",])
