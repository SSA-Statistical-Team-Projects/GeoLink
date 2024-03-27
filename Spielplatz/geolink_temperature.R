pacman::p_load(ncdf4, rstac, reticulate, terra, raster, data.table)

start_date <- as.Date("2017-01-01")
end_date <- as.Date("2017-01-31")


s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

it_obj <- s_obj %>%
  stac_search(collections = "terraclimate",
              bbox = sf::st_bbox(shp_dt[shp_dt$ADM2_EN == "Ibi",]),
              datetime = paste(start_date, end_date, sep = "/")) %>%
  get_request() %>%
  items_sign(sign_fn = sign_planetary_computer())



url_list <- lapply(1:length(it_obj$features),
                   function(x){

                     url <- (it_obj$features[[x]]$assets$"lst-in"$href)

                     return(url)

                   })

url_list_coords <- lapply(1:length(it_obj$features),
                          function(x){

                            url <- (it_obj$features[[x]]$assets$"slstr-geodetic-in"$href)

                            return(url)

                          })

# temp_dir <- tempdir()
#
#
# for (i in 1:length(url_list)) {
#   url <- url_list[[i]]
#   filename <- file.path(temp_dir, paste0("file_", i, ".nc"))
#   download.file(url, destfile = filename, mode = "wb")
# }
#
#
#
# for (i in 1:length(url_list_coords)) {
#   url <- url_list_coords[[i]]
#   filename <- file.path(temp_dir, paste0("file_coords_", i, ".nc"))
#   download.file(url, destfile = filename, mode = "wb")
# }
#
# file_path <- file.path(temp_dir, "file_1.nc")
#
#
# nc <- nc_open(file_path)


download_dir <- "data"

url <- url_list[[1]]
filename <- file.path(download_dir, "terraclimate.nc")
download.file("https://planetarycomputer.microsoft.com/api/stac/v1/search?collections=terraclimate&datetime=2017-01-01%2F2017-01-31&bbox=9.32342362600002%2C7.98550786300007%2C10.4124871770001%2C8.80440830600003", destfile = filename, mode = "wb")


options(timeout=600)


url <- url_list_coords[[1]]
filename <- file.path(download_dir, "file_1_coords_ibi.nc")
download.file(url, destfile = filename, mode = "wb", timeout = timeout_duration)





nc = "data/file_1_ibi.nc"
nc_coords = "data/file_1_coords_ibi.nc"


rast_nc <- rast(nc)
d <- as.data.frame(rast_nc)
LST_column <- d$LST

rast_nc_coords <- rast(nc_coords)
d_coords <- as.data.frame(rast_nc_coords)

long <- d_coords$longitude_in
lat <- d_coords$latitude_in

data <- data.frame(long = long, lat = lat, LST = LST_column)

dt <- data.table(as.data.frame(data, xy = TRUE))

raster <- rast(dt, type = "xyz", crs = "+proj=longlat +datum=WGS84")

ter <- nc_open("data/terraclimate.nc")





