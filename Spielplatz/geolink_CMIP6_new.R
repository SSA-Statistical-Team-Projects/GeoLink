pacman::p_load(rstac, reticulate, terra, raster, osmdata, sp, sf, geodata, httr, ncdf4, rgdal, exactextractr, parallel)

start_date = "1960-01-01"
end_date = "2020-01-01"

shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",]


start_date <- as.Date(start_date)
end_date <- as.Date(end_date)

s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

it_obj <- s_obj %>%
  stac_search(
    collections = "nasa-nex-gddp-cmip6",
    bbox = sf::st_bbox(shp_dt),
    datetime = paste(start_date, end_date, sep = "/"))%>%
  get_request() %>%
  items_sign(sign_fn = sign_planetary_computer())

urls <- lapply(1:length(it_obj$features), function(x) {
  list((
    pr = paste0("/vsicurl/", it_obj$features[[x]]$assets$pr$href)),
    (tas = paste0("/vsicurl/", it_obj$features[[x]]$assets$tas$href)),
    (hurs = paste0("/vsicurl/", it_obj$features[[x]]$assets$hurs$href)),
    (huss = paste0("/vsicurl/", it_obj$features[[x]]$assets$huss$href)),
    (rlds = paste0("/vsicurl/", it_obj$features[[x]]$assets$rlds$href)),
    (rsds = paste0("/vsicurl/", it_obj$features[[x]]$assets$rsds$href)),
    (tasmax = paste0("/vsicurl/", it_obj$features[[x]]$assets$tasmax$href)),
    (tasmin = paste0("/vsicurl/", it_obj$features[[x]]$assets$tasmin$href)),
    (sfcWind = paste0("/vsicurl/", it_obj$features[[x]]$assets$sfcWind$href))
  )
})



# Function to convert netCDF to raster
netcdf_to_raster <- function(url) {
  nc <- nc_open(url)
  vars <- names(nc$var)
  rasters <- lapply(vars, function(var) {
    raster::raster(url, varname = var)
  })
  nc_close(nc)
  return(rasters)
}

# Download and convert netCDF files to rasters
raster_objs <- lapply(unlist(urls), netcdf_to_raster)



