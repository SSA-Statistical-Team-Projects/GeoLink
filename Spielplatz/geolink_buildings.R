# pacman::p_load(ncdf4, rstac, reticulate, terra, raster, data.table)
#
#
# s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")
#
# it_obj <- s_obj %>%
#   stac_search(collections = "ms-buildings",
#               bbox = sf::st_bbox(shp_dt)) %>%
#   get_request() %>%
#   items_sign(sign_fn = sign_planetary_computer())
#
#
#
# download_dir <- "data"
#
# filename <- file.path(download_dir, "buildings.parquet")
# download.file("https://planetarycomputer.microsoft.com/api/stac/v1/search?collections=ms-buildings&bbox=2.66853356300004,4.27300739000003,14.6788162340001,13.894419133&token=next:ms-buildings:Benin_122202320_2023-04-25", destfile = filename, mode = "wb")
#
#
# options(timeout=600)
#
# read_parquet(filename)

### trying to quickly test geolink_buildings

#### read in the building rasters
raster_list <-
  list.files(pattern = "BWA_buildings_v2_0",
             path = "//esapov/esapov/BWA/GEO/BuildingFootprints",
             full.names = TRUE)

raster_list <- lapply(raster_list, terra::rast)

terra_list <- lapply(raster_list, raster::raster)

result_dt <-
  postdownload_processor(raster_objs = terra_list,
                         shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                         grid_size = 1000,
                         extract_fun = "mean",
                         survey_crs = 4326,
                         name_set = paste("bldvar", 1:length(terra_list)))









