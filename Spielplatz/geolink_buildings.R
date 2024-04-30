pacman::p_load(ncdf4, rstac, reticulate, terra, raster, data.table)


s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

it_obj <- s_obj %>%
  stac_search(collections = "ms-buildings",
              bbox = sf::st_bbox(shp_dt)) %>%
  get_request() %>%
  items_sign(sign_fn = sign_planetary_computer())



download_dir <- "data"

filename <- file.path(download_dir, "buildings.parquet")
download.file("https://planetarycomputer.microsoft.com/api/stac/v1/search?collections=ms-buildings&bbox=2.66853356300004,4.27300739000003,14.6788162340001,13.894419133&token=next:ms-buildings:Benin_122202320_2023-04-25", destfile = filename, mode = "wb")


options(timeout=600)

read_parquet(filename)
