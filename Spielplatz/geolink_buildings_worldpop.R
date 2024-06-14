pacman::p_load(rstac, terra, raster, osmdata, sp, sf, httr, geodata)


geolink_buildings <- function(version,
                              iso_code,
                              shp_dt = NULL,
                              shp_fn = NULL,
                              grid_size = 1000,
                              survey_dt,
                              survey_fn = NULL,
                              survey_lat = NULL,
                              survey_lon = NULL,
                              buffer_size = NULL,
                              extract_fun = "mean",
                              survey_crs = 4326){

  temp_dir <- tempdir()

    if (version == "v1.1") {
      url <- paste0("https://data.worldpop.org/repo/wopr/_MULT/buildings/v1.1/", iso_code, "_buildings_v1_1.zip")
      tryCatch({
        # Download the ZIP file
        response <- GET(url, write_disk(file.path(tempdir(), basename(url)), overwrite = TRUE))
        if (http_type(response) == "application/zip") {
          message("File downloaded successfully.")

          # Unzip the downloaded file
          unzip(file.path(tempdir(), basename(url)), exdir = tempdir())
          message("File unzipped successfully.")
        } else {
          warning("Downloaded file may not be a ZIP file.")
        }
      }, error = function(e) {
        print(e)
      })
    }

    if (version == "v2.0") {
      url <- paste0("https://data.worldpop.org/repo/wopr/_MULT/buildings/v2.0/", iso_code, "_buildings_v2_0.zip")
      tryCatch({
        # Download the ZIP file
        response <- GET(url, write_disk(file.path(tempdir(), basename(url)), overwrite = TRUE))
        if (http_type(response) == "application/zip") {
          message("File downloaded successfully.")

          # Unzip the downloaded file
          unzip(file.path(tempdir(), basename(url)), exdir = tempdir())
          message("File unzipped successfully.")
        } else {
          warning("Downloaded file may not be a ZIP file.")
        }
      }, error = function(e) {
        print(e)
      })
    }



  tif_files <- list.files(path = temp_dir, pattern = "\\.tif$", full.names = TRUE)

  name_set <- c()

  for (file in tif_files) {
    base_name <- basename(file)

    extracted_string <- sub(".*1_([^\\.]+)\\.tif$", "\\1", base_name)

    name_set <- c(name_set, extracted_string)
  }

  raster_objs <- lapply(tif_files, terra::rast)

  raster_list <- lapply(raster_objs, raster)

  epsg_4326 <- "+init=EPSG:4326"

  for (i in seq_along(raster_list)) {
    projection(raster_list[[i]]) <- epsg_4326
    if (is.null(projection(raster_list[[i]]))) {
      print(paste("Projection failed for raster", i))
    } else {
      print(paste("Raster", i, "projected successfully."))
    }
  }


  print("Building Raster Downloaded")


  dt <- postdownload_processor(shp_dt = shp_dt,
                               raster_objs = raster_list,
                               shp_fn = shp_fn,
                               grid_size = grid_size,
                               survey_dt = survey_dt,
                               survey_fn = survey_fn,
                               survey_lat = survey_lat,
                               survey_lon = survey_lon,
                               extract_fun = extract_fun,
                               buffer_size = buffer_size,
                               survey_crs = survey_crs,
                               name_set = name_set)

  print("Process Complete!!!")

  return(dt)
}

test_dt <- geolink_buildings(version = "v1.1",
                             iso_code = "NGA",
                             shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",])



