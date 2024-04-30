library(reticulate)
use_python("C:/Users/Diana Jaganjac/anaconda3")

geolink_population <- function(time_unit = "annual",
                               start_year,
                               end_year,
                               iso_code,
                               const_UNadj_2020,
                               bespoke,
                               version,
                               shp_dt,
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

  years <- seq(start_year, end_year)

  result_list <- paste0("ppp_", years)

  dl <- import("wpgpDownload.utils.convenience_functions", convert = TRUE)$download_country_covariates

  data <- dl(iso_code, temp_dir, result_list)

  if (const_UNadj_2020 == "Y") {
    url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/BSGM/",
                  iso_code)


    tryCatch({
      download.file(url, temp_dir, basename(url))
    }, error = function(e) {

      url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/maxar_v1/", iso_code)
      download.file(url, temp_dir, basename(url))
    })
  }

  if (bespoke == "Y") {
    url <- paste0("https://data.worldpop.org/repo/wopr/", iso_code,
                  "/population/", version, "_population_",
                  gsub("\\.", "_", version), "_mastergrid.tif")
    download.file(url, temp_dir, basename(url))

  }

  tif_files <- list.files(temp_dir, pattern = "\\.tif$", full.names = TRUE)

  raster_objs <- lapply(tif_files, terra::rast)

  raster_list = lapply(raster_objs, raster)

  name_count <- lubridate::year(start_year) - lubridate::year(end_year) + 1

  name_set <- paste0("population_", "annual_", 1:name_count)

  print("Population Raster Downloaded")

  dt <- postdownload_processor(shp_dt = shp_dt,
                               raster_objs = raster_objs,
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



df <- geolink_population(time_unit,
                         start_year=2018,
                         end_year = 2019,
                         iso_code = "NGA",
                         const_UNadj_2020 = Y,
                         shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                         grid_size = 1000,
                         survey_dt = st_as_sf(hhgeo_dt[hhgeo_dt$ADM1_EN == "Abia",],
                                              extract_fun = "mean"))

