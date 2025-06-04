
test_shp <- shp_dt[shp_dt$ADM1_EN == "Abia",]


result_chirps <- geolink_chirps(
  time_unit = "month",
  start_date = "2018-01-01",
  end_date = "2018-03-01",
  shp_dt = test_shp,
  grid_size = 1000,
  extract_fun = "mean",
  return_raster = FALSE)



result_ntl <- geolink_ntl(
  time_unit = "annual",
  start_date = "2020-01-01",
  end_date = "2020-12-31",
  shp_dt = test_shp,
  indicator = "average",
  extract_fun = "mean",
  return_raster = FALSE)


result_elev <- geolink_elevation(
  iso_code = "NGA",
  shp_dt = test_shp,
  extract_fun = "mean",
  return_raster = FALSE,
  grid_size = 1000)



result_buildings <- geolink_buildings(
  version = "v1.1",
  iso_code = "NGA",
  shp_dt = test_shp,
  grid_size = 1000,
  extract_fun = "mean",
  return_raster = FALSE)



result_wc<- geolink_worldclim(iso_code = "NGA",
                              var = "tmax",
                              res = 2.5,
                              shp_dt = test_shp,
                              return_raster = FALSE,
                              grid_size = 1000,
                              extract_fun = "mean")




crop_results <- geolink_cropland(shp_dt = test_shp,
                                 grid_size = 1000,
                                 extract_fun = "mean")



tc_results <- geolink_terraclimate( var='tmax',
                                    year = 2017,
                                    shp_dt = test_shp,
                                    grid_size = 1000,
                                    extract_fun = "mean")


poi_results = geolink_get_poi(osm_key = "amenity",
                              shp_dt = test_shp,
                              extract_fun = "mean")



elect_results = geolink_electaccess(shp_dt = test_shp,
                                    start_date = "2018-12-31",
                                    end_date = "2019-12-31",
                                    grid_size = 1000,
                                    extract_fun = "mean")


opencellid_results <- geolink_opencellid(cell_tower_file = "data/621.csv.gz",
                                         shp_dt = test_shp,
                                         extract_fun = "mean")

landcover_results <- geolink_landcover(
  start_date = "2019-01-01",
  end_date = "2019-12-31",
  shp_dt = test_shp,
  use_resampling = TRUE,
  target_resolution = 1000,
  extract_fun = "mean")

veg_results <- geolink_vegindex(shp_dt = test_shp,
                                start_date = "2019-01-01",
                                end_date = "2019-12-31",
                                indicator = "NDVI",
                                extract_fun = "mean",
                                buffer_size = 1000,
                                survey_crs = 4326,
                                grid_size = 1000)


pollution_results <- geolink_pollution(shp_dt = test_shp,
                                       start_date = "2018-06-01",
                                       end_date = "2018-09-28",
                                       indicator = "no2",
                                       grid_size = 1000,
                                       extract_fun = "mean")

