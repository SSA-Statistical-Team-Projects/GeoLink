
test_shp <- shp_dt[shp_dt$ADM1_PCODE == "NG001",]

df <- geolink_population(
  start_year = 2018,
  end_year = 2018,
  iso_code = "NGA",
  UN_adjst = "N",
  constrained = "N",
  shp_dt = test_shp,
  grid_size = 1000,
  extract_fun = "mean",
  return_raster = TRUE
)

weight_raster <- df





result_chirps <- geolink_chirps(
  time_unit = "month",
  start_date = "2018-01-01",
  end_date = "2018-03-01",
  shp_dt = test_shp,
  grid_size = 1000,
  extract_fun = "weighted_mean",
  return_raster = FALSE,
  weight_raster = weight_raster
)

result_ntl <- geolink_ntl(
  time_unit = "annual",
  start_date = "2020-01-01",
  end_date = "2020-12-31",
  shp_dt = test_shp,
  indicator = "average",
  extract_fun = "weighted_mean",
  return_raster = FALSE,
  weight_raster = weight_raster
)


result_elev <- geolink_elevation(
  iso_code = "NGA",
  shp_dt = test_shp,
  grid_size = 10000,
  extract_fun = "weighted_mean",
  return_raster = FALSE,
  weight_raster = weight_raster
)



result_buildings <- geolink_buildings(
                                      version = "v1.1",
                                      iso_code = "NGA",
                                      shp_dt = test_shp,
                                      grid_size = 10000,
                                      extract_fun = "weighted_mean",
                                      return_raster = FALSE,
                                      weight_raster = weight_raster
                                    )

result_wc<- geolink_worldclim(iso_code = "NGA",
                                    var = "tmax",
                                    res = 2.5,
                                    shp_dt = test_shp,
                                    return_raster = FALSE,
                                    weight_raster = weight_raster,
                                    extract_fun = "weighted_mean")


crop_results <- geolink_cropland(shp_dt = test_shp,
                                 grid_size = 1000,
                                 extract_fun = "weighted_mean",
                                 weight_raster = weight_raster)



tc_results <- geolink_terraclimate( var='tmax',
                                year = 2017,
                                shp_dt = test_shp,
                                weight_raster = weight_raster,
                                grid_size = 1000,
                                extract_fun = "weighted_mean")


poi_results = geolink_get_poi(osm_key = "amenity",
                                shp_dt = test_shp,
                                weight_raster = weight_raster,
                                extract_fun = "weighted_mean")



elect_results = geolink_electaccess(shp_dt = test_shp,
                               start_date = "2018-12-31",
                               end_date = "2019-12-31",
                               weight_raster = weight_raster,
                               extract_fun = "weighted_mean")


opencellid_results <- geolink_opencellid(cell_tower_file = "data/621.csv.gz",
                              shp_dt = test_shp,
                              weight_raster = weight_raster,
                              extract_fun = "weighted_mean")

landcover_results <- geolink_landcover(
                              start_date = "2019-01-01",
                              end_date = "2019-12-31",
                              shp_dt = test_shp,
                              use_resampling = TRUE,
                              target_resolution = 1000,
                              weight_raster = weight_raster,
                              extract_fun = "weighted_mean")

veg_results <- geolink_vegindex(shp_dt = test_shp,
                              start_date = "2019-01-01",
                              end_date = "2019-12-31",
                              indicator = "NDVI",
                              extract_fun = "weighted_mean",
                              buffer_size = 1000,
                              survey_crs = 4326,
                              weight_raster = weight_raster)


pollution_results <- geolink_pollution(shp_dt = test_shp,
                              start_date = "2018-06-01",
                              end_date = "2018-09-28",
                              indicator = "no2",
                              grid_size = 1000,
                              extract_fun = "weighted_mean",
                              weight_raster = weight_raster)

