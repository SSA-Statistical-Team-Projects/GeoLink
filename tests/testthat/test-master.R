## Test for errors messages ##
# Test data_type error
test_that("Test error from data_type:", {
  expect_error(run_geolink("chirps",
                           time_unit = "month",
                           start_date = "2020-01-01",
                           end_date = "2020-02-01",
                           shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                           extract_fun = "mean"),
               regexp = "Invalid data_type")
})


# Test dates
test_that("Test error for date arguments:", {
  expect_error(run_geolink("rainfall",
                           time_unit = "month",
                           start_date = "2001/20/24",
                           end_date = "2020-02-01",
                           shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                           extract_fun = "mean"),
               regexp = "a date argument you inputted is in the wrong format, it should be")

  expect_error(run_geolink("rainfall",
                           time_unit = "month",
                           start_date = "2020-01-24",
                           end_date = "2020/02/01",
                           shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                           extract_fun = "mean"),
               regexp = "a date argument you inputted is in the wrong format, it should be")
})

# Test shp_dt and survey_dt as sf objects
test_that("Test error for shp_dt and survey_dt:", {
  expect_error(run_geolink("population",
                           start_year = 2018,
                           end_year = 2019,
                           iso_code = "NGA",
                           UN_adjst = "N",
                           constrained = "N",
                           shp_dt = data.frame(shp_dt),
                           extract_fun = "mean"),
               regexp = "shp_dt must be an sf object")

  expect_error(run_geolink("nightlight",
                           time_unit = "annual",
                           indicator = "avg_rade9h",
                           start_date = "2020-01-01",
                           end_date = "2020-12-01",
                           survey_dt = as.list(hhgeo_dt[1:10,]), ## Should this check for geometry at least?
                           buffer_size = 1000,
                           extract_fun = "mean"),
               regexp = "survey_dt must be an sf object")
})

# Test iso-code provided
test_that("Test error for iso_code:", {
  expect_error(run_geolink("population",
                           start_year = 2018,
                           end_year = 2019,
                           iso_code = "NG",
                           UN_adjst = "N",
                           constrained = "N",
                           shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                           extract_fun = "mean"),
               regexp = "iso_code must be a 3-character ISO country code")

})

# Test missing required argument:
test_that("Test error for missing required argument:", {
  expect_error(run_geolink("elevation",
                           shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                           extract_fun = "mean"),
               regexp = "Missing required parameters for elevation : iso_code")

})


## Test of values equality with single functions ##
# Test for elevation
test_that("Test elevation function:", {
  suppressWarnings({test_dt <- run_geolink("elevation",
                         iso_code = "NGA",
                         shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                         extract_fun = "mean")})

  expect_contains(colnames(test_dt), "NGA_elv_msk")

  expect_true(all(test_dt$NGA_elv_msk[!is.na(test_dt$NGA_elv_msk)] >= 0 &
                    test_dt$NGA_elv_msk[!is.na(test_dt$NGA_elv_msk)] <= 2000),
              info = "Values of NGA_elv_msk should be between 0 and 2000")

  suppressWarnings({test_dt_func <- geolink_elevation(iso_code = "NGA",
                                    shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                    extract_fun = "mean")})

  expect_setequal(test_dt_func$NGA_elv_msk, test_dt$NGA_elv_msk)

  })

# Test for chirps
test_that("Test chirps function:", {
  suppressWarnings({test_dt <- run_geolink("rainfall",
                         time_unit = "month",
                         start_date = "2020-01-01",
                         end_date = "2020-02-01",
                         grid_size = 1000,
                         shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                         extract_fun = "mean")

                    suggest_dt <- crsuggest::suggest_crs(shp_dt, units = "m")

  })

  expect_contains(colnames(test_dt), "rainfall_month1")

  expect_true(all(test_dt$rainfall_month1 >= 0 &
                    test_dt$rainfall_month1 <= 1444.34),
              info = "Values of rainfall_month1 should be between 0 and 1444.34")

  suppressWarnings({test_dt_func <- geolink_chirps(time_unit = "month",
                                    start_date = "2020-01-01",
                                    end_date = "2020-02-01",
                                    grid_size = 1000,
                                    shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                    extract_fun = "mean")})

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))

  expect_setequal(test_dt_func$rainfall_month1, test_dt$rainfall_month1)

  })

# Test for terraclimate
test_that("Test terraclimate function:", {
  suppressWarnings({test_dt <- run_geolink("terraclimate",
                         var = 'tmax',
                         year = 2017,
                         grid_size = 1000,
                         shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                         extract_fun = "mean")

                    suggest_dt <- crsuggest::suggest_crs(shp_dt, units = "m")

  })

  expect_contains(colnames(test_dt), "tmax_Jan")

  expect_true(all(test_dt$tmax_Jan[!is.na(test_dt$tmax_Jan)] >= -40.80 &
                    test_dt$tmax_Jan[!is.na(test_dt$tmax_Jan)] <= 41.50),
              info = "Values of tmax_Jan should be between -40.80 and 41.50")

  suppressWarnings({test_dt_func <- geolink_terraclimate(var = 'tmax',
                                    year = 2017,
                                    grid_size = 1000,
                                    shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                    extract_fun = "mean")})

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))

  expect_setequal(test_dt_func$tmax_Jan, test_dt$tmax_Jan)

})


# Test for population
test_that("Test population function:", {
  suppressWarnings({test_dt <- run_geolink("population",
                         start_year = 2018,
                         end_year = 2019,
                         iso_code = "NGA",
                         UN_adjst = "N",
                         constrained = "N",
                         shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                         extract_fun = "mean")})

  expect_contains(colnames(test_dt), "population_2018")

  expect_true(all(test_dt$population_2018[!is.na(test_dt$population_2018)] >= 0 &
                    test_dt$population_2018[!is.na(test_dt$population_2018)] <= 2000),
              info = "Values of population_2018 should be between 0 and 2000")

  suppressWarnings({test_dt_func <- geolink_population(start_year = 2018,
                                    end_year = 2019,
                                    iso_code = "NGA",
                                    UN_adjst = "N",
                                    constrained = "N",
                                    shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                    extract_fun = "mean")})

  expect_setequal(test_dt_func$population_2018, test_dt$population_2018)

})

# Test for nightlight
test_that("Test nightlight function:", {
  suppressWarnings({test_dt <- run_geolink("nightlight",
                         time_unit = "annual",
                         indicator = "median_masked",
                         start_date = "2020-01-01",
                         end_date = "2020-12-01",
                         grid_size = 1000,
                         shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                         extract_fun = "mean")

                    suggest_dt <- crsuggest::suggest_crs(shp_dt, units = "m")

  })

  expect_contains(colnames(test_dt), "ntl_annual1median_masked")

  expect_true(all(test_dt$ntt_annual1median_masked >= 0 &
                    test_dt$nlt_annual1median_masked <= 200),
              info = "Values of nlt_annual1median_masked should be between 0 and 2000")

  suppressWarnings({test_dt_func <- geolink_ntl(time_unit = "annual",
                                    indicator = "median_masked",
                                    start_date = "2020-01-01",
                                    end_date = "2020-12-01",
                                    grid_size = 1000,
                                    shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                    extract_fun = "mean")})

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))

  expect_setequal(test_dt_func$ntl_annual1median_masked, test_dt$ntl_annual1median_masked)
})

# Test for buildings
test_that("Test buildings function:", {
  suppressWarnings({test_dt <- run_geolink("buildings",
                                           iso_code = "NGA",
                                           version = "v1.1",
                                           indicator = "ALL",
                                           shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                           grid_size = 1000,
                                           extract_fun = "mean")

                    suggest_dt <- crsuggest::suggest_crs(shp_dt, units = "m")

  })

  expect_contains(colnames(test_dt), "count")

  cv_length <- na.omit(test_dt$cv_length)
  expect_true(all(cv_length >= 0.0 & cv_length <= 2.2),
              info = "Values of cv_length should be between 0.0 and 2.2")

  suppressWarnings({test_dt_func <- geolink_buildings(
                                    shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                    iso_code = "NGA",
                                    version = "v1.1",
                                    grid_size = 1000,
                                    extract_fun = "mean")})

  expect_setequal(test_dt_func$count, test_dt$count)

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))

})


# Test CMIP6
test_that("Test CMIP6 function:", {
  suppressWarnings({test_dt <- run_geolink("cmip6",
                                           start_date = "2019-01-01",
                                           end_date = "2019-12-31",
                                           scenario = "ssp245",
                                           desired_models = "UKESM1-0-LL",
                                           shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                           grid_size = 1000,
                                           extract_fun = "mean")

  suggest_dt <- crsuggest::suggest_crs(shp_dt, units = "m")

  })

  expect_contains(colnames(test_dt), c("pr_2019",  "tas_2019", "hurs_2019",
                                        "huss_2019", "rlds_2019",
                                        "rsds_2019", "tasmax_2019",
                                        "tasmin_2019", "sfcWind_2019"))

  expect_true(all(test_dt$pr_2019 >= 0 & test_dt$pr_2019 <= 0.000249315 ),
              info = "Values of pr should be between 0 and 0.000249315")

  expect_true(all(test_dt$rsds_2019 >= 93.35552    & test_dt$rsds_2019 <= 304.32153),
              info = "Values of rsds_2019 should be between 93.35552    and 304.32153")

  expect_true(all(test_dt$tasmax_2019 >= 250.3406    & test_dt$tasmax_2019 <= 313.9287),
              info = "Values of tasmax_2019 should be between 250.3406    and 313.9287 ")

  suppressWarnings({test_dt_func <- geolink_CMIP6(start_date = "2019-01-01",
                                                  end_date = "2019-12-31",
                                                  scenario = "ssp245",
                                                  desired_models = "UKESM1-0-LL",
                                    shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                    grid_size = 1000,
                                    extract_fun = "mean")})

  expect_setequal(test_dt_func$pr_2019, test_dt$pr_2019)
  expect_setequal(test_dt_func$tas_2019, test_dt$tas_2019)
  expect_setequal(test_dt_func$hurs_2019, test_dt$hurs_2019)
  expect_setequal(test_dt_func$huss_2019, test_dt$huss_2019)
  expect_setequal(test_dt_func$rlds_2019, test_dt$rlds_2019)
  expect_setequal(test_dt_func$rsds_2019, test_dt$rsds_2019)
  expect_setequal(test_dt_func$tasmax_2019, test_dt$tasmax_2019)
  expect_setequal(test_dt_func$tasmin_2019, test_dt$tasmin_2019)
  expect_setequal(test_dt_func$sfcWind_2019, test_dt$sfcWind_2019)

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))
})


# Test cropland
test_that("Test cropland function:", {
  suppressWarnings({test_dt <- run_geolink("cropland",
                                           shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                           grid_size = 1000,
                                           extract_fun = "mean")

                    suggest_dt <- crsuggest::suggest_crs(shp_dt, units = "m")

  })

  expect_contains(colnames(test_dt), c("cropland"))

  expect_true(all(test_dt$cropland >= 0.0 & test_dt$cropland <= 1.0),
              info = "Values of cropland should be between 0.0 and 1.0")

  suppressWarnings({test_dt_func <- geolink_cropland(
                                    shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                    grid_size = 1000,
                                    extract_fun = "mean")})

  expect_setequal(test_dt_func$cropland, test_dt$cropland)

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))
})


# Test worldclim
test_that("Test worldclim function:", {
  suppressWarnings({test_dt <- run_geolink("worldclim",
                                           iso_code ="NGA",
                                           var='tavg',
                                           res=2.5,
                                           grid_size = 1000,
                                           shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                           extract_fun = "mean")

                    suggest_dt <- crsuggest::suggest_crs(shp_dt, units = "m")

  })

  expect_contains(colnames(test_dt), "NGA_WC_tavg_Aug")

  expect_true(all(test_dt$tavg[!is.na(test_dt$NGA_WC_tavg_Nov)] >= -50.0 &
                    test_dt$tavg[!is.na(test_dt$NGA_WC_tavg_Nov)] <= 50.0),
              info = "Values of tavg should be between -50.0 and 50.0")

  suppressWarnings({test_dt_func <- geolink_worldclim(iso_code ="NGA",
                                                      var='tavg',
                                                      res=2.5,
                                    grid_size = 1000,
                                    shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                    extract_fun = "mean")})

  expect_setequal(test_dt_func$NGA_WC_tavg_May, test_dt$NGA_WC_tavg_May)

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))

})

# Test opencellid
test_that("Test opencellid function: ",
          {
            suppressWarnings({ test_dt <- run_geolink("opencellid",
                                                      cell_tower_file = paste0(test_path(), "/testdata/opencellidNGA.csv.gz"),
                                                      shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                      grid_size = 1000)

            suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                                 units = "m")
            })

            #Write testing expressions below:
            #01 - expect the colnames will be created correctly
            expect_contains(colnames(test_dt), c("cell_towers"))

            #02 - expect the number of rows in the test_dt is equal to the number of rows in the shp_dt
            expect_equal(nrow(test_dt), nrow(shp_dt[shp_dt$ADM1_EN == "Abia",]))

            #03 - expect the number of cell_towers in the test_dt to be positive and less than 2828
            expect_true(all(test_dt$cell_towers >= 0) & all(test_dt$cell_towers < 2828),
                        info = "Number of cell_towers should be positive and less than 2828")

            #04 - expect the test_dt_func to be equal to the test_dt
            suppressWarnings({ test_dt_func <- geolink_opencellid(cell_tower_file = paste0(test_path(), "/testdata/opencellidNGA.csv.gz"),
                                                                  shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                                  grid_size = 1000)
            })

            expect_setequal(test_dt_func$cell_towers, test_dt$cell_towers)
          })

# Test electaccess
test_that("Test electaccess function: ",
          {
            suppressWarnings({ test_dt <- run_geolink("electaccess",
                                                      shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                      start_date = "2019-01-01",
                                                      end_date = "2019-12-31",
                                                      grid_size = 1000,
                                                      extract_fun = "mean")

            suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                                 units = "m")
            })

            suppressWarnings({
            test_dt_func <- geolink_electaccess(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                        start_date = "2019-01-01",
                                        end_date = "2019-12-31",
                                        grid_size = 1000,
                                        extract_fun = "mean")
            })

            expect_setequal(test_dt_func$night_proportion_2019, test_dt$night_proportion_2019)
            expect_setequal(test_dt_func$lightscore_2019, test_dt$lightscore_2019)
            expect_setequal(test_dt_func$light_composite_2019, test_dt$light_composite_2019)
            expect_setequal(test_dt_func$estimated_brightness_2019, test_dt$estimated_brightness_2019)

            #Write testing expressions below:
            #01 - expect the colnames will be created correctly
            expect_contains(colnames(test_dt), c("night_proportion_2019", "lightscore_2019", "light_composite_2019", "estimated_brightness_2019"))

            #02 - Test that the mean column values is between 0 and 1 based on the raster values

            expect_true(all(test_dt$night_proportion_2019[!is.na(test_dt$night_proportion_2019)] >= 0 &
                              test_dt$night_proportion_2019[!is.na(test_dt$night_proportion_2019)] <= 1),
                        info = "Values of night proportion should be between 0 and 1")
          })

#  Test vegindex
test_that("Test vegindex function: ",
          {
            suppressWarnings({ test_dt <- run_geolink("vegindex",
                                                      start_date = "2019-01-01",
                                                      end_date = "2019-12-31",
                                                      shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                      grid_size = 1000,
                                                      extract_fun = "mean")

            suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                                 units = "m")
            })

            #Write testing expressions below:
            #01 - expect the colnames will be created correctly
            expect_contains(colnames(test_dt), c("ndvi_y2019_m04"))

            #02 - Test that the mean column values is between -1 and 1 based on the raster values

            expect_true(all(test_dt$ndvi_y2019_m04[!is.na(test_dt$ndvi_y2019_m04)] >= -1 &
                              test_dt$ndvi_y2019_m04[!is.na(test_dt$ndvi_y2019_m04)] <= 1),
                        info = "Values of ndvi should be between -1 and 1")

            #03 - Test that the number of unique polygons in the test_dt is equal to the number of unique polygons in the shapefile
            expect_equal(length(unique(test_dt$poly_id)),
                         suppressWarnings({
                           length(gengrid2(shp_dt =
                                             st_transform(shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                          crs = as.numeric(suggest_dt$crs_code[1])),
                                         grid_size = 1000)$poly_id)}))

            #04 - expect the test_dt_func to be equal to the test_dt
            suppressWarnings({
              test_dt_func <- geolink_vegindex(start_date = "2019-01-01",
                                               end_date = "2019-12-31",
                                               shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                               grid_size = 1000,
                                               extract_fun = "mean")
            })
            expect_setequal(test_dt_func$ndvi_y2019_m04, test_dt$ndvi_y2019_m04)
          })
