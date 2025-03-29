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
               regexp = "start_date must be in a valid date format")

  expect_error(run_geolink("rainfall",
                           time_unit = "month",
                           start_date = "2020-01-24",
                           end_date = "2020/02/01",
                           shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                           extract_fun = "mean"),
               regexp = "end_date must be in a valid date format")
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
