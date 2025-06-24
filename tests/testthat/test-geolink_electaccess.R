# Test A-
test_that("Electaccess using a shapefile: ",
          {
            suppressWarnings({ test_dt <- geolink_electaccess(shp_dt = shp_dt[shp_dt$ADM1_EN
                                                                             ==  "Abia",],
                                                              start_date = "2019-01-01",
                                                              end_date = "2019-12-31",
                                                              grid_size = 1000,
                                                              extract_fun = "mean")
            })
            #Write testing expressions below:
            #01 - expect the colnames will be created correctly
            expect_contains(colnames(test_dt), c("night_proportion_2019", "lightscore_2019", "light_composite_2019", "estimated_brightness_2019"))

            #03 - Test that the mean column values is between 0 and 1 based on the raster values

            expect_true(all(test_dt$night_proportion_2019[!is.na(test_dt$night_proportion_2019)] >= 0 &
                              test_dt$night_proportion_2019[!is.na(test_dt$night_proportion_2019)] <= 1),
                        info = "Values of night proportion should be between 0 and 1")
          })
#Test B
test_that("Electaccess using a survey: ",
          {
            suppressWarnings({ test_dt <- geolink_electaccess(survey_dt =
                                                                st_as_sf(hhgeo_dt[1:10],
                                                                        crs = 4326),
                                                              start_date = "2019-01-01",
                                                              end_date = "2019-12-31",
                                                              buffer_size = 1000,
                                                              extract_fun = "mean")
            })
            #Write testing expressions below:
            #01 - expect the colnames  are created correctly
            expect_contains(colnames(test_dt), c("night_proportion_2019", "lightscore_2019", "light_composite_2019", "estimated_brightness_2019"))

            #02 - expect the length of test_dt be the same as the survey
            expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

            #Expect the radios of the buffer to be a 1000 m
            expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)

            #03 - Test that the mean column values is between 0 and 1

            expect_true(all(test_dt$night_proportion_2019[!is.na(test_dt$night_proportion_2019)] >= 0 &
                              test_dt$night_proportion_2019[!is.na(test_dt$night_proportion_2019)] <= 1 ),
                        info = "Values of night proportion should be between 0 and 1")
          })

#Test C
test_that("Electaccess using a survey file from stata:",
          {
            suppressWarnings({ test_dt <- geolink_electaccess(survey_fn = test_path("testdata/xy_hhgeo_dt.dta"),
                                                              survey_lat = "y",
                                                              survey_lon = "x",
                                                              start_date = "2019-01-01",
                                                              end_date = "2019-12-31",
                                                              buffer_size = 1000,
                                                              extract_fun = "mean")
            })
            #Write testing expressions below:
            #01 - expect the colnames  are created correctly
            expect_contains(colnames(test_dt), c("night_proportion_2019", "lightscore_2019", "light_composite_2019", "estimated_brightness_2019"))

            #02 - expect the length of test_dt be the same as the survey
            expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid))

            #Expect the radios of the buffer to be a 1000 m
            expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)

            #03 - Test that the mean column values is between 0 and 1

            expect_true(all(test_dt$night_proportion_2019[!is.na(test_dt$night_proportion_2019)] >= 0 &
                              test_dt$night_proportion_2019[!is.na(test_dt$night_proportion_2019)] <= 1 ),
                        info = "Values of night proportion should be between 0 and 1")
          })

#Test D
test_that("Error when using incorrect date",
          {
            expect_error(geolink_electaccess(survey_dt =
                                                st_as_sf(hhgeo_dt[1:10],
                                                        crs = 4326),
                                              start_date = "2019-01-01",
                                              end_date = "2018-12-31",
                                              buffer_size = 1000,
                                              extract_fun = "mean"),
                         regexp = "The closed date time provided is not in correct interval")
          })

#Test E
test_that("Using a shapefile from geodata: ",{
  suppressWarnings({
    temp_gamd <- sf::st_as_sf(geodata::gadm("COL", level = 2, tempdir()))

    test_dt <- geolink_electaccess(start_date = "2019-01-01",
                                   end_date = "2019-12-31",
                                   grid_size = NULL,
                                   shp_dt = temp_gamd[temp_gamd$NAME_1 == "Antioquia",])

    suggest_dt <- crsuggest::suggest_crs(temp_gamd,
                                         units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("night_proportion_2019", "lightscore_2019", "light_composite_2019", "estimated_brightness_2019"))

  #03 - Test that the mean column values is between 0 and 1 based on the raster values
  expect_true(all(test_dt$night_proportion_2019[!is.na(test_dt$night_proportion_2019)] >= 0 &
                    test_dt$night_proportion_2019[!is.na(test_dt$night_proportion_2019)] <= 1),
              info = "Values of night proportion should be between 0 and 1")
})
