# Test A-
test_that("Pollution using a shapefile: ",
          {
            suppressWarnings({ test_dt <- geolink_pollution(shp_dt = shp_dt[shp_dt$ADM1_EN ==  "Abia",],
                                                           start_date = "2019-01-01",
                                                           end_date = "2019-02-28",
                                                           indicator = "no2",
                                                           grid_size = 1000,
                                                           extract_fun = "mean")

            suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                                 units = "m")
            })
            #Write testing expressions below:
            #01 - expect the colnames will be created correctly
            expect_contains(colnames(test_dt), c("no2_y2019_m1"))

            #03 - Test that the mean column values is between 0 and 1 based on the raster values

            expect_true(all(test_dt$no2_y2019_m2[!is.na(test_dt$no2_y2019_m2)] >= -1 &
                              test_dt$no2_y2019_m2[!is.na(test_dt$no2_y2019_m2)] <= 1),
                        info = "Values of no2_y2019_m2 should be between -1 and 1")

            expect_equal(length(unique(test_dt$poly_id)),
                         suppressWarnings({
                           length(gengrid2(shp_dt =
                                             st_transform(shp_dt[shp_dt$ADM1_EN ==  "Abia",],
                                                          crs = as.numeric(suggest_dt$crs_code[1])),
                                           grid_size = 1000)$poly_id)}))
          })

# Test B-
test_that("Pollution using a survey: ",
          {
            suppressWarnings({ test_dt <- geolink_pollution(survey_dt = st_as_sf(hhgeo_dt[1:10],
                                                                     crs = 4326),
                                                           start_date = "2019-01-01",
                                                           end_date = "2019-12-28",
                                                           indicator = "aer-ai",
                                                           buffer_size = 1000,
                                                           extract_fun = "mean")

            suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                                 units = "m")
            })
            #Write testing expressions below:
            #01 - expect the colnames will be created correctly
            expect_contains(colnames(test_dt), c("aer-ai_y2019_m6"))

            #03 - Test that the mean column values is between 0 and 1 based on the raster values

            expect_true(all((test_dt$`aer-ai_y2019_m6`[!is.na(test_dt$`aer-ai_y2019_m6`)] >= -5 &
                              test_dt$`aer-ai_y2019_m6`[!is.na(test_dt$`aer-ai_y2019_m6`)] <= 5) | is.na(test_dt$`aer-ai_y2019_m6`)),
                        info = "Values of aer-ai_y2019_m6 should be between -5 and 5 or NA")
          })

# Test C-
test_that("Pollution using a survey: ",
          {
            suppressWarnings({ test_dt <- geolink_pollution(survey_fn = test_path("testdata/xy_hhgeo_dt.dta"),
                                                            survey_lat = "y",
                                                            survey_lon = "x",
                                                           start_date = "2019-01-01",
                                                           end_date = "2019-12-28",
                                                           indicator = "hcho",
                                                           buffer_size = 1000,
                                                           extract_fun = "mean")

            suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                                 units = "m")
            })
            #Write testing expressions below:
            #01 - expect the colnames will be created correctly
            expect_contains(colnames(test_dt), c("hcho_y2019_m8"))

            #03 - Test that the mean column values is between 0 and 1 based on the raster values

            expect_true(all((test_dt$`hcho_y2019_m8`[!is.na(test_dt$`hcho_y2019_m8`)] >= -5 &
                              test_dt$`hcho_y2019_m8`[!is.na(test_dt$`hcho_y2019_m8`)] <= 5) | is.na(test_dt$`hcho_y2019_m8`)),
                        info = "Values of hcho_y2019_m8 should be between -5 and 5 or NA")
          })

#Test- D
test_that("Pollution using a shapefile from geodata package:", {
  suppressWarnings({
    # Load the shapefile from geodata package
    temp_gadm <- geodata::gadm(country = "COL", level = 2, path = tempdir())

    test_dt <- geolink_pollution(shp_dt = temp_gadm[temp_gadm$NAME_1 ==  "Antioquia",],
                                                   start_date = "2019-01-01",
                                                   end_date = "2019-12-28",
                                                   indicator = "o3",
                                                   extract_fun = "mean")

    suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                         units = "m")
  })
  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("o3_y2019_m4"))

  #03 - Test that the mean column values is between -5 and 5 based on the raster values
  expect_true(all((test_dt$o3_y2019_m4[!is.na(test_dt$o3_y2019_m4)] >= -5 &
                    test_dt$o3_y2019_m4[!is.na(test_dt$o3_y2019_m4)] <= 5) | is.na(test_dt$o3_y2019_m4)),
                  info = "Values of o3_y2019_m4 should be between -5 and 5 or NA")
})
