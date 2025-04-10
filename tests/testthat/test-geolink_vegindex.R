# Test A-
test_that("Vegindex using a shapefile: ",
          {
            suppressWarnings({ test_dt <- geolink_vegindex(shp_dt = shp_dt[shp_dt$ADM1_EN ==  "Abia",],
                                                              start_date = "2019-01-01",
                                                              end_date = "2019-12-31",
                                                              grid_size = 1000,
                                                              extract_fun = "mean")
            })
            #Write testing expressions below:
            #01 - expect the colnames will be created correctly
            expect_contains(colnames(test_dt), c("ndvi_y2019_m04"))

            #03 - Test that the mean column values is between 0 and 1 based on the raster values

            expect_true(all(test_dt$ndvi_y2019_m04[!is.na(test_dt$ndvi_y2019_m04)] >= -1 &
                              test_dt$ndvi_y2019_m04[!is.na(test_dt$ndvi_y2019_m04)] <= 1),
                        info = "Values of ndvi should be between -1 and 1")

            expect_equal(length(unique(test_dt$poly_id)),
                         suppressWarnings({
                           length(gengrid2(shp_dt =
                                             st_transform(shp_dt[shp_dt$ADM1_EN ==  "Abia",],
                                                          crs = as.numeric(suggest_dt$crs_code[1])),
                                           grid_size = 1000)$poly_id)}))
          })

#Test B
test_that("Vegindex using a survey: ",
          {
            suppressWarnings({ test_dt <- geolink_vegindex(survey_dt =
                                                             st_as_sf(hhgeo_dt[1:10],
                                                                     crs = 4326),
                                                           start_date = "2019-04-01",
                                                           end_date = "2019-06-30",
                                                           buffer_size = 1000,
                                                           extract_fun = "mean")
            })
            #Write testing expressions below:
            #01 - expect the colnames  are created correctly
            expect_contains(colnames(test_dt), c("ndvi_y2019_m04"))

            #02 - expect the length of test_dt be the same as the survey
            expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

            #Expect the radios of the buffer to be a 1000 m
            expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)

            #03 - Test that the mean column values is between 0 and 1

            expect_true(all(test_dt$ndvi_y2019_m06[!is.na(test_dt$ndvi_y2019_m06)] >= -1 &
                              test_dt$ndvi_y2019_m06[!is.na(test_dt$ndvi_y2019_m06)] <= 1 ),
                        info = "Values of ndvi should be between -1 and 1")
          })

#Test C
test_that("Vegindex using a file of shapefile: ",
          {
            suppressWarnings({ test_dt <- geolink_vegindex(shp_fn = paste0(test_path(), "/testdata/shp_dt.shp"),
                                                           start_date = "2019-06-01",
                                                           end_date = "2019-06-30",
                                                           buffer_size = 1000,
                                                           extract_fun = "mean")
            })
            #Write testing expressions below:
            #01 - expect the colnames  are created correctly
            expect_contains(colnames(test_dt), c("ndvi_y2019_m06"))

            #03 - Test that the mean column values is between 0 and 1

            expect_true(all(test_dt$ndvi_y2019_m06[!is.na(test_dt$ndvi_y2019_m06)] >= -1 &
                              test_dt$ndvi_y2019_m06[!is.na(test_dt$ndvi_y2019_m06)] <= 1 ),
                        info = "Values of ndvi should be between -1 and 1")
          })

#Test D
test_that("Vegindex using a survey from stata file: ",
          {
            suppressWarnings({ test_dt <- geolink_vegindex(survey_fn = paste0(test_path(), "/testdata/xy_hhgeo_dt.dta"),
                                                           survey_lat = "y",
                                                           survey_lon = "x",
                                                           start_date = "2019-06-01",
                                                           end_date = "2019-06-30",
                                                           buffer_size = 1000,
                                                           extract_fun = "mean")
            })
            #Write testing expressions below:
            #01 - expect the colnames  are created correctly
            expect_contains(colnames(test_dt), c("ndvi_y2019_m06"))

            #03 - Test that the mean column values is between 0 and 1

            expect_true(all(test_dt$ndvi_y2019_m06[!is.na(test_dt$ndvi_y2019_m06)] >= -1 &
                              test_dt$ndvi_y2019_m06[!is.na(test_dt$ndvi_y2019_m06)] <= 1 ),
                        info = "Values of ndvi should be between -1 and 1")
          })


# Test E
test_that("Annual vegindex using a shapefile from geodata package:", {

  suppressWarnings({
    temp_gamd <- sf::st_as_sf(geodata::gadm("COL", level = 2, tempdir()))

    test_dt <- geolink_vegindex(start_date = "2019-01-01",
                           end_date = "2019-02-28",
                           indicator = "EVI",
                           shp_dt = temp_gamd[temp_gamd$NAME_1 == "Antioquia",],
                           extract_fun = "mean")

    suggest_dt <- crsuggest::suggest_crs(temp_gamd,
                                         units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames ro be created correctly
  expect_contains(colnames(test_dt), "evi_y2019_m02" )


  expect_true(all(test_dt$evi_y2019_m01[!is.na(test_dt$evi_y2019_m01)] >= 0 &
                    test_dt$evi_y2019_m01[!is.na(test_dt$evi_y2019_m01)] <= 1 ),
              info = "Values of evi should be between 0 and 1")
})
