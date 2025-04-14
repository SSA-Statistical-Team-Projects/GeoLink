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
            expect_contains(colnames(test_dt), c("n02_y2019_m1"))

            #03 - Test that the mean column values is between 0 and 1 based on the raster values

            expect_true(all(test_dt$no2_y2019_m2[!is.na(test_dt$no2_y2019_m2)] >= 0 &
                              test_dt$no2_y2019_m2[!is.na(test_dt$no2_y2019_m2)] <= 1),
                        info = "Values of ndvi should be between 0 and 1")

            expect_equal(length(unique(test_dt$poly_id)),
                         suppressWarnings({
                           length(gengrid2(shp_dt =
                                             st_transform(shp_dt[shp_dt$ADM1_EN ==  "Abia",],
                                                          crs = as.numeric(suggest_dt$crs_code[1])),
                                           grid_size = 1000)$poly_id)}))
          })
