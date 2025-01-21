###############################################################################
###Test for mach functions: Landcover

################################################################################
################################################################################


#Test- A.
test_that("geolink_landcover works using a shapefile:", {

  suppressWarnings({ test_dt <- geolink_landcover(start_date = "2020-01-01",
                                                  end_date = "2020-12-01",
                                                  shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                  grid_size = 1000)

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("no_data_2020", "water_2020", "trees_2020",
                                       "flooded_vegetation_2020", "crops_2020" ,
                                       "built_area_2020", "bare_ground_2020",
                                       "snow_ice_2020",  "clouds_2020",
                                       "rangeland_2020"))

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))

  #03 - Test that the mean column values is between 0 and 1444.34

  expect_true(all(test_dt$trees_2020 >= 0 & test_dt$trees_2020 <= 100),
              info = "Values of trees_2020 should be between 0 and 100")

}
)


#Test- B
test_that("geolink_landcover using a survey :", {

  suppressWarnings({ test_dt <-  geolink_landcover(start_date = "2020-01-01",
                                                   end_date = "2020-12-01",
                                                   survey_dt =  st_as_sf(hhgeo_dt[1:10],
                                                                      crs = 4326),
                                                buffer_size = 1000)

  })

  #Write testing expressions below:
  #01 - expect the colnames  are created correctly
  expect_contains(colnames(test_dt), c("no_data_2020", "water_2020", "trees_2020",
                                       "flooded_vegetation_2020", "crops_2020" ,
                                       "built_area_2020", "bare_ground_2020",
                                       "snow_ice_2020",  "clouds_2020",
                                       "rangeland_2020"))

  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)

  #03 - Test that the mean column values is between 0 and 1444.34
  expect_true(all(test_dt$flooded_vegetation_2020 >= 0 & test_dt$flooded_vegetation_2020 <= 1),
              info = "Values of flooded_vegetation_2020 should be between 0 and 100")

}
)



# Test -C
test_that("geolink_landcover using a survey for stata users :", {

  suppressWarnings({ test_dt <-  geolink_landcover(start_date = "2020-01-01",
                                                   end_date = "2020-12-01",
                                                   survey_fn = "testdata/xy_hhgeo_dt.dta",
                                                   survey_lon = "x",
                                                   survey_lat = "y",
                                                   buffer_size = 1000)
  })

  #Write testing expressions below:
  #01 - expect the colnames  are created correctly
  expect_true("flooded_vegetation_2020" %in% colnames(test_dt),
              info = "Column 'flooded_vegetation_2020' is not present in test_dt")

  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid), nrow(haven::read_dta("testdata/xy_hhgeo_dt.dta")))

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)

  #03 - Test that the mean column values is between 0 and 1444.34
  expect_true(all(test_dt$flooded_vegetation_2020 >= 0 & test_dt$flooded_vegetation_2020 <= 100),
              info = "Values of flooded_vegetation_2020 should be between 0 and 1444.34")

}
)



#
# # Test -C
# test_that("geolink_landcover using a survey for stata users :", {
#
#   suppressWarnings({ test_dt <-  geolink_landcover(start_date = "2020-01-01",
#                                                    end_date = "2020-12-01",
#                                                    survey_fn = "tests/testthat/testdata/xy_hhgeo_dt.dta",
#                                                    survey_lon = "x",
#                                                    survey_lat = "y",
#                                                    buffer_size = 1000)
#   })
#
#   #Write testing expressions below:
#   #01 - expect the colnames  are created correctly
#   expect_true("flooded_vegetation_2020" %in% colnames(test_dt),
#               info = "Column 'flooded_vegetation_2020' is not present in test_dt")
#
#   #02 - expect the length of test_dt be the same as the survey
#   expect_equal(length(test_dt$hhid), nrow(haven::read_dta("tests/testthat/testdata/xy_hhgeo_dt.dta")))
#
#   #Expect the radios of the buffer to be a 1000 m
#   expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)
#
#   #03 - Test that the mean column values is between 0 and 1444.34
#   expect_true(all(test_dt$flooded_vegetation_2020 >= 0 & test_dt$flooded_vegetation_2020 <= 100),
#               info = "Values of flooded_vegetation_2020 should be between 0 and 100")
#
# }
# )
#
#
#
#
#
#
#
#
#
