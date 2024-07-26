#################################################################################
# TEST CROPLAND FUNCTION
#################################################################################

#Test- A.
test_that("Cropland Function works using a shapefile:", {

  suppressWarnings({ test_dt <- geolink_cropland(shp_dt = shp_dt[shp_dt$ADM1_EN
                                                                 ==  "Abia",],
                                                  grid_size = 1000,
                                                  extract_fun = "mean")

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), "cropland")

  #03 - Test that the mean column values is between 0 and 1 based on the raster values

  expect_true(all(test_dt$cropland[!is.na(test_dt$cropland)] >= 0 &
                    test_dt$cropland[!is.na(test_dt$cropland)] <= 1),
              info = "Values of cropland should be between 0 and 1")

})



#Test- B
test_that("Cropland Function works using a survey :", {

  suppressWarnings({ test_dt <- geolink_cropland(survey_dt =
                                                    st_as_sf(hhgeo_dt[1:10],
                                                                crs = 4326),
                                                  buffer_size = 1000,
                                                  extract_fun = "mean")

  })

  #Write testing expressions below:
  #01 - expect the colnames  are created correctly
  expect_contains(colnames(test_dt), "cropland")

  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)

  #03 - Test that the mean column values is between 0 and 1

  expect_true(all(test_dt$cropland[!is.na(test_dt$cropland)] >= 0 &
                    test_dt$cropland[!is.na(test_dt$cropland)] <= 1 ),
              info = "Values of cropland should be between 0 and 1")


}
)


