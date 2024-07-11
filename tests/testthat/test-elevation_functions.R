#################################################################################
# TEST ELEVATION FUNCTION
#################################################################################

#Test- A.
test_that("Elevation Function works using a shapefile:", {

  suppressWarnings({ test_dt <- geolink_elevation(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                  grid_size = 1000,
                                                  extract_fun = "mean")

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), "srtm_38_11")

  #03 - Test that the mean column values is between -19.0 and 2381.0 based on teh tile
  #this specific region

  expect_true(all(test_dt$srtm_38_11[!is.na(test_dt$srtm_38_11)] >= -19.0 &
                    test_dt$srtm_38_11[!is.na(test_dt$srtm_38_11)] <= 2381.0),
              info = "Values of srtm_38_11 should be between -19.0 and 2381.0")

})





#Test- B
test_that("Elevation Function works using a survey :", {

  suppressWarnings({ test_dt <- geolink_elevation(survey_dt =  st_as_sf(hhgeo_dt[1:10],
                                                                        crs = 4326),
                                                  buffer_size = 1000,
                                                  extract_fun = "mean")

  })

  #Write testing expressions below:
  #01 - expect the colnames  are created correctly
  expect_contains(colnames(test_dt), "srtm_38_11")

  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)

  #03 - Test that the mean column values is between -19.0 and 2381.0 based on teh tile
  #this specific region

  expect_true(all(test_dt$srtm_38_11[!is.na(test_dt$srtm_38_11)] >= -19.0 &
                    test_dt$srtm_38_11[!is.na(test_dt$srtm_38_11)] <= 2381.0),
              info = "Values of srtm_38_11 should be between -19.0 and 2381.0")


}
)


