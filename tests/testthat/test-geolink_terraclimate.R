#################################################################################
# TEST Terraclimate
#################################################################################

#Test- A.
test_that("Terraclimate works using a shapefile:", {

  suppressWarnings({ test_dt <- geolink_terraclimate( var='tmax',
                                                  year = 2017,
                                                  shp_dt = shp_dt[shp_dt$ADM1_EN
                                                                  == "Abia",],
                                                  grid_size = 1000,
                                                  extract_fun = "mean")

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), "tmax_Jan")

  #03 - Test that the mean column values is between 10 and 45

  expect_true(all(test_dt$tmax_Jan[!is.na(test_dt$tmax_Jan)] >= -40.80   &
                    test_dt$tmax_Jan[!is.na(test_dt$tmax_Jan)] <= 41.50),
              info = "Values of NGA_WC_tmax_Sep should be between -40.80 and-40.80")

})



#Test- B
test_that("Terraclimate works using a survey :", {

  suppressWarnings({ test_dt <- geolink_terraclimate(year = 2017,
                                                  var = 'tmax',
                                                  survey_dt =  st_as_sf(hhgeo_dt[1:10],
                                                                        crs = 4326),
                                                  buffer_size = 1000,
                                                  extract_fun = "mean")

  })

  #Write testing expressions below:
  #01 - expect the colnames  are created correctly
  expect_contains(colnames(test_dt), "tmax_Jan")

  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)

  #03 - Test that the mean column values is between -19.0 and 2381.0 based on teh tile
  #this specific region

  expect_true(all(test_dt$tmax_Jan[!is.na(test_dt$tmax_Jan)] >= -40.80 &
                    test_dt$tmax_Jan[!is.na(test_dt$tmax_Jan)] <= 41.50),
              info = "Values of tmax_Jan should be between -40.80and 41.50")


}
)
