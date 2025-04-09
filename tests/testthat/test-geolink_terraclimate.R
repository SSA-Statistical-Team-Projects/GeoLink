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


  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

  #03 - Test that the mean column values is between -19.0 and 2381.0 based on teh tile
  #this specific region

  expect_true(all(test_dt$tmax_Jan[!is.na(test_dt$tmax_Jan)] >= -40.80 &
                    test_dt$tmax_Jan[!is.na(test_dt$tmax_Jan)] <= 41.50),
              info = "Values of tmax_Jan should be between -40.80and 41.50")


}
)
