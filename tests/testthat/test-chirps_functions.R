###############################################################################
###Test for mach functions: chirps and ntl

################################################################################
#1- Read in the data
################################################################################
#Shapefile
data("shp_dt")
#Survey
data("hhgeo_dt")
################################################################################
#2- Begin testingss the function when using a shapefile and a raster #############
################################################################################

#Test- A.
test_that("Monthly chirps works using a shapefile:", {

  suppressWarnings({ test_dt <- geolink_chirps(time_unit = "month",
                                               start_date = "2020-01-01",
                                               end_date = "2020-02-01",
                                               shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                               grid_size = 1000,
                                               extract_fun = "mean")

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("rainfall_month1","rainfall_month2" ))

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))

  #03 - Test that the mean column values is between 0 and 1444.34

  expect_true(all(test_dt$rainfall_month1 >= 0 & test_dt$rainfall_month1 <= 1444.34),
              info = "Values of rainfall_month1 should be between 0 and 1444.34")

}
)


#Test- B
test_that("Monthly chirps using a survey :", {

  suppressWarnings({ test_dt <- geolink_chirps(time_unit = "month",
                                               start_date = "2020-01-01",
                                               end_date = "2020-05-01",
                                               survey_dt =  st_as_sf(hhgeo_dt[1:10],
                                                                     crs = 4326),
                                               buffer_size = 1000,
                                               extract_fun = "mean")

  })

  #Write testing expressions below:
  #01 - expect the colnames  are created correctly
  expect_contains(colnames(test_dt), c("rainfall_month1","rainfall_month2" ))

  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)

  #03 - Test that the mean column values is between 0 and 1444.34
  expect_true(all(test_dt$rainfall_month1 >= 0 & test_dt$rainfall_month1 <= 1444.34),
              info = "Values of rainfall_month1 should be between 0 and 1444.34")

}
)


###
#Test- C
test_that("Annual chirps using a shapefile:", {

  suppressWarnings({ test_dt <- geolink_chirps(time_unit = "annual",
                                               start_date = "2020-01-01",
                                               end_date = "2020-12-31",
                                               shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                               grid_size = 1000,
                                               extract_fun = "mean")

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames ro be created correctly
  expect_contains(colnames(test_dt), "rainfall_annual1" )


  #03 - Test that the mean column values is between 0 and 1444.34
  expect_true(all(test_dt$rainfall_month1 >= 0 & test_dt$rainfall_month1 <= 1444.34),
              info = "Values of rainfall_month1 should be between 0 and 1444.34")
})


#Test- B
test_that("Annual chirps using a survey :", {

  suppressWarnings({ test_dt <- geolink_chirps(time_unit = "annual",
                                               start_date = "2020-01-01",
                                               end_date = "2020-12-31",
                                               survey_dt =  st_as_sf(hhgeo_dt[1:10],
                                                                     crs = 4326),
                                               buffer_size = 1000,
                                               extract_fun = "mean")

  })

  #Write testing expressions below:
  #01 - expect the colnames  are created correctly
  expect_contains(colnames(test_dt), "rainfall_annual1")

  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)


  #03 - Test that the mean column values is between 0 and 1444.34
  expect_true(all(test_dt$rainfall_month1 >= 0 & test_dt$rainfall_month1 <= 1444.34),
              info = "Values of rainfall_month1 should be between 0 and 1444.34")
}
)



##Test Error Messages
test_that("Error is thrown for invalid date range", {
  expect_error(geolink_chirps(time_unit = "month",
                              start_date = "2024-01-01",
                              end_date = "2020-02-01",
                              shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                              grid_size = 1000,
                              extract_fun = "mean"),
               regexp = "Invalid time range, start time exceeds end time!")
})


test_that("Error is thrown for date format", {
  expect_error(geolink_chirps(time_unit = "month",
                              start_date = "12-2020-01",
                              end_date = "2020-12-01",
                              shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                              grid_size = 1000,
                              extract_fun = "mean"),
               regexp = "start_date argument is not a Date, did you specify it in the form as.Date('yyyy-mm-dd')")
})








test_dt <- geolink_chirps(time_unit = "month",
                          start_date = "2020-01-01",
                          end_date = "2020-02-01",
                          shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                          grid_size = 1000,
                          extract_fun = "mean")



geolink_chirps(time_unit = "month",
               start_date = "2020-01-01",
               end_date = "2020-05-01",
               survey_dt =  st_as_sf(hhgeo_dt[1:10],
                                     crs = 4326),
               buffer_size = 1000,
               extract_fun = "mean")




geolink_chirps(time_unit = "month",
               start_date = "12-2020-01",
               end_date = "2020-12-01",
               shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
               grid_size = 1000,
               extract_fun = "mean")




