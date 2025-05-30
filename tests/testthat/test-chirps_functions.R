###############################################################################
###Test for mach functions: chirps

################################################################################
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

  suppressWarnings({ test_dt <-  geolink_chirps(time_unit = "month",
                              start_date = "2024-01-01",
                              end_date = "2024-02-01",
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



# Test -C
test_that("Monthly chirps using a survey for stata users :", {

  suppressWarnings({ test_dt <- geolink_chirps(time_unit = "month",
                                               start_date = "2020-01-01",
                                               end_date = "2020-02-01",
                                               survey_fn = test_path("testdata/xy_hhgeo_dt.dta"),
                                               survey_lat = "y",
                                               survey_lon = "x",
                                               buffer_size = 1000,
                                               extract_fun = "mean")
  })

  #Write testing expressions below:
  #01 - expect the colnames  are created correctly
  expect_true("rainfall_month2" %in% colnames(test_dt),
              info = "Column 'rainfall_month1' is not present in test_dt")

  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid),
               nrow(haven::read_dta(test_path("testdata/xy_hhgeo_dt.dta"))))

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)

  #03 - Test that the mean column values is between 0 and 1444.34
  expect_true(all(test_dt$rainfall_month1 >= 0 & test_dt$rainfall_month1 <= 1444.34),
              info = "Values of rainfall_month1 should be between 0 and 1444.34")

}
)


###``
#Test- D
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


#Test- E
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

   suppressWarnings({ expect_error(geolink_chirps(time_unit = "month",
                              start_date = "2024-01-01",
                              end_date = "2020-02-01",
                              shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                              grid_size = 1000,
                              extract_fun = "mean"),
               regexp = "Invalid time range, start time exceeds end time!")
  })
})


#Test- F
test_that("Annual chirps using a shapefile from geodata package:", {

  suppressWarnings({
    temp_gamd <- sf::st_as_sf(geodata::gadm("COL", level = 2, tempdir()))

    test_dt <- geolink_chirps(time_unit = "annual",
                                               start_date = "2020-01-01",
                                               end_date = "2020-12-31",
                                               shp_dt = temp_gamd[temp_gamd$NAME_1 == "Antioquia",],
                                               extract_fun = "mean")

  suggest_dt <- crsuggest::suggest_crs(temp_gamd,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames ro be created correctly
  expect_contains(colnames(test_dt), "rainfall_annual1" )


  #03 - Test that the mean column values is between 0 and 1444.34
  expect_true(all(test_dt$rainfall_month1 >= 0 & test_dt$rainfall_month1 <= 1444.34),
              info = "Values of rainfall_month1 should be between 0 and 1444.34")
})



