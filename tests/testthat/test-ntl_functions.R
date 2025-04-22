###############################################################################
###Test for mach functions: NTL

################################################################################
################################################################################
#Begin testingss the function when using a shapefile and a raster #############
################################################################################

#Test- A.
test_that("Monthly ntl works using a shapefile:", {

  suppressWarnings({ test_dt <- geolink_ntl(time_unit = "month",
                                              start_date = "2020-01-01",
                                              end_date = "2020-03-01",
                                              shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                              indicator = "avg_rade9h",
                                              grid_size = 1000,
                                              extract_fun = "mean")


  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), "ntl_month2avg_rade9h" )

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))

  #03 - Test that the mean column values is between 0 and 1444.34

  expect_true(all(test_dt$ntl_month3avg_rade9h >= -1.5 & test_dt$ntl_month3avg_rade9h <= 162790.6 ),
              info = "Values of ntl_month3avg_rade9h should be between -1.5 and 162790.6")

}
)


#Test- B
test_that("Monthly ntl using a survey :", {

  suppressWarnings({ test_dt <-  geolink_ntl(time_unit = "month",
                                             start_date = "2020-01-01",
                                             end_date = "2020-03-01",
                                             survey_dt =  st_as_sf(hhgeo_dt[1:10],
                                                                   crs = 4326),
                                             indicator = c("avg_rade9h"),
                                             buffer_size = 1000,
                                             extract_fun = "mean")

  })

  #Write testing expressions below:
  #01 - expect the colnames  are created correctly
  expect_contains(colnames(test_dt), c("ntl_month1avg_rade9h","ntl_month2avg_rade9h", "ntl_month3avg_rade9h"))

  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)

  #03 - Test that the mean column values is between -1.5 and  162790.6
  expect_true(all(test_dt$ntl_month3avg_rade9h >= -1.5 & test_dt$ntl_month3avg_rade9h <= 162790.6),
              info = "Values of ntl_month3avg_rade9h should be between -1.5 and  162790.6")

}
)


###
#Test- C
test_that("Annual NTL using a shapefile:", {

  suppressWarnings({ test_dt <- geolink_ntl(time_unit = "annual",
                                            start_date = "2020-01-01",
                                            end_date = "2020-12-01",
                                            shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                            indicator = "average_masked",
                                            grid_size = 1000,
                                            extract_fun = "mean")

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                        units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames ro be created correctly
  expect_contains(colnames(test_dt), "ntl_annual1average_masked" )


  expect_true(all(test_dt$ntl_annual1average_masked >= -1.5 & test_dt$ntl_annual1average_masked <= 92084.44 ),
              info = "Values of ntl_annual1average_masked should be between -1.5 and 92084.44")
})


#Test- D
test_that("Annual ntl using a survey :", {

  suppressWarnings({ test_dt <- geolink_ntl(time_unit = "annual",
                                            start_date = "2020-01-01",
                                            end_date = "2021-12-01",
                                            survey_dt =  st_as_sf(hhgeo_dt[1:10],
                                                                  crs = 4326),
                                            buffer_size = 1000,
                                            indicator = c("average_masked","cf_cvg"),
                                            extract_fun = "mean")


  })

  #Write testing expressions below:
  #01 - expect the colnames  are created correctly
  expect_contains(colnames(test_dt), "ntl_annual1average_masked")

  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)


  #03 - Test that the mean column values is between 0 and 1444.34
  expect_true(all(test_dt$ntl_annual1average_masked >= -1.5 & test_dt$ntl_annual1average_masked <= 92084.44 ),
              info = "Values of ntl_annual1average_masked should be between -1.5 and 92084.44")
}
)


#Test- E
test_that("Annual NTL using a shapefile from geodata package:", {

  suppressWarnings({
    temp_gamd <- sf::st_as_sf(geodata::gadm("COL", level = 2, tempdir()))

    test_dt <- geolink_ntl(time_unit = "annual",
                                            start_date = "2019-01-01",
                                            end_date = "2019-12-01",
                                            shp_dt = temp_gamd[temp_gamd$NAME_1 == "Antioquia",],
                                            indicator = "cf_cvg",
                                            extract_fun = "mean")

  suggest_dt <- crsuggest::suggest_crs(temp_gamd,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames ro be created correctly
  expect_contains(colnames(test_dt), "ntl_annual1cf_cvg" )


  expect_true(all(test_dt$ntl_annual1cf_cvg >= 0 & test_dt$ntl_annual1cf_cvg <= 100 ),
              info = "Values of ntl_annual1cf_cvg should be between 0 and 100")
})




