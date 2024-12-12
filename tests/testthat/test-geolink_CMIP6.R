###############################################################################
###Test fo CMIP6

################################################################################
################################################################################

#Test- A.
test_that("CMIP6 works using a shapefile:", {

  suppressWarnings({ test_dt <- geolink_CMIP6(start_date = "2019-01-01",
                                              end_date = "2019-12-31",
                                              scenario = "ssp245",
                                              desired_models = "UKESM1-0-LL",
                                              shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                              grid_size = 1000)


  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("pr_2019",  "tas_2019", "hurs_2019",
                                       "huss_2019", "rlds_2019",
                                       "rsds_2019", "tasmax_2019",
                                       "tasmin_2019", "sfcWind_2019"))

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))

  #03 - Test that the mean column values is between the ranges specified in the
  # rasters

  expect_true(all(test_dt$pr_2019 >= 0 & test_dt$pr_2019 <= 0.000249315 ),
              info = "Values of pr should be between 0 and 0.000249315")

  expect_true(all(test_dt$tas_2019 >= 247.7378  & test_dt$tas_2019 <= 306.4844 ),
              info = "Values of pr should be between 247.7378  and 306.4844 ")

  expect_true(all(test_dt$hurs_2019 >= 16.29199  & test_dt$hurs_2019 <= 97.11811 ),
              info = "Values of pr should be between 16.29199  and 97.11811")

  expect_true(all(test_dt$huss_2019 >= 0.0006420442   & test_dt$huss_2019 <= 0.0255407595),
              info = "Values of pr should be between 0.0006420442  and 0.0255407595 ")

  expect_true(all(test_dt$rlds_2019 >= 133.9438    & test_dt$rlds_2019 <= 443.8867),
              info = "Values of pr should be between 133.9438  and 443.8867 ")

  expect_true(all(test_dt$rsds_2019 >= 93.35552    & test_dt$rsds_2019 <= 304.32153),
              info = "Values of pr should be between 93.35552    and 304.32153")

  expect_true(all(test_dt$tasmax_2019 >= 250.3406    & test_dt$tasmax_2019 <= 313.9287),
              info = "Values of pr should be between 250.3406    and 313.9287 ")

  expect_true(all(test_dt$tasmin_2019 >= 244.7839     & test_dt$tasmin_2019 <= 303.2740),
              info = "Values of pr should be between 244.7839    and 303.2740 ")

  expect_true(all(test_dt$fcWind_2019 >= 0.5383518   & test_dt$fcWind_2019 <= 11.5499266),
              info = "Values of pr should be between 0.5383518  and 11.5499266")

}
)


#Test- B
test_that("CMIP6 using a survey :", {

  suppressWarnings({ test_dt <- geolink_CMIP6(start_date = "2019-01-01",
                                              end_date = "2019-12-31",
                                              scenario = "ssp245",
                                              desired_models = "UKESM1-0-LL",
                                              survey_dt =  st_as_sf(hhgeo_dt[1:10],
                                                                    crs = 4326),
                                              buffer_size  = 1000,
                                              extract_fun = "mean")

  })

  #Write testing expressions below:
  #01 - expect the colnames  are created correctly
  expect_contains(colnames(test_dt), c("pr_2019",  "tas_2019", "hurs_2019",
                                       "huss_2019", "rlds_2019",
                                       "rsds_2019", "tasmax_2019",
                                       "tasmin_2019", "sfcWind_2019"))

  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)

  #03 - Test that the mean column values is between 0 and 1444.34
  expect_true(all(test_dt$pr_2019 >= 0 & test_dt$pr_2019 <= 0.000249315 ),
              info = "Values of pr should be between 0 and 0.000249315")

  expect_true(all(test_dt$tas_2019 >= 247.7378  & test_dt$tas_2019 <= 306.4844),
              info = "Values of pr should be between 247.7378  and 306.4844 ")

  expect_true(all(test_dt$hurs_2019 >= 16.29199  & test_dt$hurs_2019 <= 97.11811),
              info = "Values of pr should be between 16.29199  and 97.11811")

  expect_true(all(test_dt$huss_2019 >= 0.0006420442   & test_dt$huss_2019 <= 0.0255407595),
              info = "Values of pr should be between 0.0006420442  and 0.0255407595 ")

  expect_true(all(test_dt$rlds_2019 >= 133.9438    & test_dt$rlds_2019 <= 443.8867),
              info = "Values of pr should be between 133.9438  and 443.8867 ")

  expect_true(all(test_dt$rsds_2019 >= 93.35552    & test_dt$rsds_2019 <= 304.32153),
              info = "Values of pr should be between 93.35552    and 304.32153")

  expect_true(all(test_dt$tasmax_2019 >= 250.3406    & test_dt$tasmax_2019 <= 313.9287),
              info = "Values of pr should be between 250.3406    and 313.9287 ")

  expect_true(all(test_dt$tasmin_2019 >= 244.7839     & test_dt$tasmin_2019 <= 303.2740),
              info = "Values of pr should be between 244.7839    and 303.2740 ")

  expect_true(all(test_dt$fcWind_2019 >= 0.5383518   & test_dt$fcWind_2019 <= 11.5499266),
              info = "Values of pr should be between 0.5383518  and 11.5499266")

}
)






