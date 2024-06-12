###############################################################################
###Test for mach functions: geolink_electaccess
################################################################################
#Read in the data
#Shapefile
data("shp_dt")
#Survey
data("hhgeo_dt")
################################################################################
##Run Tests
################################################################################

test_that("Electricity output is correct:", {

  suppressWarnings({ test_dt <- geolink_electaccess(time_unit = "annual",
                                                    start_date = "2020-01-01",
                                                    end_date = "2021-01-01",
                                                    shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],)


  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("elect_"))

  #03 - Test that the mean column values is between 0 and 1444.34
  expect_true(all(test_dt$elect_ >= 0 & test_dt$elect_<= 1),
              info = "Values of lightscore should be between 0 and 1")

}
)





test_dt <- geolink_electaccess(time_unit = "annual",
                               start_date = "2020-01-01",
                               end_date = "2022-01-01",
                               survey_dt= st_as_sf(hhgeo_dt[1:50],
                                       crs = 4326),
                               buffer_size = 1000,
                               extract_fun = "mean")

plot(shp_dt[shp_dt$ADM1_EN == "Abia",]$geometry)
plot(test_dt$geometry)
summary(test_dt$elect_)



