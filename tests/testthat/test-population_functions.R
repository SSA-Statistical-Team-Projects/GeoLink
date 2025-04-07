###############################################################################
###Test for mach functions: Population
###############################################################################
################################################################################
#2- Begin testings the function when using a shapefile and a raster #############
#################

#Test- A.
test_that("Population works using a shapefile:", {
#Create a consistent file directory:

  suppressWarnings({ test_dt <- geolink_population(start_year = 2018,
                                                   end_year = 2019,
                                                   iso_code = "NGA",
                                                   UN_adjst = "N",
                                                   constrained = "N",
                                                   shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                   grid_size = 1000,
                                                   extract_fun = "mean",
                                                   file_location = tempdir())

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("population_2018","population_2019" ))

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))

  #03 - Test that the mean column values is between 0 and 1444.34
  non_na_population <- na.omit(test_dt$population_2020)
  expect_true(all(non_na_population >= 0.02873377 & non_na_population <= 1022.052),
              info = "Values of population_2018 should be between 0.02873377 and 1022.052.")

}
)


#Test- B
test_that("Population works using a survey :", {

  suppressWarnings({ test_dt <- geolink_population(start_year = 2018,
                                                  end_year = 2019,
                                                  iso_code = "NGA",
                                                  UN_adjst = "N",
                                                  constrained = "N",
                                                  survey_dt =  st_as_sf(hhgeo_dt[1:10],
                                                                        crs = 4326),
                                                  buffer_size = 1000,
                                                  extract_fun = "mean",
                                                  file_location =tempdir())


  })

  #Write testing expressions below:
  #01 - expect the colnames  are created correctly
  expect_contains(colnames(test_dt), c("population_2018","population_2019"))

  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)


  # Test that the mean column values are between the raster values, ignoring NAs
  non_na_population <- na.omit(test_dt$population_2020)

  # Test that the mean column values are between the raster values, ignoring NAs
  expect_true(all(non_na_population >= 1.287057 & non_na_population <= 9491.771484),
              info = sprintf("Values of population_2020 should be between 1.287057 and 9491.771484. Actual range: [%f, %f]",
                             min(non_na_population), max(non_na_population)))

}
)



#Test- C
test_that("Enrure argument works:", {

  suppressWarnings({ test_dt <- geolink_population(start_year = 2020,
                                                   end_year = 2020,
                                                   iso_code = "NGA",
                                                   UN_adjst = "Y",
                                                   constrained = "Y",
                                                   shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                   grid_size = 1000,
                                                   extract_fun = "mean",
                                                   file_location = tempdir())

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("population_2020"))


  #03 - Test that the mean column values is btween the raster values
  non_na_population <- na.omit(test_dt$population_2020)

  # Test that the mean column values are between the raster values, ignoring NAs
  expect_true(all(non_na_population >= 1.287057 & non_na_population <= 9491.771484),
              info = sprintf("Values of population_2020 should be between 1.287057 and 9491.771484. Actual range: [%f, %f]",
                             min(non_na_population), max(non_na_population)))


  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))



})


##Test Error Messages
test_that("Error is thrown for invalid  link construction", {
  expect_error( test_dt <- geolink_population(start_year = 2018,
                                             end_year = 2018,
                                             iso_code = "NGA",
                                             UN_adjst = "N",
                                             constrained = "Y",
                                             shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                             grid_size = 1000,
                                             extract_fun = "mean",
                                            file_location = tempdir())

               ,
               regexp = "No valid raster files found.")
})


#Test- D.
test_that("Population works using a shapefile from package geodata:", {
  #Create a consistent file directory:

  suppressWarnings({
    temp_gamd <- sf::st_as_sf(geodata::gadm("KEN", level = 2, tempdir()))

    test_dt <- geolink_population(start_year = 2018,
                                                   end_year = 2019,
                                                   iso_code = "KEN",
                                                   UN_adjst = "N",
                                                   constrained = "N",
                                                   shp_dt = temp_gamd[temp_gamd$NAME_1 == "Nairobi",],
                                                   extract_fun = "mean",
                                                   file_location = tempdir())

  suggest_dt <- crsuggest::suggest_crs(temp_gamd,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("population_2018","population_2019" ))


  #03 - Test that the mean column values is between 0 and 1444.34
  non_na_population <- na.omit(test_dt$population_2020)
  expect_true(all(non_na_population >= 0.02873377 & non_na_population <= 1022.052),
              info = "Values of population_2018 should be between 0.02873377 and 1022.052.")

}
)
