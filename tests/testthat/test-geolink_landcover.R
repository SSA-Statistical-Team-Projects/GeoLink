# Test A-
test_that("Landcover using shapefile: ", {
  suppressWarnings({ test_dt <- geolink_landcover(
                                                  start_date = "2019-01-01",
                                                  end_date = "2019-12-31",
                                                  shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                  use_resampling = FALSE
                                                  )

    suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                         units = "m")

  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("water"))

  #03 - Test that the mean column values is between 0 and 1 based on the raster values

  expect_true(all(test_dt$water[!is.na(test_dt$water)] >= 0 &
                     test_dt$water[!is.na(test_dt$water)] <= 100),
               info = "Values of landcover should be between 0 and 100")
})


# Test B
test_that("Landcover using survey data: ", {
  suppressWarnings({ test_dt <- geolink_landcover(
                                                  start_date = "2019-01-01",
                                                  end_date = "2019-12-31",
                                                  survey_dt = hhgeo_dt[1:10,],
                                                  buffer_size = 1000,
                                                  use_resampling = FALSE
                                                  )

    suggest_dt <- crsuggest::suggest_crs(test_dt,
                                         units = "m")

  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("water"))

  #03 - Test that the mean column values is between 0 and 1 based on the raster values

  expect_true(all(test_dt$water[!is.na(test_dt$water)] >= 0 &
                     test_dt$water[!is.na(test_dt$water)] <= 100),
               info = "Values of landcover should be between 0 and 100")
})


# Test C
test_that("Landcover using shapefile from geodata: ", {
  suppressWarnings({
    temp_gadm <- geodata::gadm(country = "PER",
                                        level = 2,
                                        path = tempdir(),
                                        overwrite = TRUE)

    test_dt <- geolink_landcover(start_date = "2019-01-01",
                                 end_date = "2019-01-31",
                                 shp_dt = temp_gadm[temp_gadm$NAME_1 == "Ayacucho",],
                                 use_resampling = FALSE
                                 )

    suggest_dt <- crsuggest::suggest_crs(test_dt,
                                         units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("water"))
  #03 - Test that the mean column values is between 0 and 1 based on the raster values
  expect_true(all(test_dt$water[!is.na(test_dt$water)] >= 0 &
                     test_dt$water[!is.na(test_dt$water)] <= 100),
               info = "Values of landcover should be between 0 and 100")
})
