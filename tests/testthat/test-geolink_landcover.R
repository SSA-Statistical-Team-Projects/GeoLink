# Test A-
test_that("Landcover using shapefile: ", {
  suppressWarnings({ test_dt <- geolink_landcover(
                                                  start_date = "2019-01-01",
                                                  end_date = "2019-12-31",
                                                  shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                  use_resampling = TRUE,
                                                  target_resolution = 1000
                                                  )

    suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                         units = "m")

  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), "landcover")

  #03 - Test that the mean column values is between 0 and 1 based on the raster values

  expect_true(all(test_dt$landcover[!is.na(test_dt$landcover)] >= 0 &
                     test_dt$landcover[!is.na(test_dt$landcover)] <= 1),
               info = "Values of landcover should be between 0 and 1")
})
