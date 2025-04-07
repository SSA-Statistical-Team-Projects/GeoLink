# Test A
test_that("OpenCellID works with shapefile:", {
  suppressWarnings({ test_dt <- geolink_opencellid(cell_tower_file = "testdata/opencellidNGA.csv.gz",
                                                   shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                   grid_size = 1000)

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("cell_towers"))

  #02 - expect the number of rows in the test_dt is equal to the number of rows in the shp_dt
  expect_equal(nrow(test_dt), nrow(shp_dt[shp_dt$ADM1_EN == "Abia",]))

  #03 - expect the number of cell_towers in the test_dt to be positive and less than 2828
  expect_true(all(test_dt$cell_towers > 0) & all(test_dt$cell_towers < 2828),
              info = "Number of cell_towers should be positive and less than 2828")

})

#Test- B
test_that("OpenCellID works with survey:", {
  suppressWarnings({ test_dt <- geolink_opencellid(cell_tower_file = "testdata/opencellidNGA.csv.gz",
                                                   survey_dt =  st_as_sf(hhgeo_dt[1:10],
                                                                        crs = 4326),
                                                   buffer_size = 1000)
  })

  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("cell_towers"))

  #02 - expect the number of rows in the test_dt is equal to the number of rows in the shp_dt
  expect_equal(nrow(test_dt), nrow(hhgeo_dt[1:10]))

})
