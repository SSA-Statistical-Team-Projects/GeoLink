# Test A
test_that("Get POI works using a shapefile:", {

  suppressWarnings({ test_dt <- geolink_get_poi( osm_key = "amenity",
                                                      shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                      grid_size = 1000)

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("osm_id", "name", "amenity",
                                       "addr:city"))

})

#Test- B
test_that("Get POI works using a survey:", {
  suppressWarnings({ test_dt <- geolink_get_poi( osm_key = "amenity",
                                                      survey_dt =  st_as_sf(hhgeo_dt[1:10],
                                                                           crs = 4326),
                                                      buffer_size = 5000)
  })

  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("osm_id", "name", "amenity",
                                       "addr:city"))
})

#Test- C
test_that("Get POI works using a survey stata users:", {
  suppressWarnings({ test_dt <- geolink_get_poi( osm_key = "amenity",
                                                      survey_fn = "testdata/xy_hhgeo_dt.dta",
                                                      survey_lat = "y",
                                                      survey_lon = "x",
                                                      buffer_size = 2000)
  })

  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("osm_id", "name", "amenity",
                                       "addr:city"))
})
