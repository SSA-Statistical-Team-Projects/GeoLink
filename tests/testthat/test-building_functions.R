###############################################################################
###Test for building footprints
################################################################################


#Test- A.
test_that("Buildings works using a shapefile:", {

  suppressWarnings({ test_dt <- geolink_buildings(version = "v1.1",
                                                  iso_code = "NGA",
                                                  shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                  indicators = "ALL",
                                                  grid_size = 1000)

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("count","cv_length",
                                       "density" ,"mean_area",
                                       "mean_length",  "total_area",
                                       "total_length", "urban" ))

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))

  #03 - Test col values are within the raster values
  urban <- na.omit(test_dt$urban)
  expect_true(all(urban >= 0 & urban <= 1),
              info = "Values of urban should be between 0 and 1")
  total_length <- na.omit(test_dt$total_length)
  expect_true(all(total_length >= 2.768753 & total_length <= 6596.204),
              info = "Values of urban should be between 2.768753 and 6596.204")
  total_area <- na.omit(test_dt$total_area)
  expect_true(all(total_area >= 0.04377888 & total_area <= 489605.3),
              info = "Values of urban should be between 0.04377888 and 489605.3")
}
)


#Test- B
test_that("Buildings works using a survey :", {

  suppressWarnings({ test_dt <- geolink_buildings(version = "v1.1",
                                                  iso_code = "NGA",
                                                  survey_dt =  st_as_sf(hhgeo_dt[1:10],
                                                                        crs = 4326),
                                                   indicators = "ALL",
                                                   buffer_size = 1000,
                                                   extract_fun = "mean" )

  })

  #Write testing expressions below:
  #01 - expect the colnames  are created correctly
 expect_contains(colnames(test_dt), c("count","cv_length",
                                     "density" ,"mean_area",
                                     "mean_length",  "total_area",
                                     "total_length", "urban" ))

  #02 - expect the length of test_dt be the same as the survey
  expect_equal(length(test_dt$hhid), length(hhgeo_dt$hhid[1:10]))

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(test_dt[1,]) / pi))), 1000)


#Test that the columns are within the raster values
  urban <- na.omit(test_dt$urban)
  expect_true(all(urban >= 0 & urban <= 1),
              info = "Values of urban should be between 0 and 1")
  total_length <- na.omit(test_dt$total_length)
  expect_true(all(total_length >= 2.768753 & total_length <= 6596.204),
              info = "Values of urban should be between 2.768753 and 6596.204")
  total_area <- na.omit(test_dt$total_area)
  expect_true(all(total_area >= 0.04377888 & total_area <= 489605.3),
              info = "Values of urban should be between 0.04377888 and 489605.3")

}
)


#Test- C.
test_that("Buildings works with one indicator:", {

  suppressWarnings({ test_dt <- geolink_buildings(version = "v1.1",
                                                  iso_code = "NGA",
                                                  shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                  indicators = "urban",
                                                  grid_size = 1000)

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("urban" ))

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))

  #03 - Test col values are within the raster values
  urban <- na.omit(test_dt$urban)
  expect_true(all(urban >= 0 & urban <= 1),
              info = "Values of urban should be between 0 and 1")

})



#Test- C.
test_that("Buildings works with two indicator:", {

  suppressWarnings({ test_dt <- geolink_buildings(version = "v1.1",
                                                  iso_code = "NGA",
                                                  shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
                                                  indicators = c("urban", "count"),
                                                  grid_size = 1000)

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("urban", "count"))

  #02- Test that the object was properly tessellated
  expect_equal(length(unique(test_dt$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt =
                                   st_transform(shp_dt[shp_dt$ADM1_EN == "Abia",],
                                                crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))

  #03 - Test col values are within the raster values
  urban <- na.omit(test_dt$urban)
  expect_true(all(urban >= 0 & urban <= 1),
              info = "Values of urban should be between 0 and 1")

})

