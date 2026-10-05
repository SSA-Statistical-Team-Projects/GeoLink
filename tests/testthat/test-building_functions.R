###############################################################################
###Test for building footprints
################################################################################


#Test- A.
test_that("Buildings works using a shapefile:", {
  skip_if_not_live()

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
  ## total_length and total_area are cell totals: the sum over the cell's pixels, with pixels
  ## without buildings counted as 0. Expected ranges over the 5171 cells from an independent
  ## computation (pixel coverage by terra::rasterize(cover = TRUE), WorldPop v1.1 rasters):
  ## total_length 0 to 168371.5 m, total_area 0 to 570823.2 m2; 1% tolerance at the maximum
  total_length <- na.omit(test_dt$total_length)
  expect_true(all(total_length >= 0 & total_length <= 168371.5 * 1.01),
              info = "Cell totals of total_length should be between 0 and 168371.5")
  expect_equal(max(total_length), 168371.5, tolerance = 0.01)
  total_area <- na.omit(test_dt$total_area)
  expect_true(all(total_area >= 0 & total_area <= 570823.2 * 1.01),
              info = "Cell totals of total_area should be between 0 and 570823.2")
  expect_equal(max(total_area), 570823.2, tolerance = 0.01)
}
)


#Test- B
test_that("Buildings works using a survey :", {
  skip_if_not_live()

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
  skip_if_not_live()

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
  skip_if_not_live()

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

#Test- D.
test_that("Buildings works using a shapefile:", {
  skip_if_not_live()

  suppressWarnings({
    temp_gamd <- sf::st_as_sf(geodata::gadm("KEN", level = 2, tempdir()))

    test_dt <- geolink_buildings(version = "v1.1",
                                                  iso_code = "KEN",
                                                  shp_dt = temp_gamd[temp_gamd$NAME_1 == "Nairobi",],
                                                  indicators = "ALL")

  suggest_dt <- crsuggest::suggest_crs(temp_gamd,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), c("count","cv_length",
                                       "density" ,"mean_area",
                                       "mean_length",  "total_area",
                                       "total_length", "urban" ))

  #03 - Test col values are within the raster values
  urban <- na.omit(test_dt$urban)
  expect_true(all(urban >= 0 & urban <= 1),
              info = "Values of urban should be between 0 and 1")
  ## total_length and total_area are polygon totals (pixels without buildings count as 0).
  ## Expected ranges over the 17 Nairobi constituencies from an independent computation (pixel
  ## coverage by terra::rasterize(cover = TRUE), WorldPop v1.1 rasters): total_length 369694.9
  ## to 3450940 m, total_area 944011.3 to 8821114 m2; 1% tolerance
  total_length <- na.omit(test_dt$total_length)
  expect_equal(length(total_length), 17)
  expect_equal(range(total_length), c(369694.9, 3450940), tolerance = 0.01)
  total_area <- na.omit(test_dt$total_area)
  expect_equal(length(total_area), 17)
  expect_equal(range(total_area), c(944011.3, 8821114), tolerance = 0.01)
}
)
