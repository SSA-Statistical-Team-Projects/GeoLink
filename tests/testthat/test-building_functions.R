###############################################################################
###Test for post_downloadprocessor()
################################################################################
#1- Read in the data
################################################################################
#Shapefile
data("shp_dt")
#Survey
data("hhgeo_dt")

###Function


#Test- A.
test_that("geolink_buildings  works using a shapefile:", {

  suppressWarnings({test_dt <- geolink_buildings(version = "v1.1",
                                                 iso_code = "NGA",
                                                 # shp_dt = shp_dt[shp_dt$ADM1_EN == "Federal Capital Territory",]
                                                 shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",])

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })

  #Write testing expressions below:
  #01 - expect the colnames will be created correctly
  expect_contains(colnames(test_dt), "buildings_")

  #02- Columns generated are positive numbers
  test_that("All columns starting with 'buildings_' have values > 0", {
    # Exclude the geometry column and get columns that start with 'buildings_'
    buildings_cols <- test_dt %>%
      st_set_geometry(NULL) %>%  # Remove geometry to focus on attributes
      select(starts_with("buildings_"))

    # Check that all values in these columns are > 0
    all_positive <- map_lgl(buildings_cols, ~ all(.x > 0))

    expect_true(all(all_positive), info = "Not all 'buildings_' columns have all values > 0")
  })


  #03 - Test that the mean column values is between 0 and 1444.34

  expect_true(all(test_dt$rainfall_month1 >= 0 & test_dt$rainfall_month1 <= 1444.34),
              info = "Values of rainfall_month1 should be between 0 and 1444.34")

}
)




test_dt <- geolink_buildings(version = "v1.1",
                  iso_code = "NGA",
                  survey_dt = st_as_sf(hhgeo_dt[1:50],
                                       crs = 4326),
                  shp_dt = NULL,
                  buffer_size = 1000)

