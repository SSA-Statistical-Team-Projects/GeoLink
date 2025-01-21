# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(GeoLink)

test_check("GeoLink")

#Functions that passed tests
#
#usethis::use_test("chirps_functions") #Done
# usethis::use_test("postdownload_processor") #Done
# usethis::use_test("elevation_functions") #Done
# usethis::use_test("cropland_function")  # Done
# usethis::use_test("geolink_worldclim")  #
# usethis::use_test("ntl_functions") # Done
# usethis::use_test("population_functions") # Done
# usethis::use_test("building_functions") # Done
#usethis::use_test("geolink_CMIP6") # Done
# usethis::use_test("geolink_terraclimate") # Done
#usethis::use_test("geolink_landcover") # Needs enhancement

# 6 functions not working
#usethis::use_test("geolink_electaccess") # Not working
#usethis::use_test("geolink_vegindex") # Not working
#usethis::use_test("geolink_pollution") # Not working
#usethis::use_test("geolink_landcover") # Not working
#usethis::use_test("geolink_get_poi") # Not working
#usethis::use_test("geolink_opencellid") # Not working

