# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(GeoLink)

#test_check("GeoLink")

#Done
usethis::use_test("chirps_functions") #Done
#usethis::use_test("postdownload_processor") #Done
usethis::use_test("elevation_functions")  #Server is down
usethis::use_test("cropland_function")  #  Server is down, back in Dec 21st
usethis::use_test("geolink_worldclim")  # Server is down
#usethis::use_test("ntl_functions") # Done
#usethis::use_test("population_functions") # Done
#usethis::use_test("building_functions") # Done
#usethis::use_test("geolink_CMIP6") # Done
#usethis::use_test("geolink_landcover") # Done

usethis::use_test("geolink_vegindex") # Done

