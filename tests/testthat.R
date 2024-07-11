# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(GeoLink)


#Create test


#Done
usethis::use_test("chirps_functions") #Done
usethis::use_test("postdownload_processor") #Done

#Fixing housekeeping
usethis::use_test("electricity_functions") #Fixing

#Not working or error
usethis::use_test("ntl_functions") # Not working
usethis::use_test("population_functions") #Not working
usethis::use_test("opencell_functions") # Not working
usethis::use_test("elevation_functions") #Does not work with survey
usethis::use_test("cropland_function") # Not working , same issue as elevation

#Not working test in 002
usethis::use_test("landcover_functions") # Test in 002
usethis::use_test("building_functions") #Test in 002

test_check("GeoLink")
covr::report()



