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
usethis::use_test("chirps_functions") #1 Fail Date format error
usethis::use_test("postdownload_processor") #Working
usethis::use_test("landcover_functions") # Test in 002
usethis::use_test("population_functions") #Not working
usethis::use_test("electricity_functions") #Try to understand output
usethis::use_test("opencell_functions") # Not working
usethis::use_test("worldpop_functions") #Not working *

test_check("GeoLink")


covr::report()



