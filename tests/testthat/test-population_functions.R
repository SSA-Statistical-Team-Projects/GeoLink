###############################################################################
###Test for mach functions: Population

################################################################################
#1- Read in the data
################################################################################
#Shapefile
data("shp_dt")
#Survey
data("hhgeo_dt")
################################################################################
#2- Begin testings the function when using a shapefile and a raster #############
#################

test_dt <- geolink_population(time_unit= "annual",
                   start_year=2018,
                   end_year = 2019,
                   iso_code = "NGA",
                   const_UNadj_2020 = "Y",
                   shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                   grid_size = 1000)


geolink_population()


