###############################################################################
###Test for opencell()
################################################################################
#1- Read in the data
################################################################################
#Shapefile
data("shp_dt")
#Survey
data("hhgeo_dt")

test_dt <- geolink_opencellid(shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG025",],
                   grid_size = 1000,
                   extract_fun = "mean")
