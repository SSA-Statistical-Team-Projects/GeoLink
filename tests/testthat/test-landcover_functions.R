###############################################################################
###Test for mach functions: Landcover

################################################################################
#1- Read in the data
################################################################################
#Shapefile
data("shp_dt")
#Survey
data("hhgeo_dt")
################################################################################
#2- Begin testingss the function when using a shapefile and a raster #############
#################

##TEST IN 002 SERVER


df <- geolink_landcover(time_unit,
                        start_date = "2020-01-01",
                        end_date = "2020-03-01",
                        shp_dt = shp_dt)
