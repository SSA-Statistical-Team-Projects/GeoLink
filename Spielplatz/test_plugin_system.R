

### some scripts to test function

## get NTL for buffered survey

geolink_ntl(start_date = "2020-01-01",
            end_date = "2020-12-31",
            annual_version = "v21",
            indicator = "average_masked",
            survey_dt = st_as_sf(hhgeo_dt[ADM1_EN == "Abia",],
                                 crs = 4326),
            buffer_size = 100,
            shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])

geolink_ntl(start_date = "2020-01-01",
            end_date = "2020-12-31",
            annual_version = "v21",
            indicator = "average_masked",
            survey_dt = st_as_sf(hhgeo_dt[ADM1_EN == "Abia",],
                                 crs = 4326),
            grid_size = 1000,
            shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])


## get NTL for survey with gridded estimates

geolink_ntl(start_date = "2020-01-01",
            end_date = "2020-02-31",
            time_unit = "month",
            version = "v21",
            indicator = "average_masked",
            survey_dt = st_as_sf(hhgeo_dt[ADM1_EN == "Abia",],
                                 crs = 4326),
            shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",])



### testing the post download processor functions

pop_raster <- raster("data-raw/nga_ppp_2020_UNadj_constrained.tif")


###### grid the shapefile, extract population and spatial join to survey points
postdownload_processor(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                       raster_objs = list(pop_raster),
                       grid_size = 1000,
                       survey_dt =  st_as_sf(hhgeo_dt[ADM1_EN == "Abia",],
                                             crs = 4326),
                       name_set = "pop_2020")

###### extract into shapefile directly and assign to survey without gridding
postdownload_processor(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                       raster_objs = list(pop_raster),
                       grid_size = NULL,
                       survey_dt =  st_as_sf(hhgeo_dt[ADM1_EN == "Abia",],
                                             crs = 4326),
                       name_set = "pop_2020")

###### buffer the survey points instead and extract into the new polygons
postdownload_processor(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                       raster_objs = list(pop_raster),
                       grid_size = NULL,
                       survey_dt =  st_as_sf(hhgeo_dt[ADM1_EN == "Abia",],
                                             crs = 4326),
                       name_set = "pop_2020",
                       buffer_size = 1000)



















