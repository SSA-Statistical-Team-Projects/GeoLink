

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


### run daylan style example
geolink_ntl(start_date = "2020-01-01",
            end_date = "2020-03-01",
            time_unit = "month",
            annual_version = "v21",
            grid_size = 1000,
            indicator = "average_masked",
            shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
            survey_dt = st_as_sf(hhgeo_dt[ADM1_EN == "Abia",],
                                 crs = 4326))



### testing the post download processor functions

pop_raster <- raster("data-raw/nga_ppp_2020_UNadj_constrained.tif")


###### grid the shapefile, extract population and spatial join to survey points
postdownload_processor(shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                       raster_objs = list(pop_raster),
                       grid_size = 1000,
                       survey_dt =  st_as_sf(hhgeo_dt[ADM1_EN == "Abia",],
                                             crs = 4326),
                       name_set = "pop_2020")

geolink_chirps(time_unit = "month",
               start_date = "2020-01-01",
               end_date = "2020-03-01",
               shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
               grid_size = 1000,
               survey_dt = hhgeo_dt,
               extract_fun = "mean")

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


#### pretend to be a stata user for a second
###### grid the shapefile, extract population and spatial join to survey points
postdownload_processor(shp_fn = "D:/Ify/geolink_testshp/abia.shp",
                       raster_objs = list(pop_raster),
                       grid_size = 1000,
                       survey_fn = "data-raw/nga_househldgeovars_y4.dta",
                       survey_lat = "lat_dd_mod",
                       survey_lon = "lon_dd_mod",
                       name_set = "pop_2020")

###### extract into shapefile directly and assign to survey without gridding
postdownload_processor(shp_fn = "D:/Ify/geolink_testshp/abia.shp",
                       raster_objs = list(pop_raster),
                       grid_size = NULL,
                       survey_fn = "data-raw/nga_househldgeovars_y4.dta",
                       survey_lat = "lat_dd_mod",
                       survey_lon = "lon_dd_mod",
                       name_set = "pop_2020")


###### extract into buffered survey
postdownload_processor(raster_objs = list(pop_raster),
                       survey_fn = "data-raw/nga_househldgeovars_y4.dta",
                       survey_lat = "lat_dd_mod",
                       survey_lon = "lon_dd_mod",
                       name_set = "pop_2020",
                       buffer_size = 100)



##### just extract into shapefile both tesselated and untesselated
postdownload_processor(raster_objs = list(pop_raster),
                       grid_size = NULL,
                       shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                       name_set = "pop_2020")

postdownload_processor(raster_objs = list(pop_raster),
                       grid_size = 1000,
                       shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
                       name_set = "pop_2020")







