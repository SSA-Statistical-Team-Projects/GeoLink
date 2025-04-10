###############################################################################
###Test for post_downloadprocessor()
################################################################################
#1- Read in the data
################################################################################

#One Raster
raster_dt <- raster::raster(test_path("testdata/nga_ppp_2020_UNadj_constrained.tif"))
#A Raster List
raster_list <- lapply(list.files(test_path("testdata"),
                                 pattern ="chirps"), function(x){
                                   y <- raster(paste0(test_path("testdata/", x )))
                                 })
################################################################################
#2- Begin testing the function when using a shapefile and a raster #############
################################################################################

#Test- A
test_that("It returns the correct object structure using a shapefile and a raster", {

  suppressWarnings({result_sf <-postdownload_processor(raster_objs = list(raster_dt),
                                                       extract_fun = "mean",
                                                       shp_dt = shp_dt[1:2,],
                                                       grid_size = 1000,
                                                       name_set="nga_ppp")

  suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                       units = "m")
  })
  #Write testing expressions below:
  #Column name
  expect_contains(colnames(result_sf), "nga_ppp")

  #Returns a numeric column
  expect_equal(class(result_sf$nga_ppp), "numeric")

  #Returns an SF object
  expect_s3_class(result_sf, "sf" )

  #Shape file structure
  expect_equal(result_sf$ADM2_PCODE[1],"NG001002")

  #Test that the object was properly tessellated
  expect_equal(length(unique(result_sf$poly_id)),
               suppressWarnings({
                 length(gengrid2(shp_dt = st_transform(shp_dt[1:2,],
                                                       crs = as.numeric(suggest_dt$crs_code[1])),
                                 grid_size = 1000)$poly_id)}))
}
)

#Test - B
test_that("It returns the correct object structure using a survey and a raster", {

  result_sf <- postdownload_processor(raster_objs = list(raster_dt),
                                     extract_fun = "mean",
                                     st_as_sf(hhgeo_dt[1:10],
                                              crs = 4326),
                                     survey_crs = 4326,
                                     grid_size = NULL,
                                     buffer_size = 1000,
                                     name_set="nga_ppp")

  #Write expectations from the results sf object
  #Col name
  expect_contains(colnames(result_sf), "nga_ppp")

  #Numeric class
  expect_equal(class(result_sf$nga_ppp), "numeric")

  #SF object
  expect_s3_class(result_sf, "sf")

  #Shape file structure
  expect_equal(result_sf$ADM2_PCODE[1],"NG001015")

  #Length
  expect_equal(length(unique(result_sf$hhid)), 10 )

  #Expect the radious of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(result_sf[1,]) / pi))), 1000, tolerance = 5)

})


#Test - C
test_that("It returns the correct object structure using a survey and a raster list", {

  result_sf <-postdownload_processor(raster_objs = raster_list,
                                     extract_fun = "mean",
                                     st_as_sf(hhgeo_dt[1:10],
                                              crs = 4326),
                                     survey_crs = 4326,
                                     grid_size = NULL,
                                     buffer_size = 1000,
                                     name_set = paste0("nga_chirps_",
                                                       1:length(raster_list))
  )

  #Write expectations from the results sf object

  #Expect the nameset to work properly
  expect_equal(names(result_sf)[ncol(result_sf) - 1], "nga_chirps_2")

  #SF object
  expect_s3_class(result_sf, "sf")

  #Length
  expect_equal(length(unique(result_sf$hhid)), 10 )

  #Expect the radios of the buffer to be a 1000 m
  expect_equal(as.numeric(round(sqrt(st_area(result_sf[1,]) / pi))), 1000, tolerance = 5)
})

#Test - D
test_that("It returns the correct object structure using a survey stata users and a raster list", {

  result_sf <-postdownload_processor(raster_objs = raster_list,
                                     extract_fun = "mean",
                                     survey_fn = test_path("testdata/xy_hhgeo_dt.dta"),
                                     survey_lat = "y",
                                     survey_lon = "x",
                                     buffer_size = 1000,
                                     name_set = paste0("nga_chirps_",
                                                       1:length(raster_list)))

  #Write expectations from the results sf object

  #Expect the nameset to work properly
  expect_equal(names(result_sf)[ncol(result_sf) - 1], "nga_chirps_2")

  #SF object
  expect_s3_class(result_sf, "sf")

  #Length
  expect_equal(length(unique(result_sf$hhid)), 5116)

})


#Test - E
test_that("It returns the correct object structure using a shapefile path and a raster list", {


  suppressWarnings(  result_sf <-postdownload_processor(raster_objs = raster_list,
                                                        extract_fun = "mean",
                                                        shp_fn = test_path("testdata/shp_dt.shp"),
                                                        grid_size = 1000,
                                                        name_set = paste0("nga_chirps_",
                                                                          1:length(raster_list)))

                     )

  expect_equal(names(result_sf)[ncol(result_sf) - 1], "nga_chirps_2")

  #SF object
  expect_s3_class(result_sf, "sf")
}
)








