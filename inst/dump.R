## Includes Code that is not part of the function ntl-geoget.R

###############################
## testing masterrf function
masterrfoutput <- masterrf(
  usergrid = "mwi_ppp_2020_UNadj_constrained.tif",
  adminunit = "mwi_admbnda_adm3_nso_20181016.shp",
  admin0 = NULL,
  year = "2019",
  month = "10",
  day = "01",
  viirs = FALSE,
  chirps = TRUE,
  username = "ctl1m14@soton.ac.uk",
  password = "password",
  version = "v10",
  no_tile = TRUE,
  slc_type = "vcmcfg",
  indicator = "avg_rade9h",
  noPopdf = FALSE
)

###############################
popgrid <- rast("mwi_ppp_2020_UNadj_constrained.tif")
# calculate stats dataframe for mwi_ppp_2020_UNadj_constrained pop per admin unit (for test purposes)
mwi_ppp_2020_UNadj_constrainedzonalstatistics <- exact_extract(popgrid, adminunitsf, c("min", "max", "count", "sum", "mean", "median", "stdev"), append_cols = c("layer"), full_colnames = TRUE, progress = TRUE)


###############################
### CREATE SURVEY SPATVECTOR (For test purposes. Ultimately the survey data should be supplied by the user as an R object, formatted appropriately)

# input and join survery data
survey <- read_dta("IHS_allmodules.dta")
survey2 <- read_dta("geovars.dta") ## lookuptable (smallest)

joined_survey <- merge(survey, survey2,
                       by.x = "hid", by.y = "hid",
                       all.x = TRUE, all.y = TRUE
)

joined_survey2 <- data.table(joined_survey)

# remove rows that have lat long NAs
joined_survey2 <- na.omit(joined_survey2, cols = c("lat_modified", "lon_modified"))

# create long lat point geometries from joined survey dataframe
surveypoints <- vect(joined_survey2, geom = c("lon_modified", "lat_modified"), crs = "+proj=longlat +datum=WGS84", keepgeom = TRUE)

# use terra to extract admin unit values from raster for each survey point, then add each value to the respective point attributes
surveyextraction <- extract(adminunitraster, surveypoints, ID = TRUE, xy = TRUE, cells = TRUE, method = "simple", na.rm = TRUE, bind = TRUE)
surveyextraction <- as.data.table(surveyextraction)

### remove survey points that have no adm3 id (i.e. are NAs values) because the points are located outside of the country boundary
## surveydata <- na.omit(surveyextraction, cols="layer")

# create extra column in survey data that expresses whether each survey point is located within an admin unit (a value of 3 indicates yes, 1 indicates no)
surveydata <- surveyextraction %>%
  dplyr::mutate(merge_result = dplyr::case_when(
    layer >= 0 ~ 3,
    is.na(layer) ~ 1
  ))

surveydata <<- vect(surveydata, geom = c("lon_modified", "lat_modified"), crs = "+proj=longlat +datum=WGS84", keepgeom = TRUE)
# writeVector(surveydata, "surveydata.shp", filetype=NULL, layer=NULL, insert=FALSE, overwrite=FALSE, options="ENCODING=UTF-8")


###############################
### CREATE POPULATION DATAFRAME

# create character vector of all zonal stats object names and then call the objects
charvectdf <- mget(apropos("zonalstatistics", ignore.case = FALSE))
# Join these multiple data.frames
popdf <- charvectdf %>% purrr::reduce(inner_join, by = "layer")

###############################
output <- geomerge(usergrid, adminunit, popdf, surveydata)
outputsampleonly <- data.frame(output)
outputsample <- data.frame(output[1])
outputpopdf <- data.frame(output[2])


noPopdf <<- TRUE

