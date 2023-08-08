

geolink_chirps <- function(time_unit,
                           shp_dt,
                           grid = FALSE,
                           grid_size = 1000,
                           use_survey = TRUE,
                           survey_dt,
                           extract_fun = "mean") {


  ## download the data via readlines seeking inputs
  if (time_unit == "month") {

    {
      start_date = readline("Enter the start date (yyyy-mm-dd): ");
      end_date = readline("Enter the end date (yyyy-mm-dd): ");
    }

    raster_objs <- get_month_chirps(start_date = as.Date(start_date),
                                    end_date = as.Date(end_date))

    name_count <- lubridate::interval(as.Date(start_date),
                                      as.Date(end_date)) %/% months(1) + 1



  } else {

    {
      start_year = readline("Enter the start year (yyyy): ");
      end_year = readline("Enter the end year (yyyy): ");
    }

    raster_objs <- get_annual_chirps(start_year = as.integer(start_year),
                                     end_year = as.integer(end_year))

    name_count <- lubridate::year(as.Date(start_date)) - lubridate::year(as.Date(end_date)) + 1

  }

  ## crop the rasters to size of area
  raster_objs <- lapply(raster_objs,
                        function(X) {

                          raster_crop <- raster::crop(x = X,
                                                      y = extent(shp_dt))

                          return(raster_crop)

                        })

  ## create the name for the variables

  name_set <- paste0(time_unit, 1:name_count)


  if (grid == TRUE) {



    #### check if the shapefile is a UTM or degree CRS projection
    crs_obj <- st_crs(shp_dt)

    if (!("m" %in% crs_obj$units)) {

      suggest_dt <- crsuggest::suggest_crs(shp_dt,
                                           units = "m")

      shp_dt <- st_transform(shp_dt,
                             crs = as.numeric(suggest_dt$crs_code[1]))

    }

    shp_dt <- gengrid2(shp_dt = shp_dt,
                       grid_size = grid_size)

    shp_dt <- st_transform(shp_dt, crs = crs_obj)

  }

  ## extract into the shapefile with different column names

  ### reproject shapefile to match raster CRS if they are not the same

  shp_dt <-
    mapply(FUN = function(x, n){
      ### first ensure raster and shapefile have the same crs

      if (!identical(crs_obj, raster::crs(x))){

        shp_dt <- st_transform(shp_dt, crs = st_crs(x)$input)

      }

      shp_dt[[n]] <- exactextractr::exact_extract(x = x,
                                                  y = shp_dt,
                                                  fun = extract_fun)

      return(shp_dt)

  },
  SIMPLIFY = FALSE,
  x = raster_objs,
  n = name_set)

  if (length(shp_dt) > 1L) {

    shp_dt <- lapply(shp_dt,
                     function(X){

                       X$geoID <- 1:nrow(X)

                       return(X)

                     })

    geoid_dt <- shp_dt[[1]][, c("geoID")]

    shp_crs <- st_crs(shp_dt[[1]])$input

    shp_dt <- lapply(shp_dt,
                     function(X){

                       X <- X %>% st_drop_geometry()

                       return(X)

                     })

    shp_dt <- Reduce(merge, shp_dt)

    shp_dt <- merge(shp_dt, geoid_dt, by = "geoID")

    shp_dt <- st_as_sf(shp_dt, crs = shp_crs, agr = "constant")

  }


  ## merge the shapefile and the household survey
  if (use_survey == TRUE){

    crs_raster <- crs(raster_objs[[1]])

    survey_dt <- st_transform(x = survey_dt,
                              crs = crs_raster)

    survey_dt <- st_join(survey_dt, shp_dt)

    return(survey_dt)

  }

  return(shp_dt)

}



















