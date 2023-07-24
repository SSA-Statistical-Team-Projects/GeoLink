#' @author Christopher T Lloyd <ctl1m14@soton.ac.uk>
#'
#' @param usergrid Raster usergrid input filename (*.tif) at resolution at which analysis is to be undertaken, and extent for country in question. If NULL filename is provided then a usergrid is created to the user's spec.
#' @param adminunit Administrative Unit input filename (*.shp). The admin unit attribute column should not contain non-numeric characters. If NULL filename is provided then a pixel level analysis is assumed and an appropriate grid generated
#' @param admin0 Administrative Unit Level 0 or similar input filename (*.shp) denoting country extent. This input is only required if one of the above filenames are not provided
#' @param TRUE/FALSE Boolean value indicating whether Nighttime Lights data should be downloaded
#' @param TRUE/FALSE Boolean value indicating whether CHIRPS rainfall data should be downloaded
##' @param username Nighttime Lights user account username (not yet implemented)
##' @param password Nighttime Lights user account password (not yet implemented)
#' @param year Choose year for data download
#' @param month Choose month for data download
###' @param day Choose day for data download (only implemented to facilitate download using current download functions which are a work in progress)
##' @param version Nighttime Lights download selection parameter (not yet implemented)
##' @param no_tile Boolean value indicating whether Nighttime Lights data should be downloaded at global extent (not yet implemented)
##' @param slc_type Nighttime Lights download selection parameter (not yet implemented)
##' @param indicator Nighttime Lights download selection parameter (not yet implemented)
#' @param hs Household survey R object name (not implemented as using test data)
#' @param noPopdf Boolean value indicating that no population dataframe should be output

#' @importFrom


### VIIRS Processing Function
viirsfunction <- function(usergrid, adminunit, viirs) {
  # standardise to usergrid (pixel resolution and alignment)
  viirsres <- resample(viirs, usergrid, method = "near", threads = TRUE, NAflag = 3.40282346639e+038, datatype = "FLT4S")
  ## viirsresnd <- subst(viirsres, 0, 3.40282346639e+038)
  writeRaster(viirsres, threads = TRUE, NAflag = 3.40282346639e+038, overwrite = TRUE, filename = "viirsres.tif", datatype = "FLT4S", gdal = c("COMPRESS=LZW", "TFW=YES", "OVR=YES"))
  viirsres <- raster::raster(viirsres)
  ## writeRaster(viirsres, threads=TRUE, NAflag=3.40282346639e+038, overwrite=TRUE, filename="viirsres.tif", datatype="FLT4S", options=c("COMPRESS=NONE", "TFW=YES", "OVR=YES"))
  # calculate stats dataframe for viirs per admin unit
  viirszonalstatistics <- parallel_zonalstats(viirsres, adminunitsf, fun, append_cols, full_colnames, numCores)
}

### CHIRPS Processing
chirpsfunction <- function(usergrid, adminunit, chirps) {
  # standardise to usergrid (pixel resolution and alignment)
  chirpsres <- resample(chirps, usergrid, method = "near", threads = TRUE, NAflag = 3.40282346639e+038, datatype = "FLT4S")
  chirpsresnd <- subst(chirpsres, -9999, 0)
  writeRaster(chirpsresnd, threads = TRUE, NAflag = 3.40282346639e+038, overwrite = TRUE, filename = "chirpsresnd.tif", datatype = "FLT4S", gdal = c("COMPRESS=LZW", "TFW=YES", "OVR=YES"))
  chirpsresnd <- raster::raster(chirpsresnd)
  # calculate stats dataframe for viirs per admin unit
  chirpszonalstatistics <- parallel_zonalstats(chirpsresnd, adminunitsf, fun, append_cols, full_colnames, numCores)
}




### Master Routing Function
masterrf <- function(usergrid, adminunit, admin0, year, month, day, viirs, chirps, username, password, version, no_tile, slc_type, indicator, noPopdf) {
  # enter user variables
  usergrid <- usergrid
  adminunit <- adminunit
  admin0 <- admin0
  year <- year
  month <- month
  day <- day
  viirs <- viirs
  chirps <- chirps
  username <- username
  password <- password
  version <- version
  no_tile <- no_tile
  slc_type <- slc_type
  indicator <- indicator
  ## hs <<- hs
  ##noPopdf <<- noPopdf


  ### CREATE GRID
  # Check if raster usergrid exists for country. If it doesn't then create one to user spec.
  if (!is.null(usergrid)) {
    usergrid <- terra::rast(usergrid)
    terra::crs(usergrid) <- "epsg:4326"
    usergrid <<- usergrid
    print("usergrid exists")
  } else {
    usergridres <- svDialogs::dlg_input("Please enter required usergrid resolution in decimal degrees", Sys.info()["usergridres"])$res ## e.g. 0.00083333333
    usergridres <- as.numeric(c(usergridres, usergridres))
    usergrid <- terra::rast(resolution = usergridres, val = 0, crs = "+proj=longlat +datum=WGS84")
    # Crop to country boundaries
    admin0 <- vect(admin0, crs = "+proj=longlat +datum=WGS84")
    usergrid <- crop(usergrid, ext(admin0))
    usergrid <<- usergrid
    writeRaster(usergrid, threads = TRUE, filename = "usergrid.tif", overwrite = TRUE, gdal = c("COMPRESS=LZW", "TFW=YES", "OVR=YES"))
  }

  ### Check if vector admin unit file exists for country. If it doesn't then create grid suitable for pixel level analysis
  if (!is.null(adminunit)) {
    adminunit <- terra::vect(adminunit)
    terra::crs(adminunit) <- "epsg:4326"
    print("admin unit file exists")
    ### USER sets country admin unit ID column name which is then converted to workflow standard name 'layer'
    admincoln <- dlg_input("Set administrative unit ID column name from file (e.g. 'adm3')", Sys.info()["admincoln"])$res
    adminunit <- tidyterra::rename(adminunit, layer = all_of(admincoln))
    ## use terra to rasterize admin unit to usergrid (i.e. pixel resolution and alignment)
    adminunitraster <- terra::rasterize(adminunit, usergrid, field = "layer", filename = "adminunit.tif", overwrite = TRUE, NAflag = 3.40282346639e+038, datatype = "FLT4S")
    # convert spatvector admin unit layer to sf
    adminunitsf <- sf::st_as_sf(adminunit)
    # merge gridded admin units to create gridded admin0 layer, then rasterize to use as mask/base layer in subsequent calculations
    admin0 <- aggregate(adminunit, dissolve = TRUE)
    # writeVector(admin0, "admin0.shp", filetype=NULL, layer=NULL, insert=FALSE, overwrite=TRUE, options="ENCODING=UTF-8")
    admin0raster <- terra::rasterize(admin0, adminunitraster, value = 1, filename = "admin0raster.tif", overwrite = TRUE, NAflag = 3.40282346639e+038, datatype = "FLT4S")
    adminunit <<- adminunit
    adminunitsf <<- adminunitsf
    adminunitraster <<- adminunitraster
  } else {
    if (!is.null(admin0)) {
      # rasterize L0 file for country boundaries to use as mask/base layer in subsequent calculations
      admin0raster <- terra::rasterize(admin0, usergrid, value = 1, filename = "admin0raster.tif", overwrite = TRUE, NAflag = 3.40282346639e+038, datatype = "FLT4S")
      ## use terra to rasterize admin unit to usergrid (i.e. pixel resolution and alignment)
      adminunitraster <- admin0raster
      values(adminunitraster) <- 1:ncell(adminunitraster)
      adminunitraster <- mask(adminunitraster, admin0raster, maskvalue = 1, inverse = TRUE)
      writeRaster(adminunitraster, names = "layer", threads = TRUE, filename = "adminunit.tif", overwrite = TRUE, gdal = c("COMPRESS=LZW", "TFW=YES", "OVR=YES"))
      adminunit <- as.polygons(adminunitraster, values = TRUE, na.rm = TRUE, dissolve = FALSE)
      writeVector(adminunit, "adminunit.shp", overwrite = TRUE, options = "ENCODING=UTF-8")
      # convert spatvector admin unit layer to sf
      adminunitsf <- sf::st_as_sf(adminunit)
      adminunit <<- adminunit
      adminunitsf <<- adminunitsf
      adminunitraster <<- adminunitraster
    } else {
      print("user needs to specify country boundary shapefile in function!")
      ## input L0 file for country boundaries, then rasterize to use as mask/base layer in subsequent calculations
      ## admin0 <<- vect(admin0, crs="+proj=longlat +datum=WGS84")
      ## admin0raster <<- rasterize(admin0, usergrid, value=1, filename="admin0raster.tif", overwrite=TRUE, NAflag=3.40282346639e+038, datatype="FLT4S")
      ## use terra to rasterize admin unit to usergrid (i.e. pixel resolution and alignment)
      ## adminunitraster <<- admin0raster
      ## values(adminunitraster) <- 1:ncell(adminunitraster)
      ## adminunitraster <- mask(adminunitraster, admin0raster, maskvalue=1, inverse=TRUE)
      ### tidyterra::rename(adminunitraster, adm3=layer)
      ## writeRaster(adminunitraster, names="layer", threads=TRUE, filename="adminunit.tif", overwrite=TRUE, gdal=c("COMPRESS=LZW", "TFW=YES", "OVR=YES"))
      ## adminunit <<- as.polygons(adminunitraster, values=TRUE, na.rm=TRUE, dissolve=FALSE)
      ### tidyterra::rename(adminunit, adm3=layer)
      ### adminunit[["adm3"]] <- 1:nrow(adminunit)
      ## writeVector(adminunit, "adminunit.shp", overwrite=TRUE, options="ENCODING=UTF-8")

      ### use terra to rasterize admin unit to usergrid (i.e. pixel resolution and alignment)
      #### adminunitraster <<- rasterize(adminunit, usergrid, field="adm3", filename="adminunit.tif", overwrite=TRUE)
      ## convert spatvector admin unit layer to sf
      ## adminunitsf <<- sf::st_as_sf(adminunit)
    }
  }



  ## More work to do integrating VIIRS download function
  if (viirs == FALSE) {
    print("no viirs selected...continuing...")
  } else {
    viirstemporalres <- svDialogs::dlg_input("Please enter required viirs temporal resolution (e.g.monthly, yearly)", Sys.info()["viirstemporalres"])$res ## e.g. monthly or yearly
    if (any(viirstemporalres == "monthly")) {
      viirs <- rast("get_month_ntl(username, password, year, month, version, no_tile, slc_type = c(slc_type), indicator = c(indicator), link_base = 'https://eogdata.mines.edu/nighttime_light', cores = 1L)")
      viirszonalstatistics <<- viirsfunction(usergrid, adminunit, viirs)
    } else {
      viirs <- rast("get_annual_ntl(username, password, year, version, link_base = 'https://eogdata.mines.edu/nighttime_light', indicator = c(indicator), cores = 1L)")
      viirszonalstatistics <<- viirsfunction(usergrid, adminunit, viirs)
    }
    return(viirszonalstatistics)
  }

  ## Download CHIRPS rainfall data for one month or one year
  if (chirps == FALSE) {
    print("no chirps selected...continuing...")
  } else {
    chirpstemporalres <- svDialogs::dlg_input("Please enter required chirps temporal resolution (e.g.monthly, yearly)", Sys.info()["viirstemporalres"])$res ## e.g. monthly or yearly
    if (any(chirpstemporalres == "monthly")) {
      ### year <- "2020"
      ### month <- "11"
      ### day <- "01"
      startdate <- c(year, month, day)
      startdate <- paste(startdate, collapse = "")
      startdate <- lubridate::ymd(startdate)
      enddate <- c(year, month, day)
      enddate <- paste(enddate, collapse = "")
      enddate <- lubridate::ymd(enddate)
      try(get_month_chirps(startdate, enddate, link_base = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/", dsn = getwd(), cores = 1L))
      chirpsfilename <- chirpname_monthly(startdate, enddate)
      chirpsfilename <- chirpsfilename$filename
      chirpsfilename <- substr(chirpsfilename, 0, nchar(chirpsfilename) - 3)
      print(chirpsfilename)
      chirps <- rast(chirpsfilename)
      chirpszonalstatistics <<- chirpsfunction(usergrid, adminunit, chirps)
    } else {
      try(get_annual_chirps(year, year, link_base = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/", dsn = getwd(), cores = 1L))
      chirpsfilename <- chirpname_annual(year, year)
      print(chirpsfilename)
      chirps <- rast(chirpsfilename)
      chirpszonalstatistics <<- chirpsfunction(usergrid, adminunit, chirps)
    }
    return(chirpszonalstatistics)
  }
}

## admin0="mwi_admbnda_adm0_nso_20181016.shp"
## usergrid = "mwi_ppp_2020_UNadj_constrained.tif"
## adminunit = "mwi_admbnda_adm3_nso_20181016.shp"



### Geomerge Function
geomerge <- function(usergrid, adminunit, popdf, hs) {
  ### CREATE SAMPLE SPATVECTOR
  # merge pop dataframe with survey dataframe
  sample <- merge(hs, popdf,
                  by.x = "layer", by.y = "layer",
                  all.x = TRUE, all.y = FALSE
  )

  writeVector(sample, "sample.gpkg", filetype = NULL, layer = NULL, insert = FALSE, overwrite = TRUE, options = "ENCODING=UTF-8")

  returnedList <- list(sample, popdf)

  if (noPopdf == TRUE) {
    return(sample)
  } else {
    return(returnedList)
  }
}

