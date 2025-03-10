% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geolink_mach.R
\name{geolink_cropland}
\alias{geolink_cropland}
\title{Download cropland data}
\usage{
geolink_cropland(
  source = "WorldCover",
  shp_dt = NULL,
  shp_fn = NULL,
  grid_size = NULL,
  survey_dt = NULL,
  survey_fn = NULL,
  survey_lat = NULL,
  survey_lon = NULL,
  buffer_size = NULL,
  extract_fun = "mean",
  survey_crs = 4326
)
}
\arguments{
\item{source}{A character, the source of cropland data. Default is "WorldCover".}

\item{shp_dt}{An object of class 'sf', 'data.frame' which contains polygons or multipolygons representing the study area.}

\item{shp_fn}{A character, file path for the shapefile (.shp) to be read (for STATA users only).}

\item{grid_size}{A numeric, the grid size to be used in meters for analyzing the cropland data.}

\item{survey_dt}{An object of class "sf", "data.frame", a geocoded household survey with latitude and longitude values (optional).}

\item{survey_fn}{A character, file path for geocoded survey (.dta format) (for STATA users only & if use_survey is TRUE) (optional).}

\item{survey_lat}{A character, latitude variable from survey (for STATA users only & if use_survey is TRUE) (optional).}

\item{survey_lon}{A character, longitude variable from survey (for STATA users only & if use survey is TRUE) (optional).}

\item{buffer_size}{A numeric, the buffer size to be used around each point in the survey data, in meters (optional).}

\item{extract_fun}{A character, a function to be applied in extraction of raster into the shapefile.
Default is "mean". Other options are "sum", "min", "max", "sd", "skew" and "rms" (optional).}

\item{survey_crs}{An integer, the Coordinate Reference System (CRS) for the survey data. Default is 4326 (WGS84) (optional).}
}
\value{
A processed data frame or object based on the input parameters and downloaded data.
}
\description{
This function downloads cropland data from a specified source, such as WorldCover. It allows for further analysis of cropland distribution in a given area.
}
\examples{
\donttest{

 #example usage
df <- geolink_cropland(shp_dt = shp_dt[shp_dt$ADM1_EN
                                ==  "Abia",],
                grid_size = 1000,
                extract_fun = "mean")
}
}
