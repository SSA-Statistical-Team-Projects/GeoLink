#' Nigeria GHS 2018/19 Household Geovariables Data
#'
#' This dataset contains geospatial variables for households from the Nigerian General Household Survey 2018/19.
#'
#' @format A `data.table` and `data.frame` with 5116 rows and 42 variables:
#' \describe{
#'   \item{hhid}{Household ID}
#'   \item{dist_road2}{Distance to the nearest road (in km)}
#'   \item{dist_popcenter2}{Distance to the nearest population center (in km)}
#'   \item{dist_market}{Distance to the nearest market (in km)}
#'   \item{dist_border2}{Distance to the nearest border (in km)}
#'   \item{dist_admctr}{Distance to the nearest administrative center (in km)}
#'   \item{af_bio_1}{Bioclimatic variable 1}
#'   \item{af_bio_8}{Bioclimatic variable 8}
#'   \item{af_bio_12}{Bioclimatic variable 12}
#'   \item{af_bio_13}{Bioclimatic variable 13}
#'   \item{af_bio_16}{Bioclimatic variable 16}
#'   \item{popdensity}{Population density (per sq. km)}
#'   \item{hybrid_V8}{Hybrid variable V8}
#'   \item{ssa_aez09}{SSA AEZ 09}
#'   \item{afmnslp_pct}{Mean slope percentage}
#'   \item{srtm_1k}{SRTM elevation data (1 km resolution)}
#'   \item{twi}{Topographic Wetness Index}
#'   \item{sq1}{Soil quality variable 1}
#'   \item{sq2}{Soil quality variable 2}
#'   \item{sq3}{Soil quality variable 3}
#'   \item{sq4}{Soil quality variable 4}
#'   \item{sq5}{Soil quality variable 5}
#'   \item{sq6}{Soil quality variable 6}
#'   \item{sq7}{Soil quality variable 7}
#'   \item{anntot_avg}{Average annual total precipitation}
#'   \item{wetQ_avg}{Average wet quarter precipitation}
#'   \item{wetQ_avgstart}{Average wet quarter start}
#'   \item{h2018_tot}{Total household members in 2018}
#'   \item{h2018_wetQ}{Household members in 2018 wet quarter}
#'   \item{h2018_wetQstart}{Household members at the start of the 2018 wet quarter}
#'   \item{qstartavg}{Average quarter start}
#'   \item{ndvi_avg}{Average NDVI (Normalized Difference Vegetation Index)}
#'   \item{ndvi_max}{Maximum NDVI}
#'   \item{h2018_ndvi_avg}{Average household NDVI in 2018}
#'   \item{h2018_ndvi_max}{Maximum household NDVI in 2018}
#'   \item{ADM0_EN}{Country name (Nigeria)}
#'   \item{ADM0_PCODE}{Country code (NG)}
#'   \item{ADM1_EN}{State name}
#'   \item{ADM1_PCODE}{State code}
#'   \item{ADM2_EN}{Local government area name}
#'   \item{ADM2_PCODE}{Local government area code}
#'   \item{geometry}{POINT location of each household}
#' }
#' @source \url{https://microdata.worldbank.org/index.php/catalog/3557}
"hhgeo_dt"



#' Boundary Shapefile for Nigeria
#'
#' This shapefile contains the boundaries of Nigeria up to the second administrative division, i.e., the local government areas (LGAs).
#'
#' @format An `sf` and `data.frame` object with 774 features (polygons) and 6 fields:
#' \describe{
#'   \item{ADM0_EN}{Country name, i.e., Nigeria}
#'   \item{ADM0_PCODE}{Two-letter country code (NG)}
#'   \item{ADM1_EN}{State name (Administrative Division 1)}
#'   \item{ADM1_PCODE}{State code (Administrative Division 1)}
#'   \item{ADM2_EN}{Local government area name (Administrative Division 2)}
#'   \item{ADM2_PCODE}{Local government area code (Administrative Division 2)}
#'   \item{geometry}{The polygon geometry features}
#' }
#' @source World Bank subnational boundary repository
"shp_dt"


