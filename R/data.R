#' Nigeria GHS 2018/19 data
#'
#' The full household geovars dataset from Nigerian General Household Survey 2018/19
#' dataset.
#'
#' @format ## `hhgeo_dt`
#'
#' An `data.table`, `data.frame` with 5116 rows and 42 variables:
#'
#' \describe{
#'  \item{hhid}{Household ID}
#'  \item{dist_road2}{Distance to Nearest Road}
#'  \item{dist_popcenter2}{Distance to population center}
#'  ... see <https://microdata.worldbank.org/index.php/catalog/3557/related-materials>
#'  for the complete set of variables
#'  \item{geometry}{POINT location of each household}
#'
#'
#' }
#'
#' @source <https://microdata.worldbank.org/index.php/catalog/3557>
"hhgeo_dt"


#' Boundary Shapefile for Nigeria
#'
#' The full shapefile of Nigeria up to the second administrative divisions i.e. the local government
#' areas
#'
#' @format ## `shp_dt`
#'
#' An `sf`, `data.frame` object with 774 feature polygon geometries and 6 fields
#'
#' \describe{
#'  \item{ADM0_EN}}{Country Name i.e. Nigeria}
#'  \item{ADM0_PCODE}{Two letter country code}
#'  \item{ADM1_EN}{Administrative Division 1 names, i.e. State names}
#'  \item{ADM1_PCODE}{Administrative Division 1 codes, i.e. State codes}
#'  \item{ADM2_EN}{Admin 2 division names, i.e. local government area names}
#'  \item{ADM2_PCODE}{Admin 2 codes i.e. local government area codes}
#'  \item{geometry}{the polygon geometry features}
#'
#' }
#' @source World Bank subnational boundary repository
"shp_dt"
