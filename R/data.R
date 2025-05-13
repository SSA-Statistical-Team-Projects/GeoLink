#' Nigeria GHS 2018/19 Data
#'
#' The full household geovars dataset from the Nigerian General Household Survey 2018/19.
#'
#' @format A `data.table` and `data.frame` object with 5116 rows and 48 variables:
#' \describe{
#'   \item{hhid}{Household ID}
#'   \item{dist_road2}{Distance to the nearest road}
#'   \item{dist_popcenter2}{Distance to the nearest population center}
#'   \item{geometry}{POINT location of each household}
#'   \item{...}{For a complete list of variables, see <https://microdata.worldbank.org/index.php/catalog/3557/related-materials>}
#' }
#'
#' @source <https://microdata.worldbank.org/index.php/catalog/3557>
"hhgeo_dt"

#' Boundary Shapefile for Nigeria
#'
#' The full shapefile of Nigeria up to the second administrative divisions (local government areas).
#'
#' @format A `sf` and `data.frame` object with 774 feature polygon geometries and 6 fields:
#' \describe{
#'   \item{ADM0_EN}{Country Name, i.e. Nigeria}
#'   \item{ADM0_PCODE}{Two-letter country code}
#'   \item{ADM1_EN}{Administrative Division 1 names, i.e. State names}
#'   \item{ADM1_PCODE}{Administrative Division 1 codes, i.e. State codes}
#'   \item{ADM2_EN}{Admin 2 division names, i.e. Local Government Area names}
#'   \item{ADM2_PCODE}{Admin 2 codes, i.e. Local Government Area codes}
#'   \item{geometry}{Polygon geometry features}
#' }
#'
#' @source World Bank Subnational Boundary Repository
"shp_dt"

#' Population data for Nigeria
#'
#' Population data for second administrative divisions (local government areas.)
#'
#'@format A `data.frame` containing 773 areas with identifiers and total population.
#' \describe{
#'   \item{ADM2_EN}{Admin 2 division names, i.e. Local Government Area names}
#'   \item{ADM2_PCODE}{Admin 2 codes, i.e. Local Government Area codes}
#'   \item{T_TL}{Total population for area}
#'   \item{...}{For more information see <https://data.humdata.org/dataset/cod-ps-nga>}
#' }
#' @source Humanitarian Data Exchange
"popHDX_dt"


