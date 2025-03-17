#' Ecological subsection maps, from Cleland et al. (2007).
#'
#' @references
#' Cleland, D.T., Freeouf, J.A., Keys, J.E., Nowacki, G.J., Carpenter, C.A.,
#'     McNab, W.H., 2007. Ecological Subregions: Sections and Subsections for
#'     the conterminous United States.
#'     General Technical Report WO-76D 76D.
#' McNab, W.H., Cleland, D.T., Freeouf, J.A., Keys, J.E., Nowacki, G.J.,
#'     Carpenter, C.A., 2007. Description of ecological subregions:
#'     sections of the conterminous United States.
#'     General Technical Report WO-76B 76B, 1–82.
#'     https://doi.org/10.2737/WO-GTR-76B
#'
#'
#' @name Cleland2007_eco_map
#' @docType data
#' @format A list of three simple features objects (class "sf").
#' \describe{
#'   \item{province}{Province-level polygons}
#'   \item{sections}{Section-level polygons}
#'   \item{subsects}{Subsection-level polygons}
#' }
'Cleland2007_eco_map'
#' Cleland 2007 Province/Section/Subsection Names/Code Info
#'
#' Zone metadata pulled from 'Cleland2007_eco_map'.
#'
#' See `Cleland_meta_df.Rmd` for details on generating this dataset. Not
#' updated when data format was changed to 'sf'.
#'
#' @name Cleland_meta_df
#' @docType data
#' @format A data.frame of 12 columns and 1233 rows, where each row corresponds
#'         to a single named subsection.
#' \describe{
#'   \item{domain}{Dry, Humid Temperate, Humid Tropical, or Water}
#'   \item{divsion}{Temperate Desert, Mediterranean, Prairie, i.e. biome}
#'   \item{province_name}{Short name of province}
#'   \item{province_code}{USFS Province Code, see `ECOSUBCD`` in FIA data}
#'   \item{province_description}{Long name of province}
#'   \item{section_id}{Section ID #, shortest name}
#'   \item{section_name}{Short name of section}
#'   \item{section_code}{USFS Section Code, see `ECOSUBCD`` in FIA data}
#'   \item{section_description}{Long name of section}
#'   \item{subsection_id}{Subsection ID #, shortest name}
#'   \item{subsection_name}{Short name of subsection}
#'   \item{subsection_code}{USFS Subection Code, see `ECOSUBCD`` in FIA data}
#' }
'Cleland_meta_df'
