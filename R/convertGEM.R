#' Convert GEM Global Exposure Model
#'
#' The GEM exposure data are already reported on ISO3 country level, so no
#' disaggregation is required. This function only fills the remaining ISO
#' countries with NA to obtain a complete country set.
#'
#' @param x MAgPIE object from readGEM
#' @return MAgPIE object
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr %>%
#' @importFrom madrat toolCountryFill
#' @export

convertGEM <- function(x) {
  x %>%
    toolCountryFill(fill = NA, verbosity = 2)
}
