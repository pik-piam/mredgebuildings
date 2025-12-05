#' Convert Long Term Renovation Strategy
#'
#' @param x MagPIE object with data read from source
#' @returns MagPIE object
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getItems<- setYears
#' @importFrom madrat toolCountryFill

convertLTRS <- function(x) {
  getItems(x, 1) <- "DEU"
  x <- setYears(x, 2019)
  toolCountryFill(x, verbosity = 2)
}
