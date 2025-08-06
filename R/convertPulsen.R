#' Convert Pulsen survey on Swedish heat pump market
#'
#' @author Robin Hasse
#'
#' @param x MagPIE object with data
#' @returns MagPIE object with corrected region
#'
#' @importFrom magclass getItems<-
#' @importFrom madrat toolCountryFill
#' @export

convertPulsen <- function(x) {
  getItems(x, 1) <- "SWE"
  x <- toolCountryFill(x, verbosity = 2)
  return(x)
}
