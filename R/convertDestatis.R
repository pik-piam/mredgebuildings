#' Convert data from the Federal Statistical Office of Germany
#'
#' @param x MagaPie object
#' @param subtype character, data code
#' @returns MagPIE object
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getItems<-
#' @importFrom madrat toolCountryFill
#' @export

convertDestatis <- function(x, subtype) {
  getItems(x, 1) <- "DEU"
  x <- toolCountryFill(x, verbosity = 2)
  return(x)
}
