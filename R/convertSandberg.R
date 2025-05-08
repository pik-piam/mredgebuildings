#' Convert building life time data from Sandberg et al. 2016
#'
#' @param x MagPie object
#' @returns MagPIE object with full region converage
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getSets<- getItems getItems<-
#' @importFrom madrat toolCountry2isocode toolCountryFill

convertSandberg <- function(x) {
  getSets(x)[1:2] <- c("region", "period")
  getItems(x, 1) <- sub("The Netherlands", "Netherlands",    getItems(x, 1))
  getItems(x, 1) <- sub("Great Britain",   "United Kingdom", getItems(x, 1))
  getItems(x, 1) <- toolCountry2isocode(getItems(x, 1))
  toolCountryFill(x, verbosity = 2)
}
