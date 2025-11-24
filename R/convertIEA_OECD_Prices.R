#' read energy carrier prices
#'
#' @param x MagPIE object
#' @param subtype character, data set
#'
#' @author Robin Hasse
convertIEA_OECD_Prices <- function(x, subtype) { # nolint: object_name_linter.

  getItems(x, 1) <- toolCountry2isocode(getItems(x, 1))
  x <- toolCountryFill(x, verbosity = 2)

  x
}
