#' Convert PRISMA ICT Data to Standard Format
#'
#' Converts PRISMA ICT data to ISO country codes and fills missing countries.
#' Primarily used for data center count data.
#'
#' @param x MAgPIE object from readPRISMA_ICT
#' @param subtype Character string specifying data type (same as in readPRISMA_ICT)
#'
#' @returns A magpie object with converted country codes and filled countries
#'
#' @author Hagen Tockhorn
#'
#' @importFrom magclass getItems
#' @importFrom madrat toolCountry2isocode toolCountryFill
#'
convertPRISMA_ICT <- function(x, subtype) { # nolint: object_name_linter

  if (subtype == "nDC") {
    getItems(x, 1) <- toolCountry2isocode(getItems(x, 1))
    x <- toolCountryFill(x, verbosity = 2)
  }

  return(x)
}
