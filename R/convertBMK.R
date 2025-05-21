#' Convert heating systems sales in Austria from BMK
#'
#' @param x MagPIE object with BMK data
#' @returns MagPIE object with ISO 3 country codes
#'
#' @author Robin Hasse
#'
#' @importFrom madrat toolCountry2isocode toolCountryFill toolGetMapping
#' @importFrom magclass add_dimension getItems getItems<-
#' @importFrom dplyr pull
#' @export

convertBMK <- function(x) {

  # add region dimension
  x <- add_dimension(x, 1, "region", "Austria")
  getItems(x, 1) <- toolCountry2isocode(getItems(x, 1))

  # fill missing countries with NA
  x <- toolCountryFill(x, verbosity = 2)

  # translate technologies to English
  mapBMK <- toolGetMapping("technologyMapping_BMK.csv",
                           type = "sectoral", where = "mredgebuildings",
                           returnPathOnly = TRUE) %>%
    read.csv(encoding = "UTF-8")
  mapBMK <- pull(mapBMK, "technology", name = "technologyAUT")
  getItems(x, 3) <- unname(mapBMK[getItems(x, 3)])


  return(x)
}
