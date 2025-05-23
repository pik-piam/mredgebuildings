#' Convert heating systems sales in Spain from FEGECA
#'
#' @param x MagPIE object with Assotermica data
#' @returns MagPIE object with ISO 3 country codes
#'
#' @author Robin Hasse
#'
#' @importFrom madrat toolCountry2isocode toolCountryFill toolGetMapping
#' @importFrom magclass add_dimension getItems getItems<-
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr pull
#' @export

convertFEGECA <- function(x) {

  # add region dimension
  x <- add_dimension(x, 1, "region", "Spain")
  getItems(x, 1) <- toolCountry2isocode(getItems(x, 1))

  # fill missing countries with NA
  x <- toolCountryFill(x, verbosity = 2)

  # translate technologies to English
  mapFEGECA <- toolGetMapping("technologyMapping_FEGECA.csv",
                              type = "sectoral", where = "mredgebuildings",
                              returnPathOnly = TRUE) %>%
    read.csv(encoding = "UTF-8")
  mapFEGECA <- pull(mapFEGECA, "variable", name = "variableESP")
  getItems(x, 3.1) <- unname(mapFEGECA[getItems(x, 3.1)])

  # inter and extrapolate values
  x <- as.quitte(x) %>%
    interpolate_missing_periods(expand.values = TRUE) %>%
    as.magpie()

  return(x)
}
