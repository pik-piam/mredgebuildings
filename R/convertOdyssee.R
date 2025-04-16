#' convertOdyssee
#'
#' Rename regions and convert unit
#'
#' @param subtype category
#' @param x MAgPIE object with data from Odyssee Database
#' @return clean MAgPIE object
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getItems getItems<-
#' @importFrom madrat toolCountry2isocode toolCountryFill
#' @importFrom quitte inline.data.frame
#'
#' @export

convertOdyssee <- function(x, subtype = "households") {

  data <- x

  # filter problematic/unnecessary regions
  problematicRegions <- c("EU", "XK")
  data <- data[!getItems(data, 1) %in% problematicRegions, , ]

  # rename regions: ISO2 -> ISO3
  getItems(data, 1) <- toolCountry2isocode(getItems(data, 1))

  # unit conversion
  unitConversion <- inline.data.frame(
    "from;     to;       factor",
    "Mtoe;     EJ;       4.1868E-2",
    "ktoe;     EJ;       4.1868E-5",
    "Tj;       EJ;       1E-6",
    "TWh;      EJ;       3.6E-3",
    "k;        1;        1000",
    "m2;       m2;       1",
    "Mm2;      m2;       1E6",
    "%;        1;        1E-2",
    "degree;   dK/yr;    1",
    "MEUR2015; MEUR2015; 1"
  )
  data <- toolUnitConversion(data, unitConversion)

  # fill missing regions with NA
  data <- toolCountryFill(data, verbosity = 2)

  return(data)
}
