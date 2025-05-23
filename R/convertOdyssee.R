#' convertOdyssee
#'
#' Rename regions and convert unit
#'
#' @param x MAgPIE object with data from Odyssee Database
#' @return clean MAgPIE object
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getItems getItems<- mselect add_columns
#' @importFrom madrat toolCountry2isocode toolCountryFill
#' @importFrom quitte inline.data.frame
#'
#' @export

convertOdyssee <- function(x) {

  data <- x

  # rename regions: ISO2 -> ISO3
  getItems(data, 1) <- toolCountry2isocode(getItems(data, 1),
                                           ignoreCountries = c("EU", "XK"))

  # unit conversion
  unitConversion <- inline.data.frame(
    "from;     to;       factor",
    "Mtoe;     EJ;       4.1868E-2",
    "TWh;      EJ;       3.6E-3",
    "k;        1;        1000",
    "Mm2;      m2;       1E6",
    "%;        1;        1E-2",
    "degree;   dK/yr;    1",
    "MEUR2015; MEUR2015; 1"
  )
  for (i in seq_len(nrow(unitConversion))) {
    if (!unitConversion[[i, "from"]] %in% getItems(data, "unit")) next
    tmp <- mselect(data, unit = unitConversion[[i, "from"]])
    tmp <- tmp * unitConversion[[i, "factor"]]
    getItems(tmp, "unit") <- unitConversion[[i, "to"]]
    data <- data %>%
      mselect(unit = setdiff(getItems(data, "unit"),
                             unitConversion[[i, "from"]])) %>%
      mbind(tmp)
  }

  # manually drop erroneous data points
  data["HUN", , c("surlpn", "surmpn", "suripn")] <- NA
  data["PRT", , c("nbrlprpet", "nbrlprgaz", "nbrlprcms", "nbrlprvap", "nbrlprboi", "nbrlprele", "nbriprele")] <- NA
  data["FIN", , c("nbriprvap")] <- NA

  # calculate resistive electric heating
  namesElec <- grep("^nbr.prele\\.", getNames(data), value = TRUE)
  for (elec in namesElec) {
    elecHP <- sub("ele", "elehp", elec)
    elecRH <- sub("ele", "elerh", elec)
    dataRH <- data[, , elec] - data[, , elecHP]
    getNames(dataRH) <- elecRH
    getSets(dataRH) <- getSets(data)
    dataRH[] <- pmax(0, dataRH)
    data <- mbind(data, dataRH)
  }

  # fill missing regions with NA
  data <- toolCountryFill(data, verbosity = 2)

  return(data)
}
