#' convertEurostatBuildings
#'
#' Convert buildings-related data from Eurostat.
#'
#' @param x raw data
#' @param subtype Eurostat code of data set
#' @returns MAgPie object with converted data
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getItems getItems<-
#' @importFrom madrat toolCountry2isocode
#' @export

convertEurostatBuildings <- function(x, subtype) {
  data <- x

  # drop sub-national regions
  subRegs <- grep("^[A-Z]{2}([A-Z]|\\d)", getItems(data, 1), value = TRUE)
  data <- data[subRegs, , invert = TRUE]

  # drop EU aggregates
  dropRegions <- grep("^(EU|EA)", getItems(data, 1), value = TRUE)
  if (length(dropRegions) > 0) {
    data <- data[dropRegions, invert = TRUE, , ]
  }

  # rename regions: ISO2 -> ISO3
  getItems(data, 1) <- gsub("^XK$", "Kosovo", getItems(data, 1))
  getItems(data, 1) <- gsub("^EL$", "Greece", getItems(data, 1))
  getItems(data, 1) <- toolCountry2isocode(getItems(data, 1))

  # fill missing regions with NA
  data <- toolCountryFill(data, verbosity = 2)

  # manually drop erroneous data points
  if (subtype == "nrg_d_hhq") {
    mselect(data, region = "ESP", period = "y2017", siec = "RA600") <- NA
  } else if (subtype == "lfst_r_lfsd2hh") {
    # we assume that each country has to have households in each degree of urbanisation
    data[data <= 0] <- NA
  } else if (subtype == "ilc_lvho31") {
    data[data <= 0] <- NA
    mselect(data, region = "SRB", period = "y2020") <- NA
    mselect(data, region = "ISL") <- NA
  } else if (subtype == "ilc_hcmh02") {
    data[data <= 0] <- NA
  }

  return(data)
}
