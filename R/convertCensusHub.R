#' convertCensusHub
#'
#' Convert data from Census Hub
#'
#' @param x raw data
#' @param subtype Eurostat code of data set
#' @returns MAgPie object with converted data
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getItems getItems<- getSets<- getSets
#' @importFrom madrat toolCountry2isocode
#' @importFrom utils  head
#' @export

convertCensusHub <- function(x, subtype) {
  data <- x

  # rename dimensions
  dimNames <- c(
    tob    = "buildingType",
    poc    = "constructionPeriod",
    ocs    = "level",
    period = "period",
    region = "region"
  )
  getSets(data) <- dimNames[getSets(data)]

  # rename items
  itemNames <- c(
    TOTAL        = "total",
    RES          = "residential",
    RES1         = "residential_1dwelling",
    RES2         = "residential_2dwelling",
    RES_GE3      = "residential_ge3dwelling",
    NRES         = "nonresidential",
    UNK          = "unknown",
    DW           = "dwellings",
    Y_LT1919     = "before1919",
    `Y1919-1945` = "1919-1945",
    `Y1946-1960` = "1946-1960",
    `Y1961-1970` = "1961-1970",
    `Y1961-1980` = "1961-1980",
    `Y1971-1980` = "1971-1980",
    `Y1981-1990` = "1981-1990",
    `Y1981-2000` = "1981-2000",
    `Y1991-2000` = "1991-2000",
    `Y2001-2005` = "2001-2005",
    `Y2001-2010` = "2001-2010",
    `Y2011-2015` = "2011-2015",
    `Y_GE2006`   = "after2005",
    `Y_GE2016`   = "after2016",
    `NA`         = NA

  )
  for (dim in 3 + 0.1 * seq_along(tail(getSets(data), -2))) {
    getItems(data, dim) <- itemNames[getItems(data, dim)]
  }


  # rename regions: ISO2 -> ISO3
  getItems(data, 1) <- gsub("^EL$", "Greece", getItems(data, 1))
  getItems(data, 1) <- toolCountry2isocode(getItems(data, 1))

  # fill missing regions with NA
  data <- toolCountryFill(data, verbosity = 2)

  return(data)
}
