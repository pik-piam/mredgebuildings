#' convertEUBuildingsDB
#'
#' Convert to common units.
#'
#' @param subtype character <category>.<variable>
#' @param x MAgPIE object with data from EU Buildings Database
#' @return clean MAgPIE object with
#'
#' @author Robin Hasse
#'
#' @importFrom quitte inline.data.frame
#' @importFrom magclass getNames getNames<- getItems getItems<- getSets<-
#' @importFrom madrat toolCountryFill
#' @export

convertEUBuildingsDB <- function(x, subtype) {

  data <- x

  # split subtype
  category <- strsplit(subtype, ".", fixed = TRUE)[[1]][1]
  variable <- strsplit(subtype, ".", fixed = TRUE)[[1]][2]

  # unit conversion
  unitConversion <- inline.data.frame(
    "from;          to;     factor",
    "Mm<b2>;        m2;     1", # per dwelling
    "m<b2>;         m2;     1",
    "thousand;      1;      1E3",
    "%;             1;      1E-2",
    "index;         1;      1",
    "No-of;         1;      1",
    "Mtoe;          EJ;     0.041868",
    "MJ/kg;         MJ/kg;  1",
    "W/m<b2><b0>C;  W/m2K;  1"
  )
  data <- toolUnitConversion(data, unitConversion)

  # drop EU sum and rename regions
  data <- data["EU", , invert = TRUE]
  getItems(data, 1) <- toolCountry2isocode(getItems(data, 1))

  # manually drop erroneous data points
  if (category == "BuildingStockCharacteristics") {
    data[, 2017, c("Total floor area of single family dwellings_m2",
                   "Total floor area of multi family dwellings_m2")] <- NA
  } else if (category == "BuildingShellPerformance") {
    data[, 2008, ] <- NA
  }

  # fill missing regions with NA
  data <- toolCountryFill(data, verbosity = 2)

  if (identical(variable, as.character(NA))) {
    return(data)
  }

  # special conversion of selected variables
  switch(subtype,
    `BuildingStockCharacteristics.residentialVintageShares` = {
      getNames(data) <- gsub(" and ", "-",
                             gsub("Share of dwellings built |between |_1$", "",
                                  gsub("before ", "<",
                                       gsub("after ", ">", getNames(data)))))
      getSets(data)[3] <- "cohort"
    },
    `BuildingStockCharacteristics.shareHouseholdSize` = {
      # the shares perfectly add up to 1, so 'more than 5' means '5 or more'
      getNames(data) <- gsub(
        "^(Number|Share) of dwelling(s|) |occupied by | persons.*|_1$", "",
        gsub("with single-person households", "1",
             gsub("more than ", ">=", getNames(data)))
      )
      getSets(data)[3] <- "householdSize"
    },
    stop("'", subtype, "' is an invalid subtype.")
  )

  return(data)
}
