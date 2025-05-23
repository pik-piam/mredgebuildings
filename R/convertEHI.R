#' Convert EHI Heating Market Report
#'
#' @param x MagPIE object with raw data
#' @returns MagPIE object with country-level data
#'
#' @author Robin Hasse
#'
#' @importFrom madrat toolCountryFill toolAggregate
#' @importFrom magclass mbind getItems getItems<- time_interpolate dimSums
#'   collapseDim
#' @export

convertEHI <- function(x) {

  # FUNCTIONS ------------------------------------------------------------------

  .countriesInRegion <- function(region) {
    unlist(strsplit(region, "_"))
  }



  # CONVERT --------------------------------------------------------------------

  ## ISO 3 country code ====

  getItems(x, 1) <- sub("^The ", "", getItems(x, 1))
  getItems(x, 1) <- unlist(lapply(getItems(x, 1), function(region) {
    do.call(paste, c(lapply(.countriesInRegion(region), toolCountry2isocode), list(sep = "_")))
  }))


  ## split aggregated regions ====

  weight <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    dimSums("typ") %>%
    time_interpolate(getItems(x, 2), extrapolation_type = "constant")

  x <- do.call(mbind, lapply(getItems(x, dim = 1), function(region) {

    xReg <- x[region, , ]
    countries <- .countriesInRegion(region)

    if (length(countries) > 1) {

      # subtract individually reported countries from aggregated regions
      countriesAlsoReported <- intersect(countries, getItems(x, 1))
      xReg <- xReg -
        dimSums(mselect(x, region = countriesAlsoReported), 1)
      countries <- setdiff(countries, countriesAlsoReported)

      # disaggregated remaining region into countries
      xReg <- toolAggregate(xReg,
                            rel = data.frame(region = region,
                                             country = countries),
                            weight = mselect(weight, region = countries),
                            from = "region", to = "country",
                            dim = 1, wdim = 1)
    }

    return(xReg)
  }))


  ## fill missing countries with NA ====

  x <- toolCountryFill(x, verbosity = 2)



  # RETURN ---------------------------------------------------------------------

  return(list(x = x,
              weight = NULL,
              min = 0,
              description = "Stock and sales of (efficient) heating systems in units(/yr)"))
}
