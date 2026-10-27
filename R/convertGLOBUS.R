#' Convert GLOBUS building stock
#'
#' Disaggregate GLOBUS building stock projections from the model's aggregate
#' regions to ISO countries. For subtype \code{"total"}, population is used as the
#' disaggregation weight. For subtype \code{"perCapita"}, the per-capita values are
#' assigned to all countries of a region and the GLOBUS population is dropped.
#'
#' @param x MAgPIE object from readGLOBUS
#' @param subtype character, either \code{"total"} or \code{"perCapita"}
#'
#' @return MAgPIE object containing building stock per ISO country
#'
#' @author Hagen Tockhorn
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>% mutate recode .data
#' @importFrom magclass as.magpie collapseDim time_interpolate getYears
#' @importFrom madrat toolGetMapping toolAggregate toolCountryFill calcOutput
#' @export

convertGLOBUS <- function(x, subtype = "total") {

  # PARAMETERS -----------------------------------------------------------------

  variableMap <- c("residential building stock"     = "residential floor space",
                   "non-residential building stock" = "commercial floor space",
                   "Total building stock"           = "total floor space")


  # READ-IN DATA ---------------------------------------------------------------

  regionmap <- toolGetMapping(name  = "regionmappingGLOBUS.csv",
                              type  = "regional",
                              where = "mredgebuildings")


  # PROCESS DATA ---------------------------------------------------------------

  x <- x %>%
    as.quitte(na.rm = TRUE) %>%
    mutate(variable = recode(.data[["variable"]], !!!variableMap)) %>%
    as.magpie()

  x <- switch(subtype,

    # building stock is disaggregated by population
    total = {
      pop <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE) %>%
        collapseDim() %>%
        time_interpolate(interpolated_year = getYears(x, as.integer = TRUE),
                         extrapolation_type = "constant")

      toolAggregate(x,
                    rel = regionmap,
                    weight = pop[unique(regionmap$CountryCode), , ],
                    from = "RegionCode",
                    to = "CountryCode")
    },

    # identical per-capita values are assigned to every country of a region
    perCapita = {
      toolAggregate(x[, , "Population", invert = TRUE],
                    rel = regionmap,
                    from = "RegionCode",
                    to = "CountryCode")
    }
  )

  toolCountryFill(x, fill = NA, verbosity = 2)

}
