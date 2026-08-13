#' Convert GLOBUS building stock
#'
#' Disaggregate GLOBUS building stock projections from the model's aggregate
#' regions to ISO countries. Population is used as the disaggregation weight.
#'
#' @param x MAgPIE object from readGLOBUS
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

convertGLOBUS <- function(x) {

  # PARAMETERS -----------------------------------------------------------------

  variableMap <- c("residential building stock"     = "residential floor space",
                   "non-residential building stock" = "commercial floor space",
                   "Total building stock"           = "total floor space")


  # READ-IN DATA ---------------------------------------------------------------

  regionmap <- toolGetMapping(name  = "regionmappingGLOBUS.csv",
                              type  = "regional",
                              where = "mredgebuildings")

  pop <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)


  # PROCESS DATA ---------------------------------------------------------------

  pop <- pop %>%
    collapseDim() %>%
    time_interpolate(interpolated_year = getYears(x, as.integer = TRUE),
                     extrapolation_type = "constant")

  x %>%
    as.quitte(na.rm = TRUE) %>%
    mutate(variable = recode(.data[["variable"]], !!!variableMap)) %>%
    as.magpie() %>%

    toolAggregate(rel = regionmap,
                  weight = pop[unique(regionmap$CountryCode), , ],
                  from = "RegionCode",
                  to = "CountryCode") %>%

    toolCountryFill(fill = NA, verbosity = 2)

}
