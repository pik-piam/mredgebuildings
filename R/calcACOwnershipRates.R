#' Calculate Air Conditioning Ownership Rates
#'
#' Calculates air conditioning ownership rates per household by combining
#' data from household surveys, IEA, and Odyssee. Data sources are prioritized in the
#' following order: Odyssee > IEA > household surveys. Returns all available historical
#' data points without period harmonization.
#'
#' @return magpie object with all available historical data points per region
#'
#' @author Hagen Tockhorn
#'
#' @importFrom magclass as.magpie
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter select rename mutate coalesce full_join semi_join
#' @importFrom quitte as.quitte removeColNa
#' @importFrom madrat readSource calcOutput toolCountryFill toolGetMapping toolCountry2isocode

calcACOwnershipRates <- function() {

  # READ-IN DATA ---------------------------------------------------------------

  ## Datasets ====

  # household surveys
  surveyData <- readSource("HouseholdSurveys", subtype = "appliances") %>%
    as_tibble()

  # IEA
  ieaData <- readSource("IEA_acOwnership") %>%
    as_tibble()

  # Odyssee
  odysseeData <- readSource("Odyssee") %>%
    as.quitte(na.rm = TRUE)

  # population (aggregation weights)
  pop <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE) %>%
    as.quitte() %>%
    removeColNa()


  ## Mappings ====

  regionmapIEA <- toolGetMapping("regionmappingIEA_acOwnership.csv",
                                 type = "regional",
                                 where = "mredgebuildings")



  # PROCESS DATA ---------------------------------------------------------------

  ## Process individual data sets ====

  ### Household surveys ====

  surveyData <- surveyData %>%
    filter(!is.na(.data$value),
           .data$variable == "ac") %>%
    select(-"variable") %>%
    rename("valueSurvey" = "value")


  ### IEA ====

  ieaData <- ieaData %>%
    filter(!is.na(.data$value))

  # filter out single country data points and give them priority
  ieaSingleCountry <- ieaData %>%
    filter(!.data$region %in% regionmapIEA$regionIEA) %>%
    select(-"variable") %>%
    rename("valueIEASingle" = "value") %>%
    mutate(region = as.character(.data$region))

  ieaSingleCountry$region <- toolCountry2isocode(ieaSingleCountry$region)

  #TODO: filter regional values and disaggregate them to fill missing values? #nolint


  ### Odyssee ====

  # filter "AC equipment rate" and take most recent data point per region where necessary
  odysseeData <- odysseeData %>%
    filter(.data$variable == "teqcli") %>%
    select("region", "period", "value") %>%
    rename("valueOdyssee" = "value") %>%

    # Montenegro (MNE) shows an ownership rate of 100% which seems unrealistic
    filter(.data$region != "MNE")


  ## Merge Datasets ====

  # target : obtain merged data set keeping all historical data points
  # merge priority : odyssee > iea > surveys

  ownershipRates <- odysseeData %>%
    full_join(ieaSingleCountry, by = c("region", "period")) %>%
    full_join(surveyData, by = c("region", "period")) %>%
    mutate(value = coalesce(.data$valueOdyssee, .data$valueIEASingle, .data$valueSurvey),
           variable = "ac ownership rate") %>%
    select("region", "period", "variable", "value") %>%
    filter(!is.na(.data$value))


  ## Aggregation Weights ====

  pop <- pop %>%
    select(-"variable") %>%
    semi_join(ownershipRates, by = c("region", "period"))



  # OUTPUT ---------------------------------------------------------------------

  data <- ownershipRates %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(0, verbosity = 2)

  pop <- pop %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(0, verbosity = 2)

  return(list(x = data,
              weights = pop,
              unit = "",
              min = 0,
              max = 1,
              description = "Ac ownership rates per household",
              aggregationArguments = list(zeroWeight = "allow")))
}
