#' Calculate Air Conditioning Ownership Rates
#'
#' Calculates air conditioning ownership rates per household by combining
#' data from household surveys, IEA, and Odyssee. Data sources are prioritized in the
#' following order: Odyssee > IEA > household surveys.
#'
#' @return A data frame with one data point per region
#'
#' @author Hagen Tockhorn
#'
#' @importFrom magclass as.magpie
#' @importFrom dplyr filter select rename mutate group_by slice_max ungroup coalesce anti_join full_join semi_join
#' @importFrom tidyr as_tibble
#' @importFrom quitte as.quitte
#' @importFrom madrat toolCountryFill toolGetMapping
#' @importFrom quitte interpolate_missing_periods

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
  odysseeData <- rbind(readSource("Odyssee", subtype = "households") %>%
                         as.quitte(na.rm = TRUE),
                       readSource("Odyssee", subtype = "services") %>%
                         as.quitte(na.rm = TRUE))

  # FE demand (as aggregation weights)
  feData <- calcOutput("FEbyEUEC", aggregate = FALSE) %>%
    as_tibble()


  ## Mappings ====

  regionmapIEA <- toolGetMapping("regionmappingIEA_acOwnership.csv",
                                 type = "regional",
                                 where = "mredgebuildings")

  regionmapEDGE <- toolGetMapping("regionmappingISO-EDGE_EUR_ETP.csv",
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

  # filter "AC equipment rate" and take most recent data point per region
  odysseeData <- odysseeData %>%
    filter(.data$variable == "teqcli_1") %>%
    select("region", "period", "value") %>%
    group_by(across(all_of(c("region")))) %>%
    slice_max(order_by = .data$period, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename("valueOdyssee" = "value") %>%

    # Montenegro (MNE) shows an ownership rate of 100% which seems unrealistic
    filter(.data$region != "MNE")


  ## Merge Datasets ====

  # target : obtain merged data set with one data point per region
  # merge priority : odyssee > iea > surveys

  ownershipRates <- odysseeData %>%
    full_join(ieaSingleCountry %>%
                anti_join(odysseeData, by = "region"),
              by = c("region", "period")) %>%
    full_join(surveyData %>%
                anti_join(odysseeData, by = "region") %>%
                anti_join(ieaSingleCountry, by = "region"),
              by = c("region", "period")) %>%
    mutate(value = coalesce(.data$valueOdyssee, .data$valueIEASingle, .data$valueSurvey)) %>%
    select("region", "period", "value")


  # ensure all countries within EDGE region have the same period to avoid aggregation issues
  ownershipRates <- ownershipRates %>%
    left_join(regionmapEDGE, by = c("region" = "CountryCode")) %>%
    group_by(across(all_of("RegionCodeEUR_ETP"))) %>%
    mutate(period = as.integer(median(.data$period)),
           variable = "ac ownership rate") %>%
    ungroup() %>%
    select(-"RegionCodeEUR_ETP") %>%
    interpolate_missing_periods()


  ## Aggregation Weights ====

  # total space_cooling fe demand per region
  feCoolingData <- feData %>%
    filter(.data$enduse == "space_cooling") %>%
    group_by(across(all_of(c("region", "period")))) %>%
    reframe(value = sum(.data$value)) %>%
    interpolate_missing_periods(min(unique(ownershipRates$period)):max(unique(ownershipRates$period)),
                                expand.values = TRUE) %>%
    semi_join(ownershipRates, by = c("region", "period"))



  # OUTPUT ---------------------------------------------------------------------

  data <- ownershipRates %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(0, verbosity = 2)

  feCoolingData <- feCoolingData %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(0, verbosity = 2)

  return(list(x = data,
              weights = feCoolingData,
              unit = "",
              min = 0,
              max = 1,
              description = "Ac ownership rates per household",
              aggregationArguments = list(zeroWeight = "allow")))
}
