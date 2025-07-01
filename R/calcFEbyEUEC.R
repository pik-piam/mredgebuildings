#' Historic Final Energy Demand by Carrier and Enduse
#'
#' Takes the historic final energy demand by carriers from IEA and disaggregates
#' it into different end uses.
#'
#' The disaggregation is performed such that the aggregated carrier-specific FE
#' values of the IEA IO regions are met as a minimum requirement. In an ideal case,
#' the enduse-specific FE shares are met as well.
#'
#' @note
#' For now, existing disaggregated final energy data with respect to carriers and
#' enduses combined is replaced in the final output. However, since the Odyssee
#' data is largely underestimating the real IEA FE targets, these shall serve as
#' lower boundaries for the disaggregation.
#' Such feature has been implemented in \code{toolDisaggregate} but is not running
#' smoothly yet.
#'
#' @returns data.frame with historic energy demands
#'
#' @author Hagen Tockhorn, Robin Hasse
#'
#' @importFrom dplyr mutate semi_join right_join left_join as_tibble filter
#'   select %>% .data pull
#' @importFrom madrat toolCountryFill calcOutput toolGetMapping
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

calcFEbyEUEC <- function() {

  # READ-IN DATA ---------------------------------------------------------------

  # FE Data
  ieaIO <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings",
                      aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE)


  # FE EU Data
  feOdyssee <- calcOutput("ShareOdyssee",
                          subtype = "enduse_carrier",
                          feOnly = TRUE,
                          aggregate = FALSE) %>%
    as.quitte()


  # FE IEA EEI data
  feIEAEEI <- calcOutput("IEA_EEI",
                         subtype = "buildings",
                         aggregate = FALSE) %>%
    as.quitte()


  # EU Shares
  sharesEU <- calcOutput("SharesEU",
                         thermal = FALSE,
                         aggregate = TRUE,
                         regionmapping = "regionmappingEUshares.csv") %>%
    as.quitte()


  # Household survey carrier-enduse shares
  surveyShares <- rbind(readSource("HouseholdSurveys", "cooking") %>%
                          as_tibble(),
                        readSource("HouseholdSurveys", "lighting") %>%
                          as_tibble())


  # EU Shares mapping
  regmapping <- toolGetMapping(name  = "regionmappingEUshares.csv",
                               type  = "regional",
                               where = "mredgebuildings") %>%
    select("region", "regionAgg")



  # PARAMETERS -----------------------------------------------------------------

  # Enduse-Carrier combinations which will be systematically excluded
  exclude <- toolGetMapping(name  = "excludeEnduseCarrier.csv",
                            type  = "sectoral",
                            where = "mredgebuildings")

  # lower temporal boundary for historical data
  periodBegin <- 1990


  # PROCESS DATA ---------------------------------------------------------------

  # expand end-use shares to cover full period range
  sharesEU <- sharesEU %>%
    select("region", "period", "enduse", "value") %>%
    interpolate_missing_periods(unique(ieaIO$period), expand.values = TRUE) %>%
    filter(.data$period >= periodBegin)

  ieaIO <- ieaIO %>%
    rename(carrier = "variable") %>%
    filter(.data$period >= periodBegin)


  # remove district space cooling from disaggregation
  exclude <- exclude %>%
    rbind(data.frame(enduse = "space_cooling",
                     carrier = "heat"))


  #--- Prepare toolDisaggregate Input

  # prepares shares to force on disaggregation estimates
  surveyShares <- surveyShares %>%
    filter(!is.na(.data$value),
           # remove outlier region that leads to crooked estimates
           .data$region != "PER")

  # combine the already disaggregated data and expand to cover full period range
  feDisagg <- feOdyssee %>%
    left_join(feIEAEEI,
              by = c("region", "period", "carrier", "enduse")) %>%
    mutate(value = ifelse(is.na(.data[["value.x"]]),
                          .data[["value.y"]],
                          .data[["value.x"]])) %>%
    select("region", "period", "carrier", "enduse", "value") %>%
    interpolate_missing_periods(unique(ieaIO$period), expand.values = TRUE)


  # Disaggregate FE with EU/EC Shares
  ieaIODis <- ieaIO %>%
    select("region", "period", "carrier", "value") %>%
    mutate(unit = "fe") %>%
    toolDisaggregate(enduseShares  = sharesEU,
                     exclude       = exclude,
                     dataDisagg    = feDisagg,
                     regionMapping = regmapping,
                     forceShares   = surveyShares) %>%
    select("region", "period", "unit", "carrier", "enduse", "value")


  # existing enduse-carrier shares are applied directly on IEA data
  dataReplaceFull <- feDisagg %>%
    right_join(ieaIODis, by = c("region", "period", "carrier", "enduse"),
               suffix = c("Data", "Calc")) %>%
    group_by(across(all_of(c("region", "period", "carrier")))) %>%
    filter(!all(is.na(.data$valueData))) %>%
    mutate(value = ifelse(is.na(.data$valueData),
                          .data$valueCalc,
                          .data$valueData),
           share = proportions(.data$value)) %>%
    ungroup() %>%
    select("region", "period", "carrier", "enduse", "share")


  # existing disaggregated data replaces values from optimization
  data <- ieaIODis %>%
    left_join(dataReplaceFull, by = c("region", "period", "carrier", "enduse")) %>%
    group_by(across(all_of(c("region", "period", "carrier")))) %>%
    mutate(recalculate = any(!is.na(.data$share)),
           value = ifelse(.data$recalculate,
                          sum(.data$value) * replace_na(.data$share, 0),
                          .data$value)) %>%
    ungroup() %>%
    select("region", "period", "unit", "carrier", "enduse", "value")


  # CORRECTIONS ----------------------------------------------------------------

  # Since the data on district cooling is very sparse and the technology
  # exhibits a low global penetration rate, we assume that all historic cooling
  # demand is covered by # electricity but assume that district cooling might
  # play a more significant role in the future.

  dataCorr <- data %>%
    select(-"enduse", -"carrier", -"value") %>%
    unique() %>%
    mutate(enduse = "space_cooling",
           carrier = "heat",
           value = 0)

  dataFull <- rbind(dataCorr, data)


  # Aggregate End-Uses appliances and lighting
  dataFull <- dataFull %>%
    aggToLevel(variables = c("appliances", "lighting"), level = "enduse", newname = "appliances_light")




  # OUTPUT ---------------------------------------------------------------------

  # Pack Data
  dataFull <- dataFull %>%
    mutate(scenario = "history") %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(0, verbosity = 2)


  return(list(x = dataFull,
              weight = NULL,
              min = 0,
              unit = "EJ",
              description = "Historic Final Energy Data from IEA disaggregated by end use"))
}
