#' Calculate Filled Air Conditioning Ownership Rates with Regional Parameters
#'
#' This function fills AC ownership rates for all regions from 1990 to endOfHistory using
#' global regression parameters and calculates region-specific beta coefficients where
#' historical data is available. Regional betas are derived to match the last historical
#' data point per region.
#'
#' @param endOfHistory last period of historical data (default: 2025)
#'
#' @return magpie object with filled AC ownership rates for all regions (1990-endOfHistory)
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr select filter left_join group_by across all_of mutate ungroup
#'   slice_max right_join semi_join
#' @importFrom tibble as_tibble
#' @importFrom quitte as.quitte removeColNa
#' @importFrom magclass as.magpie
#' @importFrom madrat toolCountryFill calcOutput

calcACOwnershipRatesFilled <- function(endOfHistory = 2025) {

  # READ-IN DATA ---------------------------------------------------------------

  # AC ownership rates (sparse historical data)
  ownershipRates <- calcOutput("ACOwnershipRates", aggregate = FALSE) %>%
    as_tibble()

  # regression parameters
  regPars <- calcOutput("ACOwnershipRegression", aggregate = FALSE) %>%
    as_tibble()

  # GDP per capita
  gdppop <- calcOutput("GDPpc",
                       scenario = "SSP2",
                       average2020 = FALSE,
                       aggregate = FALSE) %>%
    as.quitte()

  # CDD
  hddcdd <- calcOutput("HDDCDD", aggregate = FALSE) %>%
    as_tibble()

  # population (aggregation weights)
  pop <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE) %>%
    as.quitte() %>%
    removeColNa()



  # PROCESS DATA ---------------------------------------------------------------

  ## Extract Global Parameters ====

  regPars <- setNames(regPars$value, regPars$variable)

  alpha <- regPars[["alpha"]]
  beta  <- regPars[["beta"]]
  gamma <- regPars[["gamma"]]
  delta <- regPars[["delta"]]


  ## Prepare Fit Data ====

  fitData <- ownershipRates %>%
    select("region", "period", "penetration" = "value") %>%
    filter(!is.na(.data$penetration),
           .data$penetration != 0) %>%

    left_join(gdppop %>%
                select("region", "period", "gdppop" = "value"),
              by = c("region", "period")) %>%

    left_join(hddcdd %>%
                filter(.data$variable == "CDD",
                       .data$tlim == 20,
                       .data$rcp == "historical") %>%
                group_by(across(all_of(c("region", "period")))) %>%
                reframe(CDD = mean(.data$value)),
              by = c("region", "period"))


  ## Calculate Regional Betas ====

  # prepare data with all regions
  allRegions <- gdppop %>%
    select("region") %>%
    unique()

  # calculate regional betas from last historical data point (non-linear approach)
  betaReg <- fitData %>%
    group_by(across(all_of(c("region")))) %>%
    slice_max(order_by = .data$period, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(betaReg = (alpha - log(1 / .data$penetration - 1)) / (.data$gdppop^delta * .data$CDD^gamma)) %>%
    select("region", "betaReg") %>%

    # fill missing regions with global beta
    right_join(allRegions, by = "region") %>%
    mutate(betaReg = ifelse(is.na(.data$betaReg) | is.infinite(.data$betaReg), beta, .data$betaReg))


  ## Fill Historical Timeline 1990-endOfHistory ====

  filledRates <- gdppop %>%
    filter(.data$period >= 1990, .data$period <= endOfHistory) %>%
    select("region", "period", "gdppop" = "value") %>%

    left_join(hddcdd %>%
                filter(.data$variable == "CDD",
                       .data$tlim == 20,
                       .data$rcp == "historical") %>%
                group_by(across(all_of(c("region", "period")))) %>%
                reframe(CDD = mean(.data$value)),
              by = c("region", "period")) %>%

    left_join(betaReg, by = "region") %>%

    mutate(value = 1 / (1 + exp(alpha - .data$betaReg * .data$gdppop^delta * .data$CDD^gamma)),
           variable = "ac ownership rate") %>%

    select("region", "period", "variable", "value")


  ## Aggregation Weights ====

  pop <- pop %>%
    select(-"variable") %>%
    semi_join(filledRates, by = c("region", "period"))



  # OUTPUT ---------------------------------------------------------------------

  data <- filledRates %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(NA, verbosity = 2)

  pop <- pop %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(0, verbosity = 2)

  return(list(x = data,
              weights = pop,
              unit = "",
              min = 0,
              max = 1,
              description = "Filled AC ownership rates per household",
              aggregationArguments = list(zeroWeight = "allow")))

}
