#' Calculate Extrapolated Air Conditioning Ownership Rates with Regional Calibration
#'
#' This function fills AC ownership rates for all regions from 1990 to endOfHistory using
#' global regression parameters (alpha, beta, gamma, delta) with region-specific GDP shifts.
#' The GDP shift (gdppopShift) is calculated for each region to horizontally shift the
#' global S-curve along the GDP axis so that it passes through the region's most recent
#' historical data point. This calibrates the global curve to match available historical
#' data while maintaining the same functional form across all regions.
#'
#' The logistic formula used is:
#' \deqn{penetration = 1 / (1 + \exp(\alpha - \beta \cdot (gdppop - gdppopShift)^\delta \cdot CDD^\gamma))}
#'
#' Regions without historical data use the global curve as-is (gdppopShift = 0).
#'
#' @param endOfHistory last period of historical data (default: 2025)
#'
#' @returns magpie object with filled AC ownership rates for all regions (1990-endOfHistory)
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr %>% .data across all_of filter group_by left_join mutate
#'   reframe right_join select semi_join slice_max ungroup
#' @importFrom madrat calcOutput toolCountryFill
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte removeColNa
#' @importFrom tibble as_tibble
#' @importFrom tidyr replace_na

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
  hddcdd <- calcOutput("HDDCDD", scenario = "ssp2", aggregate = FALSE) %>%
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
                select("region", "period", "CDD" = "value"),
              by = c("region", "period"))


  ## Calculate Regional Parameters ====

  # Calculate regional gdppopShift from last historical data point
  # gdppopShift shifts the S-curve horizontally to match the reference value
  gdppopShift <- fitData %>%
    group_by(across(all_of(c("region")))) %>%
    slice_max(order_by = .data$period, n = 1, with_ties = FALSE) %>%
    ungroup() %>%

    mutate(gdppopShift = .data$gdppop - ((alpha - log(1 / .data$penetration - 1)) /
                                           (beta * .data$CDD^gamma))^(1 / delta)) %>%
    select("region", "gdppopShift") %>%

    # fill missing regions with zero shift (use global curve as-is)
    right_join(gdppop %>%
                 select("region") %>%
                 unique(),
               by = "region") %>%
    mutate(gdppopShift = ifelse(is.na(.data$gdppopShift) | is.infinite(.data$gdppopShift), 0, .data$gdppopShift))


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

    left_join(gdppopShift, by = "region") %>%
    replace_na(list("gdppopShift" = 0)) %>%

    mutate(
      value = ifelse(
        .data$gdppopShift < .data$gdppop,
        1 / (1 + exp(alpha - beta * (.data$gdppop - .data$gdppopShift)^delta * .data$CDD^gamma)),
        # extrapolation towards really low adoption values if GDP shift exceed GDP
        # increases monotonously with GDP and continues the actual formula above
        # otherwise arbitrary, but only affects adoption values close to zero
        1 / (1 + exp(alpha)) / (1 - (.data$gdppop - .data$gdppopShift) / 10000)
      ),
      variable = "ac ownership rate"
    ) %>%

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
