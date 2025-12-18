#' Calculate Air Conditioner Ownership Regression Parameters
#'
#' This function estimates global regression parameters for air conditioner (AC)
#' ownership rates using a logistic function with GDP per capita and cooling degree
#' days (CDD) as predictors. The function fits a non-linear regression model to
#' derive parameters for the AC penetration logistic curve.
#'
#' The regression model follows the logistic formula:
#' \deqn{penetration = 1 / (1 + \exp(\alpha - \beta \cdot gdppop^\delta \cdot CDD^\gamma))}
#'
#' where:
#' \itemize{
#'   \item \eqn{\alpha} is the intercept parameter that sets the lower boundary of AC ownership
#'   \item \eqn{\beta} is the interaction coefficient (negated from fitted model coefficient b)
#'   \item \eqn{\gamma} is the CDD exponent parameter
#'   \item \eqn{\delta} is the GDP per capita exponent parameter
#' }
#'
#' The function performs a two-stage estimation process:
#' \enumerate{
#'   \item Alpha is set exogenously based on a minimum ownership rate assumption
#'   \item Linear regression on log-transformed data with fixed alpha to obtain starting values for nls()
#'   \item Non-linear regression using nls() with alpha fixed, estimating only beta, gamma, and delta
#' }
#'
#' @returns magpie object with global regression parameters (alpha, beta, gamma, delta)
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr %>% .data across all_of filter group_by left_join reframe
#'   select
#' @importFrom madrat calcOutput
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte
#' @importFrom stats coef lm nls
#' @importFrom tibble as_tibble

calcACOwnershipRegression <- function() {

  # PARAMETERS -----------------------------------------------------------------

  # Minimum AC ownership rate
  minOwnershipRate <- 0.01


  # READ-IN DATA ---------------------------------------------------------------

  # AC ownership rates
  ownershipRates <- calcOutput("ACOwnershipRates", aggregate = FALSE) %>%
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



  # PROCESS DATA ---------------------------------------------------------------

  # bind full data set
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


  # ASSUMPTIONS ----------------------------------------------------------------

  ## Minimum AC Ownership Rate ====

  # Accounting for cultural/behavioral influences reflected in disproportionally
  # low historical reference values, the alpha parameter is fixed exogenously
  # based on a minimum ownership rate assumption, chosen in accordance with the
  # available reference data.

  alpha <- log(1 / minOwnershipRate - 1)
  fitData$alpha <- alpha


  # FIT REGRESSION MODEL -------------------------------------------------------

  # linear fit with fixed alpha for starting values
  estimateLin <- lm("log(1/penetration - 1) - alpha ~ 0 + gdppop:CDD", data = fitData)

  # assign start values
  startValues <- list(b = estimateLin$coefficients[["gdppop:CDD"]],
                      c = 1,
                      d = 1)

  # non-linear fit with alpha fixed
  estimateNonLin <- nls("penetration ~ 1 / (1 + exp(alpha + b * CDD^c * gdppop^d))",
                        data = fitData,
                        start = startValues,
                        control = list(maxiter = 1000))


  # extract fit parameters to data frame
  fitPars <- data.frame(region   = "GLO",
                        variable = c("alpha", "beta", "gamma", "delta"),
                        value    = c(alpha,
                                     (-1) * coef(estimateNonLin)[["b"]],
                                     coef(estimateNonLin)[["c"]],
                                     coef(estimateNonLin)[["d"]]))



  # OUTPUT ---------------------------------------------------------------------

  fitPars <- fitPars %>%
    as.magpie()

  return(list(x = fitPars,
              isocountries = FALSE,
              unit = "",
              description = "Regression parameter for AC ownership rates"))

}
