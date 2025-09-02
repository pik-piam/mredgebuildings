#' Calculate Air Conditioner Ownership Regression Parameters
#'
#' This function estimates global regression parameters for air conditioner (AC)
#' ownership rates using a logistic function with GDP per capita and cooling degree
#' days (CDD) as predictors. The function fits a non-linear regression model to
#' derive parameters for the AC penetration logistic curve.
#'
#' The regression model follows the logistic formula:
#' \deqn{penetration = 1 / (1 + \exp(\alpha - \beta \cdot gdppop^\gamma \cdot CDD^\delta))}
#'
#' where:
#' \itemize{
#'   \item \eqn{\alpha} is the intercept parameter
#'   \item \eqn{\beta} is the interaction coefficient (inverted from model coefficient b)
#'   \item \eqn{\gamma} is the CDD exponent parameter
#'   \item \eqn{\delta} is the GDP per capita exponent parameter
#' }
#'
#' The function performs a two-stage estimation process:
#' \enumerate{
#'   \item Linear regression on log-transformed data to obtain starting values
#'   \item Non-linear regression using nls() to estimate final parameters
#' }
#'
#' @return data.frame with regression parameters
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr select filter left_join group_by reframe across all_of
#' @importFrom tibble as_tibble
#' @importFrom quitte as.quitte
#' @importFrom stats lm nls coef

calcACOwnershipRegression <- function() {

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
  hddcdd <- calcOutput("HDDCDD", aggregate = FALSE) %>%
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
                reframe(CDD = mean(.data$value)))


  # linear fit to obtain start values for non-linear fit
  estimateLin <- lm("log(1/penetration - 1) ~ gdppop:CDD", data = fitData)


  # assign start values
  startValues <- list(a = estimateLin$coefficients[["(Intercept)"]],
                      b = estimateLin$coefficients[["gdppop:CDD"]],
                      c = 1,
                      d = 1)

  # non-linear fit
  estimateNonLin <- nls("penetration ~ 1 / (1 + exp(a + b * CDD^c * gdppop^d))",
                        data = fitData,
                        start = startValues,
                        control = list(maxiter = 1000))


  # extract fit parameters to data frame
  fitPars <- data.frame(region   = "GLO",
                        variable = c("alpha", "beta", "gamma", "delta"),
                        value    = c(
                          coef(estimateNonLin)[["a"]],
                          (-1) * coef(estimateNonLin)[["b"]],
                          coef(estimateNonLin)[["c"]],
                          coef(estimateNonLin)[["d"]]
                        ))



  # OUTPUT ---------------------------------------------------------------------

  fitPars <- fitPars %>%
    as.magpie()

  return(list(x = fitPars,
              isocountries = FALSE,
              unit = "",
              description = "Regression parameter for AC ownership rates"))

}
