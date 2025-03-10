#' Calculate Enduse Shares from TCEP data base
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr %>% select mutate group_by
#' @importFrom rlang .data
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#'
#' @export

calcShareTCEP <- function() {

  # READ-IN DATA ---------------------------------------------------------------

  data <- readSource("TCEP") %>%
    as.quitte()


  # PROCESS DATA ---------------------------------------------------------------

  # end-use shares
  shares <- data %>%
    select("region", "period", "enduse", "value") %>%
    toolCalcShares(colShare = "enduse") %>%
    as.quitte() %>%
    as.magpie()

  # weights
  regShare <- data %>%
    select("region", "period", "enduse", "value") %>%
    toolCalcShares(colShare = c("region", "enduse")) %>%
    as.quitte() %>%
    as.magpie()


  # OUTPUT ---------------------------------------------------------------------

  return(list(
    x = shares,
    weight = regShare,
    min = 0,
    max = 1,
    unit = "",
    description = "Enduse Shares from TCEP data"
  ))
}
