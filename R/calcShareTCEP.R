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

  shares <- data %>%
    select("region", "period", "enduse", "value") %>%
    group_by(across(all_of(c("region", "period")))) %>%
    mutate(value = .data[["value"]] / sum(.data[["value"]], na.rm = TRUE)) %>%
    ungroup()


  # OUTPUT ---------------------------------------------------------------------

  shares <- as.magpie(shares)

  return(list(
    x = shares,
    weights = NULL,
    min = 0,
    max = 1,
    unit = "",
    description = "Enduse Shares from TCEP data"
  ))
}
