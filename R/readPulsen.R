#' Read Pulsen survey on Swedish heat pump market
#'
#' @source https://skvp.se/skvpold/statistik/pulsen
#'
#' @author Robin Hasse
#'
#' @returns MagPIE object
#'
#' @importFrom dplyr mutate across
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

readPulsen <- function() {
  read.csv2("pulsen_2023_hpReplace.csv") %>%
    mutate(across(.data$value, as.numeric)) %>%
    as.quitte() %>%
    as.magpie()
}
