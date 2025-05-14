#' Read buildings characteristics data from the Built Environment Analysis Model (BEAM^2)
#'
#' @author Hagen Tockhorn
#'
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte

readBEAM2 <- function() {
  data <- read.csv("buildingCharacteristics_BEAM2.csv") %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
