#' Read IEA Global Weighted Average of Air Conditioning Efficiencies
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr mutate
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie

readIEA_coolingEfficiencies <- function() { #nolint object_name_linter

  data <- read.csv("coolingEfficienciesGLO.csv")

  data <- data %>%
    mutate(carrier = "elec",
           enduse = "space_cooling",
           region = "GLO") %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
