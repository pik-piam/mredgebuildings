#' Read building life time data from Sandberg et al. 2016
#'
#' @source https://doi.org/10.1016/j.enbuild.2016.05.100
#'
#' @returns MagPIE object with life time parameters of buildings
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr .data mutate
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @importFrom magclass as.magpie
#' @export

readSandberg <- function() {
  data <- read.csv("table1.csv")
  units <- unlist(as.list(data[1, ]))
  data[2:nrow(data), ] %>%
    pivot_longer(which(unname(units) != ""), names_to = "variable") %>%
    mutate(unit = units[.data$variable],
           value = as.numeric(.data$value),
           .before = "value") %>%
    as.magpie(spatial = "country")
}
