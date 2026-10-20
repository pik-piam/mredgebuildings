#' Read final energy data by energy end-use for China
#'
#' The data is obtained from the either the 2021 IEA report "An Energy Sector Roadmap to Carbon
#' Neutrality in China" from Figure 3.25 or the 2024 IEA report "The Future of Heat Pumps
#' in China" from Figure 1.6.
#'
#' @param subtype indicates source to use
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr select mutate
#' @importFrom magclass as.magpie
#' @importFrom utils read.csv

readIEA_China <- function(subtype = c("EnergySectorRoadmap", "FutureOfHeatPumps")) { #nolint object_name_linter
  if (subtype == "EnergySectorRoadmap") {
    read.csv("figure3-25.csv") %>%
      select("region", "period", "enduse", "value") %>%
      as.magpie(spatial = "region", temporal = "period", datacol = "value",
                tidy = FALSE)
  } else if (subtype == "FutureOfHeatPumps") {
    read.csv("figure1-6.csv") %>%
      select("region", "period", "enduse", "scenario", "value") %>%
      as.magpie(spatial = "region", temporal = "period", datacol = "value",
                tidy = FALSE)
  } else {
    stop("Unknown subtype: ", subtype)
  }
}
