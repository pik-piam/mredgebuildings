#' Read final energy data by energy end-use for China
#'
#' The data is obtained from the 2021 IEA report "An Energy Sector Roadmap to Carbon
#' Neutrality in China" from Figure 3.25
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr select mutate
#' @importFrom magclass as.magpie
#' @importFrom utils read.csv

readIEA_China <- function() { #nolint object_name_linter
  read.csv("figure3-25.csv") %>%
    select("region", "period", "enduse", "value") %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value",
              tidy = FALSE)
}
