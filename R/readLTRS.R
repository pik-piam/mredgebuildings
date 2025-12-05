#' Read Long Term Renovation Strategy
#'
#' In the LTRS, EU member states have to describe their building stock.
#' Unfortunately, this isn't standardised in any way so it is hard to make use
#' of this data. So far, we only use data from two figures in the German LTRS.
#'
#' @returns MagPIE object
#'
#' @author Robin Hasse
#'
#' @importFrom magclass as.magpie
#' @importFrom utils read.csv

readLTRS <- function() {
  file <- file.path("DEU", "ltrs_DEU_fig7-8.csv")
  data <- read.csv(file, encoding = "UTF-8")
  as.magpie(data, tidy = FALSE)
}
