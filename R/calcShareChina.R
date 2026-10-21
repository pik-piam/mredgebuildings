#' Calculate shares from final energy end-use demand for China
#'
#' The data is obtained from the 2021 IEA report "An Energy Sector Roadmap to Carbon
#' Neutrality in China" from Figure 3.25 and 2024 IEA report "The Future of Heat Pumps
#' in China" from Figure 1.6.
#'
#' The latter report states in Box 1.1 that district heating has been incorrectly assigned
#' to water heating by the IEA, thus significantly overestimating the demand and underestimating
#' demand for space heating. They corrected their numbers using data from the
#' Tsinghua University's Annual Report on China Building Energy Efficiency in 2022 which
#' are used here to correct the space_heating/water_heating split.
#'
#' @author Robin Hasse, Hagen Tockhorn
#'
#' @importFrom madrat readSource toolCountryFill
#' @importFrom magclass dimSums
#' @importFrom dplyr filter group_by ungroup left_join mutate select across recode_values

calcShareChina <- function() {
  roadmapReportData <- readSource("IEA_China", subtype = "EnergySectorRoadmap") %>%
    as.quitte(na.rm = TRUE)

  heatPumpReportData <- readSource("IEA_China", subtype = "FutureOfHeatPumps") %>%
    as.quitte(na.rm = TRUE)

  heatSplit <- heatPumpReportData %>%
    filter(.data$scenario %in% c("history", "APS")) %>%
    pivot_wider(names_from = "enduse", values_from = "value") %>%
    mutate(spaceHeatShare = .data$space_heating / (.data$space_heating + .data$water_heating)) %>%
    select("region", "period", "spaceHeatShare") %>%
    interpolate_missing_periods(period = unique(roadmapReportData$period),
                                value = "spaceHeatShare",
                                expand.values = TRUE)

  x <- roadmapReportData %>%
    left_join(heatSplit, by = c("region", "period")) %>%
    group_by(across(-all_of(c("enduse", "value", "spaceHeatShare")))) %>%
    mutate(totalHeat = sum(.data$value[.data$enduse %in% c("space_heating", "water_heating")])) %>%
    ungroup() %>%
    mutate(value = recode_values(.data$enduse,
      "space_heating" ~ .data$totalHeat * .data$spaceHeatShare,
      "water_heating" ~ .data$totalHeat * (1 - .data$spaceHeatShare),
      default = .data$value
    )) %>%
    select("region", "period", "enduse", "value") %>%
    as.magpie()

  x <- x[, , "other", invert = TRUE]
  shares <- x / dimSums(x, "enduse")
  shares <- toolCountryFill(shares, verbosity = 2)
  weights <- toolCountryFill(x, 1, verbosity = 2)
  list(x = shares,
       weight = weights,
       min = 0,
       max = 1,
       unit = "1",
       description = "Share of end uses in Chinese buildings final energy demand")
}
