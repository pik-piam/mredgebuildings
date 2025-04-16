#' Calculate shares of carriers or enduses in buildings energy demand from IEA EEI data
#'
#' This function calculates the proportion of energy demand by carrier or enduse
#' in the buildings sector, based on IEA Energy Efficiency Indicators data.
#'
#' @param subtype Character, specifies which type of share to calculate. Must be either
#'                "enduse" or "carrier". Default is "enduse".
#'
#' @returns A data.frame with the respective shares
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr %>% as_tibble group_by across all_of mutate reframe ungroup

calcShareIEA_EEI <- function(subtype = c("enduse", "carrier")) { #nolint object_name_linter

  # READ-IN DATA ---------------------------------------------------------------

  data <- calcOutput("IEA_EEI",
                     subtype = "buildings",
                     mixData = TRUE,
                     aggregate = FALSE,
                     warnNA = FALSE) %>%
    as_tibble()


  # PROCESS DATA ---------------------------------------------------------------

  # match subtypes
  subtype <- match.arg(subtype)

  # calculate shares
  shares <- data %>%
    droplevels() %>%
    group_by(across(all_of(c("region", "period", subtype)))) %>%
    reframe(value = sum(.data$value, na.rm = TRUE)) %>%
    group_by(across(c("region", "period"))) %>%
    mutate(value = proportions(.data$value)) %>%
    ungroup()


  # calculate weights
  weights <- data %>%
    droplevels() %>%
    group_by(across(all_of(c("region", "period", subtype)))) %>%
    reframe(value = sum(.data$value, na.rm = TRUE))


  # OUTPUT ---------------------------------------------------------------------

  shares <- shares %>%
    as.quitte() %>%
    as.magpie()

  weights <- weights %>%
    as.quitte() %>%
    as.magpie()

  return(list(x = shares,
              weight = weights,
              unit = "1",
              min = 0,
              max = 1,
              description = "Share of carrier or enduse in buildings energy demand from IEA EEI data"))
}
