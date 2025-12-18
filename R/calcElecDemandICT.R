#' Calculate Electricity Demand for ICT from Data Center Demands
#'
#' This function processes historical and scenario electricity demand data for
#' Information and Communication Technology (ICT) and data centers (DC). The initial data
#' comes from exogenous projections using the Digital Transformation Level (DGI)
#' as the primary driver of demand, with IEA data serving as the historical baseline.
#'
#' The function processes multiple demand estimates:
#' \itemize{
#'   \item Mean and central estimate
#'   \item Lower demand boundary (DGI elasticity from historical efficiency dominant phases)
#'   \item Upper demand boundary (DGI elasticity from historical service dominant phases)
#' }
#'
#' Data is downscaled from R5 to country-level using historical DC counts,
#' with fixed network shares calculated at the end of the historical period. Historical
#' data (IEA Base) is separated and harmonized with SSP scenario projections.
#'
#' @param endOfHistory upper temporal boundary for historical data
#' @param extrapolate if \code{TRUE}, regional demand will be linearly extrapolated using the slope
#' of the last two existing data points per scenario, if \code{FALSE} data remains unchanged
#'
#' @returns A magpie object containing ICT electricity demand data calculated from
#'   DC demands across regions, time periods, and scenarios.
#'
#' @author Hagen Tockhorn
#'
#' @importFrom madrat readSource toolGetMapping
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter mutate group_by reframe ungroup rename
#'   select across all_of .data %>% cross_join left_join
#' @importFrom tidyr pivot_wider pivot_longer replace_na
#' @importFrom purrr map_dfr
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom magclass as.magpie
#' @importFrom madrat toolCountryFill

calcElecDemandICT <- function(endOfHistory = 2025, extrapolate = FALSE) {

  # PARAMETER ------------------------------------------------------------------

  # TWh to EJ conversion factor
  twh2ej <- 3600 * 10^12 / 10^18



  # READ-IN DATA ---------------------------------------------------------------

  # R12 Resolution (Elec. Demand DC + ICT)
  dataR12 <- readSource("PRISMA_ICT", subtype = "R12", convert = FALSE) %>%
    as_tibble() %>%
    filter(!is.na(.data$value))

  # R5 Resolution (Elec. Demand DC)
  dataR5 <- readSource("PRISMA_ICT", subtype = "R5", convert = FALSE) %>%
    as_tibble() %>%
    filter(!is.na(.data$value))

  # Number of DCs
  dataDC <- readSource("PRISMA_ICT", subtype = "nDC") %>%
    as_tibble() %>%
    filter(!is.na(.data$value))


  # IEA R5 region mapping
  regionmap <- toolGetMapping("regionmappingIEA_R5.csv",
                              type = "regional",
                              where = "mredgebuildings")

  # variable mapping
  variableMap <- toolGetMapping("variableMapping_ICT.csv",
                                type = "sectoral",
                                where = "mredgebuildings")
  variableMap <- setNames(variableMap$variableTarget, variableMap$variable)



  # PROCESS DATA ---------------------------------------------------------------

  # re-map variables in R5 and R12
  dataR12 <- dataR12 %>%
    mutate(variable = recode(.data$variable, !!!variableMap)) %>%
    separate(col = "variable", into = c("variable", "estimate"), sep = " ")

  dataR5 <- dataR5 %>%
    mutate(variable = recode(.data$variable, !!!variableMap)) %>%
    separate(col = "variable", into = c("variable", "estimate"), sep = " ")


  # calculate global network/DC ratio from R12 data
  networkDCRatio <- dataR12 %>%
    group_by(across(all_of(c("variable", "estimate", "period")))) %>%
    reframe(value = sum(.data$value)) %>%
    ungroup() %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    mutate(ratio = .data$ict / .data$dc - 1, .keep = "unused")


  data <- dataR5 %>%
    # harmonize historical and scenario data
    (\(df) {
      # split IEA (Base) into historical and post-historical periods
      ieaHistorical <- filter(df, .data$scenario == "IEA (Base)",
                              .data$period <= endOfHistory) %>%
        mutate(scenario = "history")

      ieaScenario <- filter(df, .data$scenario == "IEA (Base)",
                            .data$period > endOfHistory)

      # append IEA future data to each SSP scenario
      ieaScenarioExpanded <- map_dfr(grep("^SSP", unique(df$scenario), value = TRUE), function(ssp) {
        ieaScenario %>%
          mutate(scenario = ssp)
      })

      # combine: historical + SSP scenarios + expanded IEA future
      rbind(
        ieaHistorical,
        filter(df, grepl("^SSP", .data$scenario)),
        ieaScenarioExpanded
      )
    })() %>%

    # add country-level weights (number of DCs)
    rename("regionTarget" = "region") %>%
    left_join(regionmap %>%
                select("region" = "CountryCode", "regionTarget" = "Region"),
              by = "regionTarget",
              relationship = "many-to-many") %>%
    left_join(dataDC %>%
                select("region", "weight" = "value"),
              by = "region") %>%

    # disaggregate to ISO country level
    group_by(across(all_of(c("regionTarget", "period", "variable", "scenario", "estimate")))) %>%
    mutate(value = .data$value * .data$weight / sum(.data$weight, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-"regionTarget", -"weight") %>%

    # calculate ICT (DC + Network) demand using global network share
    left_join(networkDCRatio, by = c("estimate", "period")) %>%
    mutate(value = .data$value * (1 + .data$ratio), .keep = "unused") %>%
    select(-"variable", -"unit") %>%
    rename("variable" = "estimate") %>%

    # convert TWh/yr to EJ/yr
    mutate(value = .data$value * twh2ej,
           carrier = "elec",
           enduse = "ict") %>%
    replace_na(list("value" = 0))


  # extrapolate scenario data to 2100 using linear fit over last two periods (here: 2045-50)
  if (isTRUE(extrapolate)) {
    data <- data %>%
      group_by(across(all_of(c("region", "scenario", "variable")))) %>%
      group_modify(~ {
        periods <- sort(unique(.x$period))

        if (.y$scenario != "history") {
          # linear extrapolation for future scenarios
          fitData <- .x %>%
            filter(.data$period %in% tail(periods, 2))

          model <- lm(value ~ period, data = fitData)

          .x %>%
            interpolate_missing_periods(seq.int(endOfHistory + 5, 2100, 5)) %>%
            mutate(pred = predict(model, newdata = .),
                   value = ifelse(is.na(.data$value), pmax(0, .data$pred), .data$value)) %>%
            select(-"pred")
        } else {
          .x
        }
      }) %>%
      ungroup()
  }



  # weights
  weights <- dataDC %>%
    select("region", "value") %>%
    mutate(region = as.character(.data$region)) %>%
    right_join(data %>%
                 select("region", "period", "variable") %>%
                 unique(),
               by = "region") %>%
    replace_na(list("value" = 0))



  # OUTPUT ---------------------------------------------------------------------

  data <- data %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(0, verbosity = 2)

  weights <- weights %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(0, verbosity = 2)


  return(list(x = data,
              weight = weights,
              unit = "EJ",
              min = 0,
              description = "Annual historical and SSP ICT electricity demands"))

}
