#' Calculate Electricity Demand for ICT from Data Center Demands
#'
#' This function processes historical and scenario electricity demand data for
#' Information and Communication Technology (ICT) and data centers (DC). The initial data
#' comes from exogenous projections using the Digital Transformation Level (DGI)
#' as the primary driver of demand, with IEA data serving as the historical baseline.
#'
#' The function processes multiple demand estimates:
#' \itemize{
#'   \item Mean and medium estimate
#'   \item Lower demand boundary (DGI elasticity from historical efficiency dominant phases)
#'   \item Upper demand boundary (DGI elasticity from historical service dominant phases)
#' }
#'
#' Data is downscaled from R5 to country-level using data center counts as weights,
#' with exceptional fixed shares for China within Asia Pacific (73.28%) and for Europe's
#' R12 sub-regions (WEU: 75%, EEU: 14%, FSU: 11%). Network demand is added using global network/DC ratios.
#' Historical data (IEA Base) is separated and harmonized with SSP scenario projections.
#'
#' @param endOfHistory upper temporal boundary for historical data
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

calcElecDemandICT <- function(endOfHistory = 2025) {

  # PARAMETER ------------------------------------------------------------------

  # TWh to EJ conversion factor
  twh2ej <- 3600 * 10^12 / 10^18

  # fixed share of ICT demand China <-> Asia Pacific
  chinaShare <- 0.7328

  # fixed shares of R12 regions within R5 Europe
  europeShares <- data.frame(
    "R12" = c("WEU", "EEU", "FSU"),
    "share" = c(0.75, 0.14, 0.11)
  )


  # READ-IN DATA ---------------------------------------------------------------

  # R12 Resolution (Elec. Demand DC + ICT)
  dataR12 <- readSource("PRISMA_ICT", subtype = "R12", convert = FALSE) %>%
    as_tibble() %>%
    filter(!is.na(.data$value))

  # R5 Resolution (Elec. Demand DC)
  dataR5 <- readSource("PRISMA_ICT", subtype = "R5 2100", convert = FALSE) %>%
    as_tibble() %>%
    filter(!is.na(.data$value))

  # Number of DCs
  dataDC <- readSource("PRISMA_ICT", subtype = "nDC") %>%
    as_tibble() %>%
    filter(!is.na(.data$value))


  # MessageIX region mapping
  regionmapMessage <- toolGetMapping("regionmappingMessageIX.csv",
                                     type = "regional",
                                     where = "mredgebuildings")

  # IEA R5 region mapping
  regionmapR5 <- toolGetMapping("regionmappingIEA_R5.csv",
                                type = "regional",
                                where = "mredgebuildings")


  # variable mapping
  variableMap <- toolGetMapping("variableMapping_ICT.csv",
                                type = "sectoral",
                                where = "mredgebuildings")
  variableMap <- setNames(variableMap$variableTarget, variableMap$variable)



  # PROCESS DATA ---------------------------------------------------------------

  # unite region mappings (ISO -> R5 -> R12)
  regionmap <- regionmapR5 %>%
    select("region" = "CountryCode", "R5" = "Region") %>%
    left_join(regionmapMessage %>%
                select("region" = "CountryCode", "R12"),
              by = "region")


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
    mutate(ratio = .data$ict / .data$dc - 1, .keep = "unused") %>%
    interpolate_missing_periods(unique(dataR5$period), expand.values = TRUE, value = "ratio")


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
    rename("R5" = "region") %>%
    left_join(regionmap, by = "R5", relationship = "many-to-many") %>%

    # Step 1: Disaggregate regions with fixed disaggregation shares (China + Europe)
    (\(df) {
      # disaggregate China from Asia Pacific with fixed share
      chinaDisagg <- df %>%
        filter(.data$R5 == "Asia Pacific") %>%
        mutate(share = ifelse(.data$region == "CHN", chinaShare, 1 - chinaShare),
               value = .data$value * .data$share,
               regionTarget = ifelse(.data$region == "CHN", "CHN", .data$R5)) %>%
        select(-"share", -"R5", -"R12")

      # disaggregate Europe into R12 regions with fixed shares
      europeDisagg <- df %>%
        filter(.data$R5 == "Europe") %>%
        left_join(europeShares, by = "R12") %>%
        mutate(value = .data$value * .data$share,
               regionTarget = .data$R12) %>%
        select(-"share", -"R5", -"R12")

      # filter remaining regions
      rest <- df %>%
        filter(!.data$R5 %in% c("Asia Pacific", "Europe")) %>%
        mutate(regionTarget = .data$R5) %>%
        select(-"R5", -"R12")

      # bind all data
      rbind(chinaDisagg, europeDisagg, rest)
    })() %>%

    # Step 2: Disaggregate within each regionTarget using DC counts
    left_join(dataDC %>%
                select("region", "weight" = "value"),
              by = "region") %>%

    # disaggregate to ISO country level
    group_by(across(all_of(c("regionTarget", "period", "variable", "scenario", "estimate")))) %>%
    mutate(value = .data$value * .data$weight / sum(.data$weight, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-"regionTarget", -"weight") %>%

    # calculate ICT (DC + Network) demand using global network / DC ratio
    left_join(networkDCRatio, by = c("estimate", "period")) %>%
    mutate(value = .data$value * (1 + .data$ratio), .keep = "unused") %>%
    select(-"variable", -"unit") %>%
    rename("variable" = "estimate") %>%

    # convert TWh/yr to EJ/yr
    mutate(value = .data$value * twh2ej,
           carrier = "elec",
           enduse = "ict") %>%
    replace_na(list("value" = 0))



  # OUTPUT ---------------------------------------------------------------------

  data <- data %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(0, verbosity = 2)


  return(list(x = data,
              unit = "EJ",
              min = 0,
              description = "Annual historical and SSP ICT electricity demands"))

}
