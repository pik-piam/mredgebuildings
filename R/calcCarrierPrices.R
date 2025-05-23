#' Calculate carrier prices
#'
#' Final energy carrier prices including energy supply cost, transport and
#' distribution cost(T&D) and taxes (without VAT). CO2 price component and VAT
#' are added later depending on the scenario assumptions.
#'
#' @author Robin Hasse
#'
#' @importFrom madrat readSource
#' @importFrom dplyr %>% .data select mutate group_by across all_of any_of
#'   summarise left_join full_join cross_join rename
#' @importFrom tidyr pivot_longer complete
#' @importFrom quitte as.quitte inline.data.frame interpolate_missing_periods
#' @importFrom madrat toolGetMapping
#' @export

calcCarrierPrices <- function() {

  # temporal scope
  periodsPast <- 2000:2022
  periodsFuture <- seq(2025, 2100, 5)
  periods <- c(periodsPast, periodsFuture)

  # levels of future projections
  lvs <- c("low", "central", "high")

  # unit conversion: EUR -> USD
  usd2eur <- usd2eur()


  # Energy prices --------------------------------------------------------------

  # prices w/o VAT w/o CO2 price


  ## district heating ====

  dhPrices <- readSource("Energiforsk2016") %>%
    toolCountryFillAvg(verbosity = 2) %>%
    as.quitte(na.rm = TRUE) %>%
    select("region", "period", "value") %>%
    mutate(carrier = "heat",
           value = .data[["value"]] * 3.6E-3 / usd2eur) # EUR/GJ to USD/kWh

  # extrapolate prices linearly for past periods, constant in future
  extrapolate <- function(p, v, until) {
    linModel <- lm(value ~ period, data.frame(period = p, value = v))
    vUntil <- predict(linModel, newdata = data.frame(period = until))[[1]]
    return(c(v, vUntil))
  }
  dhPrices <- dhPrices %>%
    group_by(across(all_of(c("region", "carrier")))) %>%
    reframe(value = extrapolate(.data[["period"]], .data[["value"]],
                                max(periodsPast)),
            period = c(.data[["period"]], max(periodsPast))) %>%
    ungroup() %>%
    interpolate_missing_periods(periods, expand.values = TRUE) %>%
    filter(.data[["period"]] %in% periods)


  ## other carriers ====

  # map ECEMF carriers
  # assumption: liquids and gases remain fossil
  carrierMap <- toolGetMapping("carrierMapping_ECEMF.csv",
                               type = "sectoral",
                               where = "mredgebuildings")

  # all considered price components
  # (drop CO2 price as this is a scenario assumption)
  components <- c("energy",
                  "T&D&Sales markup",
                  "Tax markup (w/o VAT)",
                  "Pellet processing markup")

  # price assumptions for all carriers except district heating
  ecemfPrices <- readSource("ECEMF", "FEPrices") %>%
    toolCountryFillAvg(verbosity = 2) %>%
    as.quitte(na.rm = TRUE) %>%
    left_join(carrierMap, by = c(carrier = "carrierECEMF")) %>%
    filter(.data[["sector"]] %in% c("Res&Com", "Supply"),
           !is.na(.data[["carrier.y"]]),
           .data[["component"]] %in% components) %>%
    select("region", "period", carrier = "carrier.y", "component", "value") %>%
    group_by(across(-all_of(c("component", "value")))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
    ungroup() %>%
    mutate(value = .data[["value"]] * 1E-3 / usd2eur) %>% # EUR/MWh to USD/kWh
    interpolate_missing_periods(periods, expand.values = TRUE) %>%
    filter(.data[["period"]] %in% periods)

  # all prices
  prices <- rbind(ecemfPrices, dhPrices) %>%
    mutate(variable = "price",
           unit = "USD/kWh",
           level = "central")



  # Emission intensity ---------------------------------------------------------


  ## past ====

  # extremely rough assumption based on remind results
  emiHeat <- data.frame(period  = c(2000, 2022),
                        value   = c(4E-4, 3E-4),
                        unit    = "t_CO2/kWh") %>%
    interpolate_missing_periods(periodsPast, expand.values = TRUE) %>%
    filter(.data[["period"]] %in% periodsPast) %>%
    mutate(carrier = "heat")

  # historic emission intensity from EEA
  emiElec <- readSource("EEA_EuropeanEnvironmentAgency",
                        "ghgEmissionIntensityElec") %>%
    toolCountryFillAvg() %>%
    as.quitte(na.rm = TRUE) %>%
    interpolate_missing_periods(periodsPast, expand.values = TRUE) %>%
    filter(.data[["period"]] %in% periodsPast) %>%
    select("region", "period", "value") %>%
    mutate(value = .data[["value"]] * 1E-6,  # g/kWh to t/kWh
           unit = "t_CO2/kWh",
           carrier = "elec")

  # emission intensity of other carriers
  emiOther <- readSource("ECEMF", "emissionFactor") %>%
    as.quitte(na.rm = TRUE) %>%
    left_join(carrierMap, by = c(carrier = "carrierECEMF")) %>%
    select(carrier = "carrier.y", "value") %>%
    rbind(data.frame(carrier = "biomod", value = 0)) %>% # set biomod to zero
    mutate(value = .data[["value"]] * 1E-3,  # t/MWh to t/kWh
           unit = "t_CO2/kWh")

  # all emission factors
  emi <- Reduce(x = list(emiHeat, emiElec, emiOther), f = function(x, y) {
    cols <- setdiff(intersect(colnames(x), colnames(y)), c("carrier", "value"))
    fun <- if (length(cols) > 0) full_join else cross_join
    args <- lapply(list(x, y), pivot_wider, names_from = "carrier")
    if (identical(fun, full_join)) args <- c(args, list(by = cols))
    do.call(fun, args) %>%
      pivot_longer(cols = -any_of(union(colnames(x), colnames(y))),
                   names_to = "carrier")
  })


  ## future ====

  # assume long-term residual emission intensity
  emi <- expand.grid(carrier = c("elec", "heat"),
                     level = lvs) %>%
    mutate(period = ifelse(.data[["level"]] == "low",
                           ifelse(.data[["carrier"]] == "elec",
                                  2040,
                                  2045),
                           2050),
           factor = ifelse(.data[["level"]] == "low",
                           0,
                           ifelse(.data[["level"]] == "central",
                                  ifelse(.data[["carrier"]] == "elec",
                                         0.1,
                                         0.2),
                                  1))) %>%
    complete(carrier = unique(emi$carrier),
             level = lvs,
             fill = list(factor = 1, period = max(periodsFuture))) %>%
    full_join(emi %>%
                filter(.data[["period"]] == max(periodsPast)) %>%
                select(-"period"),
              by = "carrier",
              relationship = "many-to-many") %>%
    mutate(value = .data[["value"]] * .data[["factor"]]) %>%
    select(-"factor") %>%
    rbind(cross_join(emi, data.frame(level = lvs))) %>%
    interpolate_missing_periods(periods, expand.values = TRUE) %>%
    filter(.data[["period"]] %in% periods) %>%
    mutate(variable = "emi",
           unit = "t_CO2/kWh")



  # Combine data ---------------------------------------------------------------

  data <- rbind(prices, emi)

  # all carriers included?
  carrier <- toolGetMapping("dim_hs.csv",
                            type = "sectoral", where = "brick") %>%
    select("carrier") %>%
    unique()
  data <- data %>%
    right_join(carrier, by = "carrier")
  if (any(is.na(data))) {
    stop("Incomplete mapping of energy carriers.")
  }

  data <- data  %>%
    as.quitte() %>%
    as.magpie()

  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    dimSums("typ") %>%
    time_interpolate(periods, extrapolation_type = "constant")

  return(list(x = data,
              unit = "USD2020/kWh or t_CO2/kWh",
              weight = feBuildings,
              min = 0,
              description = "Components of FE carrier prices"))
}
