#' Historical floor space demand
#'
#' Historical residential and commercial floor space per capita is compiled from
#' several data sources. For each region, only a single source is used: going
#' down a fixed priority (Odyssee > IEA EEI > GLOBUS > Daioglou > GEM > EEA), the
#' highest-priority source that provides any data point for a region contributes
#' all of its data points (all variables and periods). Individual regions can be
#' reassigned to a specific source via a fixed list of exceptions. Missing
#' regions and periods are subsequently filled by a
#' sectorial regression on GDP per capita (and, for residential, population
#' density) that is performed region-wise if enough data points exist. If not,
#' projections are scaled by existing data.
#'
#' @param endOfHistory upper temporal boundary for historical data
#'
#' @returns MAgPIE object with historic floor space
#'
#' @author Robin Hasse, Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom madrat readSource calcOutput toolCountryFill
#' @importFrom quitte as.quitte factor.data.frame interpolate_missing_periods
#'   calc_addVariable
#' @importFrom dplyr filter mutate select group_by left_join %>% .data ungroup
#'   group_modify recode across all_of bind_rows rename anti_join inner_join
#' @importFrom magclass mbind as.magpie collapseDim mselect getItems
#' @importFrom tidyr replace_na pivot_wider pivot_longer
#' @export

calcFloorspacePast <- function(endOfHistory = 2025) {

  # FUNCTIONS ------------------------------------------------------------------

  # A region-specific linear regression given by 'formula' is
  # fitted for every region that provides enough observations. Regions with too
  # few (or no) observations fall back to a global regression fitted across all
  # regions. Predicted values are corrected w.r.t. historical data where
  # available, otherwise the prediction is chosen. 'gdppop' and 'density' are
  # expected to already carry the columns referenced by 'formula'.
  projectFloorspace <- function(df, gdppop, dens, formula, periodBegin, endOfHistory) {

    # minimum number of observations
    minObsRegion <- 5

    # create full data set
    dataFull <- df %>%
      factor.data.frame() %>%
      interpolate_missing_periods(period = seq(periodBegin, endOfHistory)) %>%
      as.magpie() %>%
      toolCountryFill(verbosity = 2) %>%
      as.quitte() %>%
      droplevels() %>%
      left_join(gdppop, by = c("region", "period")) %>%
      left_join(dens, by = c("region", "period"))

    # global regression, used as fallback for regions without enough data
    estimateGlobal <- lm(formula, data = filter(dataFull, !is.na(.data[["value"]])))

    # predict a region's floorspace: use its own regression if it has enough
    # observations, otherwise fall back to the global regression
    predictRegion <- function(chunk, ...) {
      obs <- filter(chunk, !is.na(.data[["value"]]))
      model <- if (nrow(obs) >= minObsRegion) {
        tryCatch(lm(formula, data = obs), error = function(e) estimateGlobal)
      } else {
        estimateGlobal
      }
      pred <- tryCatch(exp(predict(model, newdata = chunk)),
                       error = function(e) exp(predict(estimateGlobal, newdata = chunk)))
      mutate(chunk, pred = pred)
    }

    # predict missing data
    dataFull %>%

      # make prediction with region-specific or global regression
      group_by(across(all_of("region"))) %>%
      group_modify(predictRegion) %>%

      # create correction factor to balance-out deviations w.r.t. historic data
      mutate(factor = .data[["value"]] / .data[["pred"]]) %>%

      # regress deviation factor for all periods
      group_modify(~ extrapolateMissingPeriods(.x, key = "factor", slopeOfLast = 20)) %>%
      ungroup() %>%

      # correct prediction deviations if factor is available
      mutate(pred = .data[["pred"]] * replace_na(.data[["factor"]], 1)) %>%

      # fill missing values w/ predictions
      mutate(value = ifelse(is.na(.data[["value"]]), .data[["pred"]], .data[["value"]])) %>%

      # select columns
      select("region", "period", "variable", "value")
  }


  # PARAMETERS -----------------------------------------------------------------

  # lower temporal boundary for historical data
  periodBegin <- 1990


  varMapOdyssee <- list("surter" = "commercial",       # total commercial floor area in m2
                        "nbrlog" = "nDwellings",       # number of residential dwellings
                        "surlog" = "residentialAvg")   # average floor space of single dwelling in m2

  varMapIEAEEI <- list("ACT_R_AREA" = "residential",   # residential floor space in Gm2
                       "ACT_S_AREA" = "commercial")    # commercial floor space in Gm2

  varMapGEM <- list("COM" = "commercial",
                    "RES" = "residential")


  # source hierarchy
  sourcePriority <- c("Odyssee", "IEA EEI", "GLOBUS", "Daioglou", "GEM", "EEA")

  # exceptions to the source hierarchy overwriting the default priority selection
  sourceExceptions <- list("IEA EEI" = c("FIN", "GRC"),
                           "GLOBUS"  = c("BGR", "LUX", "MLT"))


  # LOAD DATA ------------------------------------------------------------------

  # Odyssee
  odyssee <- readSource("Odyssee") %>%
    as.quitte(na.rm = TRUE)

  # IEA EEI
  eei <- readSource("IEA_EEI", convert = TRUE) %>%
    as.quitte()

  # GLOBUS
  globus <- readSource("GLOBUS") %>%
    as.quitte(na.rm = TRUE)

  # Daioglou
  daioglou <- readSource("Daioglou") %>%
    as.quitte(na.rm = TRUE)

  # EEA
  eea <- readSource("EEAfloorspace") %>%
    as.quitte(na.rm = TRUE)

  # Global Earthquake Model
  gem <- readSource("GEM") %>%
    as.quitte(na.rm = TRUE)

  # Population
  pop <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE) %>%
    as.quitte()

  # GDP per capita
  gdppop <- calcOutput("GDPpc",
                       scenario = "SSP2",
                       average2020 = FALSE,
                       aggregate = FALSE,
                       years = 1960:endOfHistory) %>%
    as.quitte()

  # Population density
  dens <- calcOutput("Density", aggregate = FALSE) %>%
    as.quitte()



  # PROCESS DATA ---------------------------------------------------------------

  ## socio-economic variables ====
  pop <- pop %>%
    mutate(value = .data$value * 1e6) %>%
    select("region", "period", "pop" = "value")

  gdppop <- gdppop %>%
    select("region", "period", "gdppop" = "value")

  dens <- dens %>%
    select("region", "period", "density" = "value")


  ## Odyssee ====
  odyssee <- odyssee %>%
    filter(.data$variable %in% names(varMapOdyssee)) %>%
    mutate(variable = recode(.data$variable, !!!varMapOdyssee)) %>%
    select("region", "period", "variable", "value") %>%

    # calculate total residential floor space
    pivot_wider(names_from = "variable", values_from = "value") %>%
    mutate(residential = .data$nDwellings * .data$residentialAvg) %>%
    select(-"nDwellings", -"residentialAvg") %>%
    pivot_longer(cols = c("residential", "commercial"),
                 names_to = "variable",
                 values_to = "value") %>%
    left_join(pop, by = c("region", "period")) %>%
    mutate(value = .data$value / .data$pop,
           source = "Odyssee") %>%
    select("region", "period", "variable", "source", "value")


  ## IEA EEI ====
  eei <- eei %>%
    filter(.data$ITEM %in% names(varMapIEAEEI)) %>%
    mutate(variable = recode(.data$ITEM, !!!varMapIEAEEI),
           value = .data$value * 1e9) %>%
    filter(.data$value != 0) %>%
    left_join(pop, by = c("region", "period")) %>%
    mutate(value = .data$value / .data$pop,
           source = "IEA EEI") %>%
    select("region", "period", "variable", "source", "value")


  ## GLOBUS ====
  globus <- globus %>%
    filter(grepl("residential|commercial", .data$variable)) %>%
    mutate(variable = sub(" floor space", "", .data$variable),
           value = .data$value * 1e6) %>%
    left_join(pop, by = c("region", "period")) %>%
    mutate(value = .data$value / .data$pop,
           source = "GLOBUS") %>%
    select("region", "period", "variable", "source", "value")


  ## Daioglou ====
  daioglou <- daioglou %>%
    filter(.data[["quintile"]] == 0,
           .data$demographic == "Total") %>%
    mutate(variable = "residential",
           source = "Daioglou") %>%
    select("region", "period", "variable", "source", "value")


  ## EEA ====
  eea <- eea %>%
    mutate(variable = "residential",
           value = .data$value * 1e6) %>%
    left_join(pop, by = c("region", "period")) %>%
    mutate(value = .data$value / .data$pop,
           source = "EEA") %>%
    select("region", "period", "variable", "source", "value")


  ## GEM ====
  gem <- gem %>%
    filter(.data$variable == "TOTAL_AREA_SQM",
           .data$sector %in% names(varMapGEM)) %>%
    left_join(pop, by = c("region", "period")) %>%
    mutate(variable = recode(.data$sector, !!!varMapGEM),
           value = .data$value / .data$pop,
           source = "GEM") %>%
    select("region", "period", "variable", "source", "value")



  ## Data Mixing ====

  # Going down 'sourcePriority', the highest-priority source that provides any
  # data point for a region contributes all of its data points, unless a
  # variable/region combo is reassigned in 'sourceExceptions'.
  dataSources <- odyssee %>%
    rbind(eei, globus, daioglou, eea, gem) %>%
    filter(!is.na(.data$value),
           .data$period <= endOfHistory)


  # flatten exceptions to (region, source) pairs
  sourceOverride <- bind_rows(lapply(names(sourceExceptions), function(src) {
    data.frame(region = sourceExceptions[[src]], source = src)
  }))


  data <- dataSources %>%
    # keep only the rows of the highest-priority source available per region
    mutate(source = factor(.data$source, levels = sourcePriority, ordered = TRUE)) %>%
    group_by(across(all_of("region"))) %>%
    filter(.data$source == min(.data$source)) %>%
    ungroup() %>%
    mutate(source = as.character(.data$source)) %>%

    # replace overridden regions with their designated source (both variables)
    anti_join(sourceOverride, by = "region") %>%
    bind_rows(inner_join(dataSources, sourceOverride,
                         by = c("region", "source"))) %>%
    select("region", "period", "variable", "value")


  ## Projections ====

  # residential
  residential <- data %>%
    filter(.data$variable == "residential") %>%
    projectFloorspace(gdppop, dens, "log(value) ~ 1 + log(gdppop) + log(density)",
                      periodBegin, endOfHistory)

  # commercial
  commercial <- data %>%
    filter(.data$variable == "commercial") %>%
    projectFloorspace(gdppop, dens, "log(value) ~ 1 + log(gdppop)",
                      periodBegin, endOfHistory)

  data <- rbind(residential, commercial) %>%
    mutate(scenario = "history")



  # OUTPUT ---------------------------------------------------------------------

  data <- data %>%
    as.quitte() %>%
    as.magpie() %>%
    collapseDim()

  pop <- pop %>%
    rename("value" = "pop") %>%
    as.quitte() %>%
    as.magpie() %>%
    mselect(region = getItems(data, 1), period = getItems(data, 2)) %>%
    collapseDim()


  return(list(x = data,
              weight = pop,
              min = 0,
              unit = "m2/cap",
              description = "floor space per capita"))
}
