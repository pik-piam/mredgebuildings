#' Historical floor space demand
#'
#' Historical residential and commercial floor space per capita is compiled from
#' several data sources. For each region, only a single source is used: Going
#' down a fixed priority (Odyssee > IEA EEI > GLOBUS > Daioglou > GEM > EEA), the
#' highest-priority source that provides any data point for a region contributes
#' all of its data points (all variables and periods). Individual regions can be
#' reassigned to a specific source via a fixed list of exceptions. Region/source
#' combinations listed as outliers are discarded beforehand. Missing
#' regions and periods are subsequently filled by a sectoral regression on GDP
#' per capita (and, for residential, population density) that is performed
#' region-wise if enough data points exist, otherwise a global regression is
#' used. Both regressions are fitted on the data of all sources, weighted by
#' source priority; the global regression additionally weights each region by
#' its population. Predictions are subsequently scaled to match the selected
#' data region-wise.
#'
#' @param endOfHistory upper temporal boundary for historical data
#' @param returnPars if \code{TRUE}, only the parameters of the global
#'   regressions are returned as a MAgPIE object (region \code{"GLO"}) instead of
#'   the historic floor space
#'
#' @returns MAgPIE object with historic floor space per capita, or, if
#'   \code{returnPars = TRUE}, with the global regression parameters
#'
#' @author Robin Hasse, Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom madrat readSource calcOutput toolCountryFill toolGetMapping toolAggregate
#' @importFrom quitte as.quitte factor.data.frame interpolate_missing_periods
#'   calc_addVariable
#' @importFrom dplyr filter mutate select group_by left_join %>% .data ungroup
#'   group_modify recode across all_of bind_rows rename anti_join inner_join
#' @importFrom magclass mbind as.magpie collapseDim mselect getItems getYears
#' @importFrom tidyr replace_na pivot_wider pivot_longer unnest
#' @importFrom stats coef lm predict as.formula
#' @importFrom tibble enframe
#' @export

calcFloorspacePast <- function(endOfHistory = 2025, returnPars = FALSE) {

  # FUNCTIONS ------------------------------------------------------------------

  # Interpolate data within [periodBegin, endOfHistory] and add the predictors.
  # If 'fill' is TRUE, the data is expanded to all ISO countries.
  .prepareData <- function(df, gdppop, dens, periodBegin, endOfHistory, fill = TRUE) {
    df <- df %>%
      factor.data.frame() %>%
      interpolate_missing_periods(period = seq(periodBegin, endOfHistory))
    if (isTRUE(fill)) {
      df <- df %>%
        as.magpie() %>%
        toolCountryFill(verbosity = 2) %>%
        as.quitte()
    }
    df %>%
      droplevels() %>%
      left_join(gdppop, by = c("region", "period")) %>%
      left_join(dens, by = c("region", "period"))
  }

  # Add a regression weight to each data point: the population of every region
  # is split among its data points proportionally to their source weights.
  .addWeights <- function(df, pop, sourceWeight) {
    df %>%
      mutate(sourceWeight = unname(sourceWeight[as.character(.data$source)])) %>%
      left_join(pop, by = c("region", "period")) %>%
      group_by(across(all_of(c("region", "period", "variable")))) %>%
      mutate(weight = .data$pop * proportions(.data$sourceWeight)) %>%
      ungroup() %>%
      select(-"sourceWeight", -"pop")
  }

  # A global weighted linear regression given by 'formula' is fitted across all
  # regions of 'fitData'. Regions do not need to be ISO countries.
  .fitGlobal <- function(fitData, formula) {
    lm(as.formula(formula), data = fitData, weights = fitData$weight)
  }

  # A region-specific weighted linear regression given by 'formula' is fitted on
  # 'fitData' for every region that provides enough observations. Regions with
  # too few (or no) observations fall back to the global regression 'estimateGlobal'.
  # Predicted values are corrected region-wise w.r.t. historical data 'df' where
  # available, otherwise the prediction is chosen.
  #
  # Returns the filled floor space per capita (region, period, variable, value).
  .projectFloorspace <- function(df, fitData, gdppop, dens, formula, periodBegin, endOfHistory,
                                 estimateGlobal) {

    ## helper functions ====

    # fit a region's regression, falling back to the global one
    .fitRegion <- function(chunk, formula, estimateGlobal, minObsRegion) {
      formulaVars <- all.vars(as.formula(formula))
      obs <- chunk %>%
        select(all_of(intersect(c(formulaVars, "weight"), names(chunk))))
      if (nrow(obs) >= minObsRegion) {
        tryCatch(lm(as.formula(formula), data = obs, weights = obs$weight),
                 error = function(e) estimateGlobal)
      } else {
        estimateGlobal
      }
    }

    # predict a region's floor space
    .predictRegion <- function(chunk, key, fitData, formula, estimateGlobal, minObsRegion) {
      model <- .fitRegion(chunk = fitData %>%
                            filter(.data$region == key$region),
                          formula = formula,
                          estimateGlobal = estimateGlobal,
                          minObsRegion = minObsRegion)
      pred <- tryCatch(exp(predict(model, newdata = chunk)),
                       error = function(e) exp(predict(estimateGlobal, newdata = chunk)))
      mutate(chunk, pred = pred)
    }


    # minimum number of observations for a region-specific regression
    minObsRegion <- 5

    # create full data set
    dataFull <- .prepareData(df, gdppop, dens, periodBegin, endOfHistory)

    # predict missing data with region-specific or global regression
    pred <- dataFull %>%
      group_by(across(all_of("region"))) %>%
      group_modify(.predictRegion,
                   fitData = fitData,
                   formula = formula,
                   estimateGlobal = estimateGlobal,
                   minObsRegion = minObsRegion) %>%
      ungroup()

    # create correction factor to balance-out deviations w.r.t. historic data and
    # linearly interpolate deviation factor in between data points, hold constant beyond
    factor <- pred %>%
      mutate(factor = .data[["value"]] / .data[["pred"]]) %>%
      select("region", "period", "factor") %>%
      interpolate_missing_periods(value = "factor", expand.values = TRUE)

    # correct prediction deviations if factor is available and fill missing values w/ predictions
    pred %>%
      left_join(factor, by = c("region", "period")) %>%
      mutate(pred = .data[["pred"]] * replace_na(.data[["factor"]], 1)) %>%
      mutate(value = ifelse(is.na(.data[["value"]]), .data[["pred"]], .data[["value"]])) %>%
      select("region", "period", "variable", "value")
  }

  # coefficients of a global regression (region = "GLO", variable, value)
  # where 'variable' holds the coefficient name
  .globalParameters <- function(estimateGlobal) {
    globalCoefs <- coef(estimateGlobal)
    data.frame(region = "GLO",
               variable = names(globalCoefs),
               value = unname(globalCoefs))
  }


  # PARAMETERS -----------------------------------------------------------------

  # lower temporal boundary for historical data
  periodBegin <- 1990


  varMapOdyssee <- list("surter" = "commercial",       # total commercial floor area in m2
                        "nbrlpr" = "nDwellings",       # stock of permanently occupied dwellings
                        "surlog" = "residentialAvg")   # average floor space of single dwelling in m2

  varMapIEAEEI <- list("ACT_R_AREA" = "residential",   # residential floor space in Gm2
                       "ACT_S_AREA" = "commercial")    # commercial floor space in Gm2

  varMapGEM <- list("COM" = "commercial",
                    "RES" = "residential")


  # source hierarchy
  sourcePriority <- c("Odyssee", "IEA EEI", "GLOBUS", "Daioglou", "GEM", "EEA")

  # source weights in the regressions decline geometrically with priority rank
  sourceWeightBase <- 0.5
  sourceWeight <- setNames(sourceWeightBase^(seq_along(sourcePriority) - 1),
                           sourcePriority)

  # exceptions to the source hierarchy overwriting the default priority selection
  sourceExceptions <- list("IEA EEI" = c("FIN", "GRC"),
                           "GLOBUS"  = c("BGR", "LUX", "MLT", "USA"),
                           "GEM"     = c("BEL", "EST", "ITA", "LVA", "ROU"))

  # region/source combinations excluded from the regressions as outliers
  outlierSources <- list("RUS" = c("GEM"))


  # LOAD DATA ------------------------------------------------------------------

  # Odyssee
  odyssee <- readSource("Odyssee") %>%
    as.quitte(na.rm = TRUE)

  # IEA EEI
  eei <- readSource("IEA_EEI", convert = TRUE) %>%
    as.quitte(na.rm = TRUE)

  # GLOBUS (total, country-level)
  globus <- readSource("GLOBUS") %>%
    as.quitte(na.rm = TRUE)

  # GLOBUS (per capita, region-level)
  globusAgg <- readSource("GLOBUS", convert = FALSE, subtype = "perCapita") %>%
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
  pop <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)

  # GDP per capita
  gdppop <- calcOutput("GDPpc",
                       scenario = "SSP2",
                       average2020 = FALSE,
                       aggregate = FALSE,
                       years = periodBegin:endOfHistory)

  # Population density
  dens <- calcOutput("Density", aggregate = FALSE)

  # GLOBUS region mapping
  regionmapGlobus <- toolGetMapping(name  = "regionmappingGLOBUS.csv",
                                    type  = "regional",
                                    where = "mredgebuildings")



  # PROCESS DATA ---------------------------------------------------------------

  ## socio-economic variables ====

  # predictors of the global regression additionally cover the multi-country
  # GLOBUS regions (single-country GLOBUS regions share their ISO code)
  regionmapGlobusMulti <- regionmapGlobus %>%
    filter(.data$RegionCode != .data$CountryCode)

  gdppopGlobus <- toolAggregate(gdppop[regionmapGlobusMulti$CountryCode, , ],
                                rel = regionmapGlobusMulti,
                                weight = pop[regionmapGlobusMulti$CountryCode, getYears(gdppop), ],
                                from = "CountryCode",
                                to = "RegionCode")

  densGlobus <- toolAggregate(dens[regionmapGlobusMulti$CountryCode, , ],
                              rel = regionmapGlobusMulti,
                              weight = pop[regionmapGlobusMulti$CountryCode, getYears(dens), ],
                              from = "CountryCode",
                              to = "RegionCode")

  popGlobus <- toolAggregate(pop[regionmapGlobusMulti$CountryCode, , ],
                             rel = regionmapGlobusMulti,
                             from = "CountryCode",
                             to = "RegionCode")

  gdppopFit <- mbind(gdppop, gdppopGlobus) %>%
    as.quitte() %>%
    select("region", "period", "gdppop" = "value")

  densFit <- mbind(dens, densGlobus) %>%
    as.quitte() %>%
    select("region", "period", "density" = "value")

  popFit <- mbind(pop, popGlobus) %>%
    as.quitte() %>%
    select("region", "period", "pop" = "value")

  pop <- pop %>%
    as.quitte() %>%
    mutate(value = .data$value * 1e6) %>%
    select("region", "period", "pop" = "value")

  gdppop <- gdppop %>%
    as.quitte() %>%
    select("region", "period", "gdppop" = "value")

  dens <- dens %>%
    as.quitte() %>%
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

  globusAgg <- globusAgg %>%
    filter(grepl("residential|commercial", .data$variable)) %>%
    mutate(variable = sub(" floor area per capita", "", .data$variable),
           variable = sub("non-residential", "commercial", .data$variable),
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
  # variable/region combo is reassigned in 'sourceExceptions'. Outliers listed in
  # 'outlierSources' are discarded entirely.

  # flatten outliers to (region, source) pairs
  outliers <- outlierSources %>%
    enframe(name = "region", value = "source") %>%
    unnest("source")

  dataSources <- odyssee %>%
    rbind(eei, globus, daioglou, eea, gem) %>%
    filter(!is.na(.data$value),
           .data$period <= endOfHistory) %>%
    anti_join(outliers, by = c("region", "source"))


  # flatten exceptions to (region, source) pairs
  sourceOverride <- bind_rows(lapply(names(sourceExceptions), function(src) {
    data.frame(region = sourceExceptions[[src]], source = src)
  }))


  selectedData <- dataSources %>%
    # keep only the rows of the highest-priority source available per region
    mutate(source = factor(.data$source, levels = sourcePriority, ordered = TRUE)) %>%
    group_by(across(all_of("region"))) %>%
    filter(.data$source == min(.data$source)) %>%
    ungroup() %>%
    mutate(source = as.character(.data$source)) %>%

    # replace overridden regions with their designated source (both variables)
    anti_join(sourceOverride, by = "region") %>%
    bind_rows(inner_join(dataSources, sourceOverride, by = c("region", "source"))) %>%
    select("region", "period", "variable", "value")

  # data for the regional regressions: all sources
  fitDataRegional <- dataSources %>%
    .prepareData(gdppop, dens, periodBegin, endOfHistory, fill = FALSE) %>%
    filter(!is.na(.data[["value"]])) %>%
    .addWeights(pop, sourceWeight)

  # data for the global regression: all sources with the disaggregated GLOBUS
  # data replaced by the original aggregate GLOBUS data
  fitDataGlobal <- dataSources %>%
    filter(.data$source != "GLOBUS") %>%
    bind_rows(globusAgg %>%
                filter(.data$period <= endOfHistory) %>%
                anti_join(outliers, by = c("region", "source"))) %>%
    .prepareData(gdppopFit, densFit, periodBegin, endOfHistory, fill = FALSE) %>%
    filter(!is.na(.data[["value"]]) & .data$value != 0) %>%
    .addWeights(popFit, sourceWeight)


  ## Projections ====

  formulas <- list(residential = "log(value) ~ 1 + log(gdppop) + log(density)",
                   commercial  = "log(value) ~ 1 + log(gdppop)")

  projections <- NULL
  fitCoefs    <- NULL

  for (sector in names(formulas)) {
    # global regression
    estimateGlobal <- fitDataGlobal %>%
      filter(.data$variable == sector) %>%
      .fitGlobal(formulas[[sector]])

    # region-specific projection with global fallback
    projections <- mbind(projections,
                         selectedData %>%
                           filter(.data$variable == sector) %>%
                           .projectFloorspace(fitDataRegional %>%
                                                filter(.data$variable == sector),
                                              gdppop,
                                              dens,
                                              formulas[[sector]],
                                              periodBegin,
                                              endOfHistory,
                                              estimateGlobal) %>%
                           mutate(scenario = "history") %>%
                           as.quitte() %>%
                           as.magpie())

    # coefficients of the global regression, labelled as <coefficient>_<sector>
    fitCoefs <- mbind(fitCoefs,
                      .globalParameters(estimateGlobal) %>%
                        mutate(variable = paste0(.data$variable, "_", sector)) %>%
                        as.magpie())
  }

  projections <- collapseDim(projections)



  # OUTPUT ---------------------------------------------------------------------

  pop <- pop %>%
    rename("value" = "pop") %>%
    as.quitte() %>%
    as.magpie() %>%
    mselect(region = getItems(projections, 1), period = getItems(projections, 2)) %>%
    collapseDim()


  # return only the global regression coefficients
  if (isTRUE(returnPars)) {
    return(list(x = fitCoefs,
                isocountries = FALSE,
                unit = "",
                description = "global regression parameters for historic floor space per capita"))
  } else {
    return(list(x = projections,
                weight = pop,
                min = 0,
                unit = "m2/cap",
                description = "floor space per capita"))
  }
}
