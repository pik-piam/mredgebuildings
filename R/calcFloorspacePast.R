#' Historical floor space demand
#'
#' Historical residential and commercial floor space per capita is compiled from
#' several data sources. For each region, only a single source is used: Going
#' down a fixed priority (Odyssee > IEA EEI > GLOBUS > Daioglou > GEM > EEA), the
#' highest-priority source that provides any data point for a region contributes
#' all of its data points (all variables and periods). Individual regions can be
#' reassigned to a specific source via a fixed list of exceptions. Missing
#' regions and periods are subsequently filled by a sectoral regression on GDP
#' per capita (and, for residential, population density) that is performed
#' region-wise if enough data points exist, otherwise a global regression is
#' used. Predictions are subsequently scaled to match existing data region-wise.
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
#'   summarise pull
#' @importFrom magclass mbind as.magpie collapseDim mselect getItems getYears
#' @importFrom tidyr replace_na pivot_wider pivot_longer
#' @importFrom stats coef lm predict as.formula
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

  # A global linear regression given by 'formula' is fitted across all regions
  # of 'fitData'. Regions do not need to be ISO countries.
  .fitGlobal <- function(fitData, gdppop, dens, formula, periodBegin, endOfHistory) {
    fitData %>%
      .prepareData(gdppop, dens, periodBegin, endOfHistory, fill = FALSE) %>%
      filter(!is.na(.data[["value"]]) & .data$value != 0) %>%
      lm(formula = as.formula(formula))
  }

  # A region-specific linear regression given by 'formula' is fitted for every
  # region that provides enough observations. Regions with too few (or no)
  # observations fall back to the global regression 'estimateGlobal'.
  # Predicted values are corrected region-wise w.r.t. historical data where
  # available, otherwise the prediction is chosen.
  #
  # Returns the filled floor space per capita (region, period, variable, value).
  .projectFloorspace <- function(df, gdppop, dens, formula, periodBegin, endOfHistory,
                                 estimateGlobal) {

    ## helper functions ====

    # fit a region's regression, falling back to the global one
    .fitRegion <- function(chunk) {
      obs <- chunk %>%
        filter(!is.na(.data[["value"]])) %>%
        select(all_of(intersect(formulaVars, names(chunk))))
      if (nrow(obs) >= minObsRegion) {
        tryCatch(lm(formula, data = obs), error = function(e) estimateGlobal)
      } else {
        estimateGlobal
      }
    }

    # predict a region's floor space
    .predictRegion <- function(chunk, key, ...) {
      model <- .fitRegion(chunk)
      pred <- tryCatch(exp(predict(model, newdata = chunk)),
                       error = function(e) exp(predict(estimateGlobal, newdata = chunk)))
      mutate(chunk, pred = pred)
    }


    # minimum number of observations for a region-specific regression
    minObsRegion <- 5

    # variables entering the regression
    formulaVars <- all.vars(as.formula(formula))

    # create full data set
    dataFull <- .prepareData(df, gdppop, dens, periodBegin, endOfHistory)

    # predict missing data with region-specific or global regression
    pred <- dataFull %>%
      group_by(across(all_of("region"))) %>%
      group_modify(.predictRegion) %>%
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

  # exceptions to the source hierarchy overwriting the default priority selection
  sourceExceptions <- list("IEA EEI" = c("FIN", "GRC"),
                           "GLOBUS"  = c("BGR", "LUX", "MLT", "USA"),
                           "GEM"     = c("BEL", "EST", "ITA", "LVA", "ROU"))

  # minimum share of countries of a GLOBUS region with GLOBUS as selected source
  # for the region to enter the global regression with its original aggregate data
  minShareGlobus <- 0.5


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
                       years = 1960:endOfHistory)

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

  gdppopFit <- mbind(gdppop, gdppopGlobus) %>%
    as.quitte() %>%
    select("region", "period", "gdppop" = "value")

  densFit <- mbind(dens, densGlobus) %>%
    as.quitte() %>%
    select("region", "period", "density" = "value")

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
  # variable/region combo is reassigned in 'sourceExceptions'.
  dataSources <- odyssee %>%
    rbind(eei, globus, daioglou, eea, gem) %>%
    filter(!is.na(.data$value),
           .data$period <= endOfHistory)


  # flatten exceptions to (region, source) pairs
  sourceOverride <- bind_rows(lapply(names(sourceExceptions), function(src) {
    data.frame(region = sourceExceptions[[src]], source = src)
  }))


  selected <- dataSources %>%
    # keep only the rows of the highest-priority source available per region
    mutate(source = factor(.data$source, levels = sourcePriority, ordered = TRUE)) %>%
    group_by(across(all_of("region"))) %>%
    filter(.data$source == min(.data$source)) %>%
    ungroup() %>%
    mutate(source = as.character(.data$source)) %>%

    # replace overridden regions with their designated source (both variables)
    anti_join(sourceOverride, by = "region") %>%
    bind_rows(inner_join(dataSources, sourceOverride,
                         by = c("region", "source")))

  data <- selected %>%
    select("region", "period", "variable", "value")

  # GLOBUS regions where GLOBUS is the selected source for enough countries
  globusRegions <- regionmapGlobus %>%
    left_join(selected %>%
                select("region", "source") %>%
                unique(),
              by = c("CountryCode" = "region")) %>%
    group_by(across(all_of("RegionCode"))) %>%
    summarise(share = mean(.data$source %in% "GLOBUS"), .groups = "drop") %>%
    filter(.data$share >= minShareGlobus) %>%
    pull("RegionCode")

  globusCountries <- regionmapGlobus %>%
    filter(.data$RegionCode %in% globusRegions) %>%
    pull("CountryCode")

  # data for the global regression: the disaggregated GLOBUS data of these
  # regions is replaced by the original aggregate GLOBUS data
  fitData <- selected %>%
    filter(!(.data$source == "GLOBUS" & .data$region %in% globusCountries)) %>%
    select("region", "period", "variable", "value") %>%
    bind_rows(globusAgg %>%
                filter(.data$region %in% globusRegions,
                       .data$period <= endOfHistory) %>%
                select("region", "period", "variable", "value"))


  ## Projections ====

  formulas <- list(residential = "log(value) ~ 1 + log(gdppop) + log(density)",
                   commercial  = "log(value) ~ 1 + log(gdppop)")

  projections <- NULL
  fitCoefs    <- NULL

  for (sector in names(formulas)) {
    # global regression
    estimatesGlobal <- fitData %>%
      filter(.data$variable == sector) %>%
      .fitGlobal(gdppopFit, densFit, formulas[[sector]], periodBegin, endOfHistory)

    # region-specific projection with global fallback
    projections <- mbind(projections,
                         data %>%
                           filter(.data$variable == sector) %>%
                           .projectFloorspace(gdppop, dens, formulas[[sector]], periodBegin,
                                              endOfHistory, estimatesGlobal) %>%
                           mutate(scenario = "history") %>%
                           as.quitte() %>%
                           as.magpie())

    # coefficients of the global regression, labelled as <coefficient>_<sector>
    fitCoefs <- mbind(fitCoefs,
                      .globalParameters(estimatesGlobal) %>%
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
