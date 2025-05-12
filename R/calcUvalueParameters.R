#' Calculate fit parameters for U-value projections
#'
#' Estimates regional fit parameters for projecting building envelope U-values using mixed-effects
#' regression. Combines data from Hotmaps, MessageIX Buildings, and EU Buildings Observatory to model
#' thermal performance across regions based on climate indicators (HDD/CDD) and economic development
#' (GDP per capita).
#'
#' @param endOfHistory Integer. The last year to consider as historical data (default: 2025)
#'
#' @return magpie object with regional fit parameters
#'
#' @details
#' The function uses a linear mixed-effects model with the formula:
#' log(U-value - minU) ~ HDD * log(gdppop + 1) + CDD * log(gdppop + 1) + (1 | region)
#'
#' This captures the relationship between U-values and:
#' - Heating degree days (HDD)
#' - Cooling degree days (CDD)
#' - GDP per capita (gdppop)
#' - Interaction terms: how climate effects vary with economic development
#' - Regional random effects: region-specific deviations from global pattern
#'
#' The function handles regional parameter estimation in two ways:
#' 1. For regions with sufficient historical data points, parameters are directly estimated
#'    via mixed-effects regression (lmer)
#' 2. For regions with insufficient data, the function:
#'    - Uses global fixed effects as a baseline
#'    - Calculates the difference between aggregated regional projections and their reference values
#'    - Applies empirical Bayes shrinkage to balance between global pattern and regional differences
#'    - Adjusts intercepts using these shrunken offsets to create region-specific parameters
#'
#' The resulting parameters can be used to project future U-values based on climate and
#' economic scenarios.
#'
#' @importFrom stringr str_to_lower word
#' @importFrom dplyr filter mutate select rename group_by ungroup reframe recode semi_join left_join right_join
#' @importFrom tibble deframe
#' @importFrom lme4 lmer fixef
#' @importFrom stats coef sigma var
#' @importFrom quitte aggregate_map
#'
#' @author Hagen Tockhorn
#'
#' @export

calcUValueParameters <- function(endOfHistory = 2025) {

  # READ-IN DATA ---------------------------------------------------------------

  ## U-value data ====

  hotmapsData <- readSource("Hotmaps") %>%
    as.quitte(na.rm = TRUE)

  messageData <- readSource("MessageIXBuildings", subtype = "uvalue") %>%
    as.quitte(na.rm = TRUE)

  euBuildObsData <- readSource("EUBuildingsDB", "BuildingShellPerformance") %>%
    as.quitte(na.rm = TRUE)


  # Floor space data ====

  # MESSAGE
  messageFloorspace <- readSource("MessageIXBuildings", subtype = "floorByCohort") %>%
    as.quitte(na.rm = TRUE)

  # Odyssee
  odysseeData <- rbind(readSource("Odyssee", subtype = "households") %>%
                         as.quitte(na.rm = TRUE),
                       readSource("Odyssee", subtype = "services") %>%
                         as.quitte(na.rm = TRUE))

  # historical floor space as regional aggregation weights
  floorspaceWeights <- calcOutput("FloorspacePast", aggregate = FALSE) %>%
    as_tibble()


  #TODO: improve this with new HDDCDD function   #nolint: todo_comment_linter
  # Heating / Cooling Degree-Days
  hddcdd <- calcOutput("HDDCDD", fromSource = TRUE, aggregate = FALSE) %>%
    as_tibble()


  # Population
  pop <- calcOutput("Population", aggregate = FALSE) %>%
    as.quitte()


  # GDP per capita
  gdppop <- calcOutput("GDPpc",
                       scenario = "SSP2",
                       average2020 = FALSE,
                       unit = "constant 2005 Int$PPP",
                       aggregate = FALSE,
                       years = 1960:endOfHistory) %>%
    as.quitte()


  # General distribution of component area for different building types (taken from BEAM^2 model)
  buildingCharacteristics <- readSource("BEAM2") %>%
    as_tibble()



  # PARAMETERS -----------------------------------------------------------------

  # Historical limit temperatures
  tLim <- c("HDD" = 14, "CDD" = 20)


  # factor for minimum u-value assumption (minU = minFactor * min(historical u-values))
  minFactor <- 0.75


  ## Relevant parameters ====

  uvalueParameters <- c("BUILDING|Construction features (U-values)|Floor",
                        "BUILDING|Construction features (U-values)|Walls",
                        "BUILDING|Construction features (U-values)|Roof",
                        "BUILDING|Construction features (U-values)|Windows")

  buildingsToIgnore <- c("Residential sector|Total|Total",
                         "Residential sector|Residential sector|Total",
                         "Service sector|Total|Total")

  floorParameters <- c("BUILDING|Area|Constructed area")


  ## Mappings ====

  # Hotmaps buildings types
  buildingTypeMap <- toolGetMapping("buildingTypeMapping_Hotmaps-BEAM2.csv",
                                    type = "sectoral",
                                    where = "mredgebuildings")

  # Vintage types of MessageIX
  vintageMapMessage <- toolGetMapping("vintageMapping_MessageIX.csv",
                                      type = "sectoral",
                                      where = "mredgebuildings")

  # Variable mappings

  varMapEUBuildObs <- toolGetMapping("variableMapping_EUBuildingsObservatory.csv",
                                     type = "sectoral",
                                     where = "mredgebuildings")

  varMapBuildCharacteristics <- list("Cellar ceiling"       = "floor",
                                     "Roof / upper ceiling" = "roof",
                                     "Exterior Walls"       = "walls",
                                     "Windows"              = "windows")

  varMapOdyssee <- list("surter_m2" = "floorCom",        # total floor area of services in m2
                        "nbrlog_1"  = "nDwellings",      # number of residential dwellings
                        "surlog_m2" = "residentialAvg")  # average floor space of single dwelling in m2

  varMapFitPars <- c("(Intercept)"         = "parINTERCEPT",
                     "HDD"                 = "parHDD",
                     "CDD"                 = "parCDD",
                     "log(gdppop + 1)"     = "parGDPPOP",
                     "HDD:log(gdppop + 1)" = "parHDDGDPPOP",
                     "log(gdppop + 1):CDD" = "parCDDGDPPOP")

  # Region mappings

  regionmapMessage <- toolGetMapping("regionmappingMessageIX.csv",
                                     type = "regional",
                                     where = "mredgebuildings")



  # PROCESS DATA ---------------------------------------------------------------


  ## Process U-Value Data ====


  ### Hotmaps Data =====

  # U-Values per building vintage and building component (e.g. floor, windows, ...)
  uvaluesHotmapsData <- hotmapsData %>%
    filter(.data$variable %in% uvalueParameters,
           !.data$building %in% buildingsToIgnore) %>%
    mutate(variable = str_to_lower(word(.data$variable, -1, sep = "\\|"))) %>%
    select(-"unit", -"model", -"scenario")

  # Prepare building components
  buildingCharacteristics <- buildingCharacteristics %>%
    mutate(variable = recode(.data$variable, !!!varMapBuildCharacteristics)) %>%
    rename("weight" = "value") %>%
    select(-"unit", -"source", -"floorspace", -"component_area", -"region", -"period")

  # Aggregate U-Values per building bage with weighted mean across component areas
  uvaluesAggComp <- uvaluesHotmapsData %>%
    left_join(buildingTypeMap, by = c("building" = "typHotmaps")) %>%
    left_join(buildingCharacteristics, by = c("variable", "typBEAM" = "building")) %>%
    group_by(across(all_of(c("region", "period", "bage", "building")))) %>%
    reframe(value = sum(.data$weight * .data$value)) %>%
    ungroup()

  # Floor space as aggregation weights
  floorspaceHotmaps <- hotmapsData %>%
    filter(.data$variable %in% floorParameters,
           !.data$building %in% buildingsToIgnore) %>%
    rename("floorspace" = "value") %>%
    select(-"scenario", -"model", -"unit", -"variable")

  # Aggregate u-values across building bages and types
  uvaluesHotmaps <- uvaluesAggComp %>%
    filter(!is.na(.data$value)) %>%
    left_join(floorspaceHotmaps, by = c("region", "period", "bage", "building")) %>%
    group_by(across(all_of(c("region", "period")))) %>%
    reframe(uvalue = sum(.data$value * .data$floorspace) / sum(.data$floorspace))



  ### EU Building Observatory ====

  # Floor space weights
  floorspaceOdyssee <- odysseeData %>%
    # rename variables
    filter(.data$variable %in% names(varMapOdyssee)) %>%
    mutate(variable = recode(.data$variable, !!!varMapOdyssee)) %>%
    select("region", "period", "variable", "value") %>%

    # calculate total residential floor space
    pivot_wider(names_from = "variable", values_from = "value") %>%
    mutate(floorRes = .data$nDwellings * .data$residentialAvg) %>%
    select(-"nDwellings", -"residentialAvg") %>%

    # calculate regional residential share
    group_by(across(all_of(c("region", "period")))) %>%
    mutate(floorShareRes = .data$floorRes / (.data$floorRes + .data$floorCom)) %>%
    ungroup() %>%

    # calculate global average shares per period
    group_by(across(all_of("period"))) %>%
    mutate(floorShareResGlo = mean(.data$floorShareRes, na.rm = TRUE)) %>%
    ungroup() %>%

    # allocate regional shares; use global average where NA
    mutate(floorShareRes = ifelse(is.na(.data$floorShareRes),
                                  .data$floorShareResGlo,
                                  .data$floorShareRes)) %>%
    select("region", "period", "floorShareRes")


  # Determine scaling factor component_area / floor space in (m2/m2) for residential + commercial
  scalingFactor <- buildingCharacteristics %>%
    mutate(building = recode(.data$building,
                             !!!deframe(buildingTypeMap %>%
                                          select("typBEAM", "typHotmaps")))) %>%
    # join floor space
    right_join(floorspaceHotmaps,
               by = "building",
               relationship = "many-to-many") %>%
    filter(!is.na(.data$weight)) %>%

    # make variable declaration compliant with other data
    mutate(building = sub(" sector\\|.*", "", .data$building),
           building = ifelse(.data$building == "Residential", "residential", "commercial")) %>%

    # aggregate factors per building type and vintage
    group_by(across(all_of(c("region", "building", "variable")))) %>%
    reframe(weight = sum(.data$weight * .data$floorspace) / sum(.data$floorspace))


  # Calculate U-values per region and period
  uvaluesEUBuildObs <- euBuildObsData %>%
    filter(.data$variable %in% varMapEUBuildObs$variable,
           .data$period != 2008) %>%

    left_join(varMapEUBuildObs, by = "variable") %>%
    select("region", "period", "variable" = "variableNew", "building", "value") %>%

    left_join(scalingFactor, by = c("region", "variable", "building")) %>%

    group_by(across(all_of(c("region", "period", "building")))) %>%
    reframe(value = sum(.data$value * .data$weight)) %>%

    # make variable names compliant
    pivot_wider(names_from = "building", values_from = "value") %>%

    # aggregate u-values
    left_join(floorspaceOdyssee, by = c("region", "period")) %>%

    mutate(uvalue = .data$residential * .data$floorShareRes +
             .data$commercial * (1 - .data$floorShareRes)) %>%

    select("region", "period", "uvalue")



  ### MessageIX-Buildings / STURM ====

  # Pre-process floor space weights
  messageFloorspace <- messageFloorspace %>%
    filter(.data$scenario == "SSP2",
           .data$period <= endOfHistory) %>%
    select("region", "period", "floorspace" = "value", "variable" = "cohort")


  # Aggregate u-values across building vintages per region
  uvaluesMessage <- messageData %>%
    select("region", "variable", "value") %>%

    # aggregate to vintage types since u-values are not distinguished between single-/multi-family homes
    mutate(variable = sub("^[^_]+_", "", .data$variable)) %>%
    group_by(across(all_of(c("region", "variable")))) %>%
    reframe(uvalue = mean(.data$value)) %>%

    # match cohort types
    right_join(vintageMapMessage,
               by = c("variable" = "cohortCode"),
               relationship = "many-to-many") %>%

    # aggregate u-values by floor space
    right_join(messageFloorspace, by = c("region", "cohort" = "variable")) %>%
    group_by(across(all_of(c("region", "period")))) %>%
    reframe(uvalue = sum(.data$uvalue * .data$floorspace) / sum(.data$floorspace),
            floorspace = sum(.data$floorspace))



  ## Prepare data for regression ====

  # Aggregate u-values to initial MessageIX regions for regression

  regionmapMessage <- regionmapMessage %>%
    select("region" = "CountryCode", "regionTarget" = "X11regions") %>%
    semi_join(uvaluesMessage, by = "region")


  ### U-values ====

  uvaluesMessageAgg <- uvaluesMessage %>%
    mutate(unit = NA) %>%
    pivot_longer(cols = c("uvalue", "floorspace"), names_to = "variable", values_to = "value") %>%
    aggregate_map(mapping = regionmapMessage,
                  by = "region",
                  subset2agg = "uvalue",
                  weights = "floorspace",
                  forceAggregation = TRUE) %>%
    select(-"unit") %>%
    filter(.data$variable == "uvalue") %>%
    pivot_wider(names_from = "variable", values_from = "value")


  ### Degree-Days ====

  # Filter correct historical limit temperatures
  hddcdd <- hddcdd %>%
    filter((.data$variable == "CDD" & .data$tlim == tLim[["CDD"]]) |
             (.data$variable == "HDD" & .data$tlim == tLim[["HDD"]]),
           .data$rcp == "historical",
           !is.na(.data$value)) %>%
    filter(.data$period <= endOfHistory) %>%
    select("region", "period", "variable", "value") %>%

    # we have some 0 entries due to insufficient resolution for very small islands
    filter(.data$value != 0)


  # Aggregate HDD/CDDS to MessageIX regions
  #TODO: replace this by calcOutput("HDDCDD", aggregate = TRUE, ...)   #nolint: todo_comment_linter
  pop <- pop %>%
    filter(.data$variable == "pop_SSP2") %>%
    mutate(variable = sub("_SSP2", "", .data$variable)) %>%
    select("region", "period", "variable", "value")

  hddcddMessage <- hddcdd %>%
    rbind(pop) %>%
    mutate(unit = NA) %>%
    aggregate_map(mapping = regionmapMessage %>%
                    semi_join(hddcdd, by = "region"),
                  by = "region",
                  subset2agg = c("HDD", "CDD"),
                  weights = "pop",
                  forceAggregation = TRUE) %>%
    select(-"unit")

  # Full historical HDD/CDD data for all relevant regions
  hddcddEstimate <- hddcdd %>%
    filter(!.data$region %in% unique(regionmapMessage$regionTarget)) %>%
    rbind(hddcddMessage) %>%
    pivot_wider(names_from = "variable", values_from = "value")


  ### GDP per capita ====

  # Aggregate GDP/pop to MessageIX regions
  gdppop <- gdppop %>%
    mutate(variable = "gdppop") %>%
    filter(.data$period <= endOfHistory) %>%
    select("region", "period", "variable", "value")

  gdppopMessage <- gdppop %>%
    rbind(pop) %>%
    mutate(unit = NA) %>%
    aggregate_map(mapping = regionmapMessage %>%
                    semi_join(gdppop, by = "region"),
                  by = "region",
                  subset2agg = "gdppop",
                  weights = "pop",
                  forceAggregation = TRUE) %>%
    select(-"unit")

  # Full historical GDP/pop data for all relevant regions
  gdppopEstimate <- gdppop %>%
    filter(!.data$region %in% unique(regionmapMessage$regionTarget)) %>%
    rbind(gdppopMessage) %>%
    pivot_wider(names_from = "variable", values_from = "value")


  ### Full data sets ====

  # Gather full historical data for estimates on u-values, HDD/CDDs and GDP/pop
  dataEstimate <- uvaluesHotmaps %>%
    rbind(uvaluesEUBuildObs,
          uvaluesMessageAgg) %>%
    right_join(hddcddEstimate, by = c("region", "period"),
               relationship = "many-to-many") %>%
    left_join(gdppopEstimate, by = c("region", "period")) %>%
    filter(!is.na(.data$uvalue))


  # Gather full historical data for predictions
  dataHist <- gdppop %>%
    semi_join(hddcdd, by = c("region", "period")) %>%
    rbind(hddcdd) %>%
    pivot_wider(names_from = "variable", values_from = "value")



  ## Make linear regression to fill historical data ====

  # Assumed minimum u-value
  minU <- minFactor * min(dataEstimate$uvalue)

  # Prepare data
  dataEstimate <- dataEstimate %>%
    mutate(uTrans = log(.data$uvalue - minU))

  # Make fit model
  model <- lmer("uTrans ~ HDD * log(gdppop + 1) + CDD * log(gdppop + 1) + (1 | region)",
                data = dataEstimate)

  # Make Projections
  uvalueProjection <- dataHist %>%
    mutate(uTransProj = predict(model,
                                newdata = dataHist,
                                re.form = NULL,
                                allow.new.levels = TRUE),
           value = minU + exp(.data$uTransProj),
           variable = "uvalue") %>%
    select("region", "period", "variable", "value")



  ## Extract fit parameters and calculate offset for previously aggregated regions ====

  # countries where we already have data points
  singleRegions <- dataEstimate %>%
    filter(!.data$region %in% regionmapMessage$regionTarget) %>%
    pull("region") %>%
    unique() %>%
    as.character()

  # global fit parameters
  globalPars <- fixef(model)

  # regional fit parameters with already adjusted intercept
  regionalPars <- coef(model)$region %>%
    as.data.frame() %>%
    (\(x) mutate(x, region = row.names(x)))() %>%
    filter(.data$region %in% singleRegions) %>%
    rename(!!!setNames(names(varMapFitPars), unlist(varMapFitPars)))

  row.names(regionalPars) <- c()


  # calculate regional offset for remaining countries
  remainingOffsets <- uvalueProjection %>%
    filter(!.data$region %in% singleRegions) %>%

    # aggregate to MESSAGE resolution
    rbind(floorspaceWeights %>%
            mutate(variable = "floorspace")) %>%
    mutate(unit = NA) %>%

    # suppress warnings since some regions were intentionally excluded
    (\(x) {
      suppressWarnings(
        aggregate_map(
          x,
          mapping = regionmapMessage,
          by = "region",
          subset2agg = "uvalue",
          weights = "floorspace",
          forceAggregation = TRUE
        )
      )
    })() %>%


    # calculate difference to reference values
    left_join(uvaluesMessageAgg, by = c("region", "period")) %>%
    select("region", "period", "projection" = "value", "reference" = "uvalue") %>%
    filter(!is.na(.data$reference)) %>%
    group_by(across(all_of("region"))) %>%
    reframe(
      # Calculate the weighted mean difference
      meanDiff = mean(.data$reference - .data$projection, na.rm = TRUE),

      # Calculate variance of these differences
      varDiff = var(.data$reference - .data$projection, na.rm = TRUE),

      # Calculate total variance (model residual variance + region variance)
      totalVar = .data$varDiff + sigma(model)^2,

      # Calculate shrinkage factor using empirical Bayes
      shrinkage = .data$varDiff / .data$totalVar,

      # Calculate offset with appropriate shrinkage
      offset = .data$shrinkage * .data$meanDiff
    ) %>%
    select("regionTarget" = "region", "offset")


  # Determine remaining fit parameters
  remainingRegionalPars <- remainingOffsets %>%
    left_join(regionmapMessage, by = c("regionTarget")) %>%
    filter(!.data$region %in% singleRegions) %>%
    select("region", "offset") %>%

    # add global parameters
    mutate(!!!setNames(globalPars[unlist(names(varMapFitPars))], unlist(varMapFitPars))) %>%

    # adjust (Intercept) with offset
    mutate("parINTERCEPT" = .data[["parINTERCEPT"]] + .data$offset) %>%
    select(-"offset")


  # Bind full set of parameters
  fullPars <- regionalPars %>%
    rbind(remainingRegionalPars) %>%
    mutate(minU = minU) %>%
    pivot_longer(cols = -all_of("region"), names_to = "variable", values_to = "value")



  # OUTPUT ---------------------------------------------------------------------

  fullPars <- fullPars %>%
    as.magpie()

  floorspaceWeights <- floorspaceWeights %>%
    filter(.data$period == endOfHistory) %>%
    select(-"period") %>%
    as.magpie()

  return(list(x = fullPars,
              weights = floorspaceWeights,
              unit = "",
              description = "Regional fit parameters for u-value projections"))

}
