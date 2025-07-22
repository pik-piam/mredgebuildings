#' Calculation Regression Parameters for FE-UE Efficiency Projection
#'
#' Calculate the regression parameters for the projection of final (FE)
#' to useful (UE) energy conversion projection for all combinations of
#' energy enduses and carriers. The regression parameters correspond to an
#' asymptotic regression model and encompass the parameters Asym, R0 and lrc.
#' For electric space cooling, a bounded logistic function is used with parameters
#' k and x0.
#' All parameters are determined using a nonlinear least-squares regression.
#'
#' This approach closely follows the model by De Stercke et al. which is
#' mainly driven by GDP per Capita.
#'
#' @param gasBioEquality Determines if natural gas and modern biomass share the same efficiencies
#'
#' @references De Stercke, S. (2014). Dynamics of Energy Systems: A Useful
#' Perspective (Interim Report, p. 68). IIASA.
#' http://pure.iiasa.ac.at/id/eprint/11254/1/IR-14-013.pdf
#'
#' @author Hagen Tockhorn
#'
#' @importFrom quitte aggregate_map removeColNa as.quitte
#' @importFrom stats as.formula na.omit nls nls.control
#' @importFrom dplyr mutate select filter left_join group_by across all_of rename coalesce ends_with %>% .data
#' @importFrom tidyr spread unite
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom magclass as.magpie
#' @importFrom minpack.lm nlsLM
#'
#' @export


calcEfficiencyRegression <- function(gasBioEquality = TRUE) {

  # FUNCTIONS ------------------------------------------------------------------

  # Extrapolate historic FE-UE Efficiencies from Fit Function
  getRegressionPars <- function(df, var, weight) {
    # Prepare Historic Data
    dataHist <- df %>%
      removeColNa() %>%
      filter(.data[["variable"]] == var) %>%
      spread("variable", "value") %>%
      na.omit()

    # Replace 0's with small Values to avoid Inf Issues
    dataHist[dataHist[var] == 0, var] <-
      min(dataHist[dataHist[var] != 0, var]) / 10

    # Create Estimation Object for Non-Linear Model
    if (var == "space_cooling.elec") {
      # calculate weighted input variable mix of GDP per cap and period
      dataHist <- dataHist %>%
        mutate(x = log(dataHist$gdppop) * weight + dataHist$period * (1 - weight))

      estimate <- nlsLM(
        "space_cooling.elec ~ min + (max - min) / (1 + exp(-k * (x + x0)))",
        data = dataHist,
        start = list(k = 0.0001, x0 = 1000),
        control = nls.control(maxiter = 500)
      )
    } else {
      estimate <- nls(as.formula(paste(var, "~ SSasymp(gdppop, Asym, R0, lrc)")),
                      data = dataHist)
    }

    return(estimate$m$getPars())
  }



  # READ-IN DATA ---------------------------------------------------------------

  # --- Data

  # Final and useful energy data
  pfu <- calcOutput("PFUDB", aggregate = FALSE) %>%
    as.quitte()

  # Electric space cooling efficiencies
  coolingEfficiencies <- readSource("IEA_coolingEfficiencies") %>%
    as_tibble() %>%
    filter(!is.na(.data$value))

  # GDP per capita
  gdppop <- calcOutput("GDPpc",
                       scenario = "SSP2",
                       average2020 = FALSE,
                       aggregate = FALSE) %>%
    as.quitte()

  gdppopGLO <- calcOutput("GDPpc",
                          scenario = "SSP2",
                          average2020 = FALSE,
                          aggregate = TRUE,
                          regionmapping = "regionalmappingGLO.csv") %>%
    as.quitte()

  # Population
  pop <- calcOutput("PopulationPast", aggregate = FALSE) %>%
    as.quitte()

  # Cooling COP boundaries
  coolingBounds <- toolGetMapping("coolingEfficiencyBoundaries.csv",
                                  type = "sectoral",
                                  where = "mredgebuildings")


  # --- Mappings

  # Region mapping
  regionmapping <- toolGetMapping("pfu_regionmapping.csv", type = "regional", where = "mredgebuildings")

  # Regression parameter corrections
  parsCorrections <- toolGetMapping("correctEfficiencies.csv",
                                    type = "sectoral",
                                    where = "mredgebuildings")

  # Equal efficiency assumptions
  equalEfficiencies <- toolGetMapping("equalEfficiencies.csv",
                                      type = "sectoral",
                                      where = "mredgebuildings")



  # PARAMETERS -----------------------------------------------------------------

  # Minimum Requirement to be considered
  minEfficiency <- 0.05

  # Weight given to log(gdppop) for space_cooling.elec input variable
  #   -> x = log(gdppop) * weight + period * (1 - weight)
  gdppopWeight <- 0.95


  # PROCESS DATA ---------------------------------------------------------------

  coolingBounds <- coolingBounds %>%
    select("variable", "value") %>%
    pivot_wider(names_from = "variable", values_from = "value")


  #--- Calculate existing FE-EU efficiencies

  # Aggregate PFU Data to PFU Country Code
  pfuAggregate <- pfu %>%
    mutate(value = replace_na(.data[["value"]], 0)) %>%
    unite("variable", c("enduse", "carrier"), sep = ".") %>%
    aggregate_map(mapping = regionmapping[!is.na(regionmapping$PFUDB), c("iso", "PFUDB")],
                  by = c("region" = "iso"),
                  forceAggregation = TRUE)

  # Aggregate GDPpop to PFU Country Code
  gdppopAggregate <- gdppop %>%
    aggregate_map(mapping = regionmapping[!is.na(regionmapping$PFUDB), c("iso", "PFUDB")],
                  by = c("region" = "iso"),
                  forceAggregation = TRUE,
                  weights = pop %>%
                    select("region", "period", "value") %>%
                    rename(weight = "value"),
                  weight_item_col = "region",
                  weight_val_col = "weight") %>%
    rbind(gdppopGLO) %>%
    select(-"model", -"scenario", -"unit", -"variable") %>%
    rename("gdppop" = "value")


  # Take average of commercial and residential ACs in stock
  coolingEfficiencies <- coolingEfficiencies %>%
    filter(grepl("stock", .data$variable)) %>%
    group_by(across(-all_of(c("variable", "value")))) %>%
    reframe(value = mean(.data$value)) %>%
    ungroup()


  histEfficiencies <- pfuAggregate %>%
    # Calculate efficiencies from UE/FE data
    select(-"model", -"scenario") %>%
    spread(.data[["unit"]], .data[["value"]]) %>%
    mutate(value = .data[["ue"]] / .data[["fe"]]) %>%
    select(-"fe", -"ue") %>%

    # Filter out unrealistic efficiencies
    filter(.data[["value"]] >= minEfficiency) %>%

    # Replace space_cooling.elec data
    filter(.data$variable != "space_cooling.elec") %>%
    rbind(coolingEfficiencies %>%
            unite("variable", c("enduse", "carrier"), sep = ".") %>%
            select("region", "period", "variable", "value")) %>%

    # join GDP per capita
    left_join(gdppopAggregate, by = c("region", "period")) %>%

    # append AC COP boundaries
    cross_join(coolingBounds)


  euecCombinations <- unique(histEfficiencies$variable)

  #--- Calculate regression parameter
  fitPars <- do.call(rbind, lapply(euecCombinations, function(euec) {
    pars <- getRegressionPars(histEfficiencies, euec, weight = gdppopWeight)
    as.data.frame(do.call(cbind, as.list(pars))) %>%
      mutate(variable = euec) %>%
      pivot_longer(cols = -"variable",
                   names_to = "fitVariable",
                   values_to = "value")
  }))



  # CORRECTIONS ----------------------------------------------------------------

  # Replace regression parameters to account for RH/HP mix in electric heating end-uses
  fitPars <- fitPars %>%
    left_join(parsCorrections %>%
                pivot_longer(cols = -"variable",
                             names_to = "fitVariable",
                             values_to = "correction"),
              by = c("variable", "fitVariable")) %>%
    mutate(value = coalesce(.data$correction, .data$value)) %>%
    select(-"correction")


  # Correct efficiencies of enduse.carrier combinations assumed to be of equal efficiency
  if (isTRUE(gasBioEquality)) {
    fitPars <- fitPars %>%
      left_join(equalEfficiencies, by = "variable") %>%
      left_join(fitPars,
                by = c("equalTo" = "variable", "fitVariable"),
                suffix = c("", ".target")) %>%
      mutate(value = ifelse(!is.na(.data$equalTo), .data[["value.target"]], .data$value)) %>%
      select(-"equalTo", -ends_with(".target"))
  }



  # OUTPUT ---------------------------------------------------------------------

  # Trim Dataframe
  fitPars <- fitPars %>%
    separate("variable", c("enduse", "carrier"), sep = "\\.") %>%
    mutate(region = "GLO") %>%
    rename("variable" = "fitVariable") %>%
    select("region", "carrier", "enduse", "variable", "value") %>%
    as.magpie()


  return(list(x = fitPars,
              isocountries = FALSE,
              description = "Regression Parameter for FE-UE-Efficiency Projection",
              unit = ""))
}
