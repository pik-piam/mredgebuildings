#' calculate renovation cost
#'
#' floor-space specific renovation cost
#'
#' @param energyLadder logical, should the calculation include renovation
#'   transitions that are considered a decline on the energy ladder?
#' @param granularity character, name of BRICK granularity
#' @returns MagPIE object with floor-space specific renovation cost depending on
#'   the initial and final state of the building
#'
#' @author Robin Hasse
#'
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom magclass as.magpie
#' @importFrom dplyr %>% .data mutate select left_join filter select cross_join
#'   group_by ungroup
#' @importFrom tidyr pivot_wider
#' @export

calcCostRenovation <- function(energyLadder = FALSE, granularity = NULL) {

  # ASSUMPTIONS ----------------------------------------------------------------


  ## Vintage mark up ====

  # quadratically increasing factor to increase shell cost for old buildings
  vinFactorMax <- 1.5
  vinFactorStart <- 2020
  vinFactor <- toolGetMapping("dim_vin.csv",
                              type = "sectoral", where = "brick") %>%
    mutate(avgYear = (.data[["from"]] + .data[["to"]]) / 2,
           factor = ifelse(.data[["avgYear"]] < vinFactorStart,
                           (vinFactorStart - .data[["avgYear"]]) ^ 2,
                           0),
           factor = 1 + (vinFactorMax - 1) * .data[["factor"]] / max(.data[["factor"]])) %>%
    select("vin", "factor")


  ## Heating mark down ====

  # decrease installation cost if there is no technology switch
  sameHsFactor <- 0.2



  # CALCULATE ------------------------------------------------------------------


  ## Building shell ====

  ### relative demand ####
  relDem <- toolGetMapping("dim_bs.csv",
                           type = "sectoral", where = "brick") %>%
    select("bs", "relDem") %>%
    unique()

  # building shell states
  bs <- getElement(relDem, "bs")
  bsr <- c(bs, "0")  # 0: no change of building shell

  ### renovation transitions ####

  # all theoretical building shell transitions
  shellTransitions <- expand.grid(bs = bs, bsr = bsr)

  # calculate renovation depth
  shellTransitions <- shellTransitions %>%
    left_join(relDem, by = "bs") %>%
    left_join(relDem, by = c(bsr = "bs"), suffix = c(".initial", ".final")) %>%
    mutate(depth = .data[["relDem.initial"]] - .data[["relDem.final"]],
           depth = replace_na(.data[["depth"]], 0)) %>%
    select("bs", "bsr", "depth")

  # remove transitions to worse states
  shellTransitions <- shellTransitions %>%
    filter(.data[["depth"]] >= 0 | is.na(.data[["depth"]]))

  ### calculate transition costs ####

  # transition cost as a function of depth
  shellCostModel <- calcOutput("RenovationCostModel", aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE) %>%
    select(-"model", -"scenario", -"unit") %>%
    pivot_wider(names_from = "variable", values_from = "value")

  # calculate cost
  shellCost <- cross_join(shellCostModel, shellTransitions) %>%
    mutate(shellCost = ifelse(
      .data[["bsr"]] == "0",
      0,
      .data[["intercept"]] + .data[["slope"]] * .data[["depth"]]
    )) %>%
    select(-"intercept", -"slope", -"depth")

  # add vintage dimension and assume mark up for older buildings
  shellCost <- cross_join(shellCost, vinFactor) %>%
    mutate(shellCost = .data[["shellCost"]] * .data[["factor"]]) %>%
    select(-"factor")


  ## Heating system ====

  ### renovation transitions ####

  # heating systems hierarchy
  heatingLadder <- toolGetMapping("dim_hs.csv",
                                  type = "sectoral", where = "brick") %>%
    select("hs", ladder = "energyLadder")

  # heating system states
  .hs <- heatingLadder %>%
    getElement("hs") %>%
    unique()
  .hsr <- c(.hs, "0")
  heatingTransitions <- expand.grid(hs = .hs, hsr = .hsr,
                                    stringsAsFactors = FALSE)

  # do not allow to go down the energy ladder
  if (energyLadder) {
    heatingTransitions <- heatingTransitions %>%
      left_join(heatingLadder, by = "hs") %>%
      left_join(heatingLadder, by = c(hsr = "hs"),
                suffix = c(".initial", ".final")) %>%
      mutate(ladder.final = ifelse(.data[["hsr"]] == "0",
                                   .data[["ladder.initial"]],
                                   .data[["ladder.final"]])) %>%
      filter(.data[["ladder.initial"]] >= .data[["ladder.final"]]) %>%
      select("hs", "hsr")
  }



  ### calculate transition cost ####

  # preliminary floor-space specific capacity in kW/m2
  heatingCapacity <- calcOutput("HeatingCapacity", swissFormular = TRUE,
                                aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE) %>%
    select("region", "typ", "vin", "bs", capacity = "value")

  # heating system purchasing cost: USD/kW -> USD/m2
  heatingCost <- calcOutput("HeatingSystem",
                            subtype = "Purchasing cost",
                            aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE) %>%
    interpolate_missing_periods(unique(shellCost[["period"]]),
                                expand.values = TRUE) %>%
    filter(.data[["period"]] %in% unique(shellCost[["period"]])) %>%
    left_join(heatingCapacity, by = c("region", "typ"),
              relationship = "many-to-many") %>%
    mutate(heatingCost = .data[["value"]] * .data[["capacity"]]) %>%
    select("region", "period", "bs", "hs", "typ", "vin", "heatingCost")

  # add dimension: initial heating system,
  heatingCost <- heatingTransitions %>%
    mutate(hs.final = ifelse(.data[["hsr"]] == "0",
                             .data[["hs"]],
                             .data[["hsr"]])) %>%
    left_join(heatingCost, by = c(hs.final = "hs"),
              relationship = "many-to-many") %>%
    mutate(heatingCost = ifelse(.data[["hsr"]] == "0",
                                0,
                                .data[["heatingCost"]])) %>%
    select(-"hs.final")

  # heating system installation cost (independent of capacity)
  # rough numbers from BDEW Heizkostenvergleich cost/floor
  installationCost <- inline.data.frame(
    "typ; installationCost",
    "SFH; 30",
    "MFH; 10",
    "Com; 6"
  )
  heatingCost <- heatingCost %>%
    left_join(installationCost, by = "typ") %>%
    mutate(installationCost = .data[["installationCost"]] *
             ifelse(.data[["hsr"]] == "0",
                    0,
                    ifelse(.data[["hsr"]] == "reel",
                           0.1,
                           ifelse(.data[["hs"]] == .data[["hsr"]],
                                  sameHsFactor,
                                  1))),
           heatingCost = .data[["heatingCost"]] + .data[["installationCost"]]) %>%
    select(-"installationCost")

  # mark up for installation of central heating system (piping, etc.)
  # https://www.checkatrade.com/blog/cost-guides/central-heating-installation-cost/
  heatingCost <- heatingCost %>%
    mutate(heatingCost = .data[["heatingCost"]] +
             ifelse(.data[["hs"]] == "reel" & !.data[["hsr"]] %in% c("0", "reel"),
                    3000 / 150 * 1.21, # units: GBP / m2 * USD/GBP # nolint: commented_code_linter.
                    0))

  ## Total ====

  # add shell and heating transition costs
  renovationCost <- cross_join(shellTransitions[, c("bs", "bsr")],
                               heatingTransitions) %>%
    mutate(bs.final = ifelse(.data[["bsr"]] == "0",
                             .data[["bs"]],
                             .data[["bsr"]])) %>%
    left_join(shellCost, by = c("bs", "bsr"), relationship = "many-to-many") %>%
    left_join(heatingCost,
              by = c("region", "period", bs.final = "bs", "hs", "hsr", "typ", "vin"),
              relationship = "many-to-many") %>%
    mutate(value = .data[["shellCost"]] + .data[["heatingCost"]]) %>%
    select("region", "period", "bs", "hs", "bsr", "hsr", "typ", "vin", "value")



  # RETURN ---------------------------------------------------------------------

  # convert to magpie object
  renovationCost <- renovationCost %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value")

  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    time_interpolate(getItems(renovationCost, 2),
                     extrapolation_type = "constant")

  # aggregate to BRICK granularity
  agg <- toolAggregateBrick(renovationCost, granularity, feBuildings)



  return(list(x = agg$x,
              unit = "USD2020/m2",
              weight = agg$weight,
              min = 0,
              description = "Floor-space specific cost of renovation"))
}
