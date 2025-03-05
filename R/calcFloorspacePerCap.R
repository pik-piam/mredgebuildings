#' Residential floor space per capita
#'
#' @returns MagPIE object with residential floor space per capita
#'
#' @author Robin Hasse
#'
#' @param scenario A string (or vector of strings) designating the scenario(s) to be returned.
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass getItems getItems<- getSets getSets<- time_interpolate
#'   mselect collapseDim
#'
calcFloorspacePerCap <- function(scenario) {

  # Replace any calls to scenario groups such as "SSPs" and "SSP2IndiaDEAs",
  # to calls of the individual scenarios.
  scenario <- mrdrivers::toolReplaceShortcuts(scenario) %>% unique()

  # TODO: get rid of the mrremind dependency # nolint: todo_comment_linter
  fs <- readSource("EdgeBuildings", subtype = "Floorspace", subset = scenario) %>%
    mselect(variable = "residential", collapseNames = TRUE)

  t <- getItems(fs, 2)

  pop <- calcOutput("PopulationBuildings", aggregate = FALSE) %>%
    time_interpolate(t)
  getSets(pop)[3] <- getSets(fs)[3]
  getItems(pop, "scenario") <- sub("pop_", "", getItems(pop, "scenario"))

  # scale floor space to match stock in 2000
  stock <- calcOutput("BuildingStock", aggregate = FALSE) %>%
    mselect(variable = "floor") %>%
    dimSums(c("variable", "vin", "hs", "bs")) %>%
    time_interpolate(t, extrapolation_type = "constant") %>%
    toolCountryFillAvg(verbosity = 2)
  getSets(stock)[2] <- getSets(fs)[2]
  fs <- fs * collapseDim(dimSums(stock[, 2000, ]) / fs[, 2000, ])

  # split floor space into typ and loc
  fs <- fs * (stock / dimSums(stock))

  # filter common scenarios
  s <- intersect(getItems(fs, "scenario"), getItems(pop, "scenario"))
  fs <- fs[, , s]
  pop <- pop[, , s]

  # floor space per capita
  fsPerCap <- fs / pop
  fsPerCap <- lowpass(fsPerCap)

  return(list(x = fsPerCap,
              weight = pop,
              min = 0,
              description = "Residential floor space per capita",
              unit = "m2/cap"))
}
