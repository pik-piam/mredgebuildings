#' fullBRICK
#'
#' Compute complete input data set for BRICK
#'
#' @author Robin Hasse
#'
#' @param rev data revision which should be used as input (positive numeric).
#'
#' @importFrom madrat calcOutput

fullBRICK <- function(rev = 0) {

  # Building stock -------------------------------------------------------------

  calcOutput("BuildingStock", subtype = "residential", file = "f_buildingStock.cs4r")



  # Housing demand -------------------------------------------------------------

  calcOutput("PopulationBuildings", file = "f_population.cs4r")
  calcOutput("FloorspacePerCap",    file = "f_floorspacePerCap.cs4r")



  # Costs ----------------------------------------------------------------------

  # floor-space specific cost
  calcOutput("CostConstruction", file = "f_costConstruction.cs4r")
  calcOutput("CostRenovation",   file = "f_costRenovation.cs4r")
  calcOutput("CostDemolition",   file = "f_costDemolition.cs4r")

  # components of operational cost
  calcOutput("CarrierPrices",                         file = "f_carrierPrices.cs4r")
  calcOutput("UEdemand",                              file = "f_ueDemand.cs4r")
  calcOutput("HeatingSystem", subtype = "Efficiency", file = "f_heatingEfficiency.cs4r")

  calcOutput("RenDepth", aggregate = FALSE,           file = "f_renovationDepth.cs4r")



  # Life time ------------------------------------------------------------------

  calcOutput("LifetimeParams", subtype = "building",      file = "f_lifetimeBuilding.cs4r")
  calcOutput("LifetimeParams", subtype = "heatingSystem", file = "f_lifetimeHeatingSystem.cs4r")
  calcOutput("LifetimeParams", subtype = "buildingShell", file = "f_lifetimeBuildingShell.cs4r")
}
