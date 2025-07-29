#' fullBRICK
#'
#' Compute complete input data set for BRICK
#'
#' @author Robin Hasse
#'
#' @param rev data revision which should be used as input (positive numeric).
#' @param granularity character, name of BRICK granularity
#'
#' @importFrom madrat calcOutput

fullBRICK <- function(rev = 0, granularity = NULL) {

  scenarios <- c("SSPs")

  # Building stock -------------------------------------------------------------

  calcOutput("BuildingStock", subtype = "residential", granularity = granularity, file = "f_buildingStock.cs4r")



  # Housing demand -------------------------------------------------------------

  calcOutput("PopulationBuildings", granularity = granularity, file = "f_population.cs4r")
  calcOutput("FloorspacePerCap", granularity = granularity, scenarios = scenarios, file = "f_floorspacePerCap.cs4r")



  # Costs ----------------------------------------------------------------------

  # floor-space specific cost
  calcOutput("CostConstruction", granularity = granularity, file = "f_costConstruction.cs4r")
  calcOutput("CostRenovationBS", granularity = granularity, file = "f_costRenovationBS.cs4r")
  calcOutput("CostRenovationHS", granularity = granularity, file = "f_costRenovationHS.cs4r")
  calcOutput("CostDemolition",                              file = "f_costDemolition.cs4r")

  # components of operational cost
  calcOutput("CarrierPrices",                         file = "f_carrierPrices.cs4r")
  calcOutput("UEdemand", granularity = granularity,   file = "f_ueDemand.cs4r")
  calcOutput("HeatingSystem", subtype = "Efficiency", granularity = granularity, file = "f_heatingEfficiency.cs4r")

  calcOutput("RenDepth", aggregate = FALSE,           file = "f_renovationDepth.cs4r")

  calcOutput("DiscountRate",
             granularity = granularity,
             aggregate = FALSE,
             file = "f_discountRate.cs4r")


  # Life time ------------------------------------------------------------------

  calcOutput("LifetimeParams", subtype = "building",
             granularity = granularity,
             file = "f_lifetimeBuilding.cs4r")
  calcOutput("LifetimeParams", subtype = "heatingSystem",
             granularity = granularity,
             file = "f_lifetimeHeatingSystem.cs4r")
  calcOutput("LifetimeParams", subtype = "buildingShell",
             file = "f_lifetimeBuildingShell.cs4r")
}
