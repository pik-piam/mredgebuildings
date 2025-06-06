#' fullEDGEBUILDINGS
#'
#' Function that produces the complete ISO data set required for the
#' EDGE - Buildings model.
#'
#' @param rev data revision which should be used as input (positive numeric).
#'
#' @author Antoine Levesque, Robin Hasse, Hagen Tockhorn
#'
#' @seealso
#' \code{\link[madrat]{readSource}},\code{\link[madrat]{getCalculations}},\code{\link[madrat]{calcOutput}}
#' @examples
#' \dontrun{
#' fullEDGEBUILDINGS()
#' }
#'
#' @importFrom madrat calcOutput
#' @export

fullEDGEBUILDINGS <- function(rev = 0) {

  # Scenarios: High India for population and gdp, but not for urbanization -----
  scensIndia <- c("SSPs", "SDPs", "SSP2IndiaHigh")
  scenarios <- c("SSPs", "SDPs")

  # socio-economic data --------------------------------------------------------
  calcOutput("Population",  scenario = scensIndia, naming = "scenario", file = "f_pop.cs4r")
  calcOutput("GDP",
             scenario = scensIndia,
             naming = "scenario",
             average2020 = FALSE,
             file = "f_gdp.cs4r")
  calcOutput("Surface",                           file = "f_surface.cs4r")
  calcOutput("Urban",       scenario = scenarios, naming = "scenario", file = "f_urban.cs4r")

  # climate data ---------------------------------------------------------------
  calcOutput("HDDCDD", fromSource = TRUE, file = "f_hddcdd.cs4r")

  # energy ---------------------------------------------------------------------
  calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", file = "f_edge_buildings.cs4r")
  calcOutput("IOEdgeBuildings", subtype = "output_EDGE",           file = "f_edge_stationary.cs4r")
  calcOutput("FloorspacePast",                                     file = "f_floorspace.cs4r")
  calcOutput("TCEP", subtype = "floorspace",    aggregate = FALSE, file = "f_floorspace_tcep.cs4r")
  calcOutput("FEbyEUEC",                                           file = "f_fe.cs4r")
  calcOutput("FEUEefficiencies",                                   file = "f_feue_efficiencies.cs4r")
  calcOutput("EfficiencyRegression",            aggregate = FALSE, file = "f_feue_efficiencyPars.cs4r")
  calcOutput("UvalueParameters",                                   file = "f_uvaluePars.cs4r")

}
