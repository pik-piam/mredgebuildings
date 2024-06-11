#' fullEDGEBUILDINGS
#'
#' Function that produces the complete ISO data set required for the
#' EDGE - Buildings model.
#'
#' @param rev data revision which should be used as input (positive numeric).
#'
#' @author Antoine Levesque, Robin Hasse
#'
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' fullEDGEBUILDINGS()
#' }
#'
#' @importFrom madrat calcOutput
#' @export

fullEDGEBUILDINGS <- function(rev = 0) {

  # socio-economic data --------------------------------------------------------
  calcOutput("PopulationPast", aggregate = FALSE, file = "f_poppast.cs4r")
  calcOutput("Population",     aggregate = FALSE, file = "f_pop.cs4r")
  calcOutput("GDP",            aggregate = FALSE, file = "f_gdp.cs4r", average2020 = FALSE)
  calcOutput("GDPPast",        aggregate = FALSE, file = "f_gdppast.cs4r")
  calcOutput("RatioPPP2MER",   aggregate = FALSE, file = "f_ppp2mer.cs4r")
  calcOutput("Surface",        aggregate = FALSE, file = "f_surface.cs4r")

  # energy ---------------------------------------------------------------------
  # move calcIO to mrcommons
  calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = FALSE, file = "f_edge_buildings.cs4r")
  calcOutput("IOEdgeBuildings", subtype = "output_EDGE",           aggregate = FALSE, file = "f_edge_stationary.cs4r")
  calcOutput("IEAPFU",                                             aggregate = FALSE, file = "f_iea_pfu.cs4r")
  calcOutput("FloorspacePast",                                     aggregate = FALSE, file = "f_floorspace.cs4r")
  calcOutput("IEAfloorspace",                                      aggregate = FALSE, file = "f_iea_floorspace.cs4r")
  calcOutput("Uvalues", subtype = "EUBuildingsObservatory",        aggregate = FALSE, file = "f_uvalues_rescom.cs4r")
  calcOutput("Uvalues", subtype = "ETSAP",                         aggregate = FALSE, file = "f_uvalues_etsap.cs4r")

  # climate data ---------------------------------------------------------------
  for (tlim in 17:25) {
    calcOutput("HDDCDD", tlimit = tlim, aggregate = FALSE, file = paste0("f_hddcdd_", tlim, ".cs4r"))
  }
}
