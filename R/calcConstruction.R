#' Calculate historic construction flows
#'
#' @author Robin Hasse
#'
#' @importFrom madrat readSource calcOutput
#' @export

calcConstruction <- function() {

  mapTyp <- toolGetMapping("structuremapping_Destatis_31121-0005_BAUGB7.csv",
                           "sectoral", "mredgebuildings")
  mapHs <- toolGetMapping("structuremapping_Destatis_31121-0005_ENRAT1.csv",
                          "sectoral", "mredgebuildings")

  constrBuildings <- readSource("Destatis", "31121-0005")["DEU", , ] %>%
    mselect(ENRVW1 = "ENR-HZG-PRM") %>%
    as.quitte(na.rm = TRUE) %>%
    right_join(mapTyp, by = "BAUGB7") %>%
    right_join(mapHs, by = "ENRAT1") %>%
    group_by(across(all_of(c("region", "period", "typ", "hs")))) %>%
    summarise(nBuildings = sum(.data[["value"]]),
              nBoilers = sum(.data[["value"]] * .data[["nBoilersPerBuilding"]]),
              unit = "1/yr",
              .groups = "drop") %>%
    pivot_longer(c("nBuildings", "nBoilers"), names_to = "variable")

  constrBuildings <- constrBuildings %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value") %>%
    toolCountryFill(verbosity = 2)

  return(list(x = constrBuildings,
              weight = NULL,
              min = 0,
              unit = "1/yr",
              description = "Construction of new buildings"))
}
