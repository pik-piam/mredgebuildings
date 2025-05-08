#' Calculate U-value
#'
#' @author Robin Hasse
#'
#' @importFrom madrat readSource toolGetMapping
#' @importFrom magclass mselect getItems
#' @importFrom quitte inline.data.frame as.quitte
#' @importFrom dplyr .data %>% select filter right_join left_join group_by
#'   across all_of summarise
#' @export

calcUValue <- function() {

  # map Hotmaps vintages
  vinMap <- toolGetMapping("vintageMapping_Hotmaps.csv",
                           type = "sectoral", where = "mredgebuildings") %>%
    select("vin", "vinHotmaps")

  # map Hotmaps building types
  typMap <- toolGetMapping("buildingTypeMapping_Hotmaps.csv",
                           type = "sectoral", where = "mredgebuildings") %>%
    select("typ", building = "typHotmaps")

  # Simple average of U-values across building components
  uval <- readSource("Hotmaps") %>%
    as.quitte(na.rm = TRUE) %>%
    filter(grepl("U-values", .data[["variable"]])) %>%
    right_join(typMap, by = "building") %>%
    left_join(vinMap, by = c(bage = "vinHotmaps"),
              relationship = "many-to-many") %>%
    group_by(across(all_of(c("region", "period", "vin", "typ")))) %>%
    summarise(uval = mean(.data[["value"]]), .groups = "drop")

  # convert to magpie object and fill missing values with simple average
  uval <- uval %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "uval") %>%
    toolCountryFillAvg(verbosity = 2)

  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    mselect(period = getItems(uval, 2))


  return(list(x = uval,
              unit = "W/m2/K",
              weight = feBuildings,
              min = 0,
              description = "Estimate of thermal transmittance of buildings"))
}
