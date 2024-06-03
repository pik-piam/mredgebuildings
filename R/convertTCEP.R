#' Convert TCEP data base
#'
#' @param x MAgPIE object with data from TCEP #nolint
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn, Robin Hasse
#'
#' @importFrom dplyr %>% select mutate
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom quitte as.quitte aggregate_map revalue.levels
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping calcOutput
#' @export

convertTCEP <- function(x) {

  # READ-IN DATA ---------------------------------------------------------------

  data <- x


  # Enduse Mapping
  # AL didn't map Middle East & Africa. Instead he mapped IND & other dev. Asia
  # to both MIE and AFR.
  # RH: I now map Middle East & Africa to MIE and AFR
  # TODO: check this mapping #nolint
  tcepMap <- toolGetMapping("regionmappingTCEP2.csv",
                            type = "regional",
                            where = "mredgebuildings")

  # Population
  # TODO: check if FE demand in buildings should be used as weights #nolint‚
  pop <- calcOutput("PopulationPast", aggregate = FALSE) %>%
    as.quitte()


  # PARAMETERS -----------------------------------------------------------------

  enduseMapping <- c(
    "Space heating"         = "space_heating",
    "Space cooling"         = "space_cooling",
    "Water heating"         = "water_heating",
    "Lighting"              = "lighting",
    "Cooking"               = "cooking",
    "Appliances and other"  = "appliances"
  )


  # PROCESS DATA ---------------------------------------------------------------

  # remap enduse names
  data <- data %>%
    as.quitte() %>%
    select("region", "period", "variable", "unit", "value") %>%
    revalue.levels(variable = enduseMapping)

  # correct FE enduse data for "appliances" in India
  data <- data %>%
    mutate(value = ifelse(.data[["region"]] == "India and other dev. Asia" &
                            .data[["period"]] == 2000 &
                            .data[["variable"]] == "appliances",
                          .data[["value"]] * 0.70,
                          .data[["value"]]))


  # Disaggregate to ISO Level
  data <- data %>%
    mutate(region = gsub("_", ".", .data[["region"]])) %>%
    aggregate_map(
      mapping = tcepMap[, c("CountryCode", "RegionCode")],
      by = c("region" = "RegionCode"),
      weights = pop %>%
        rename(weight_val_col = "value") %>%
        select("region", "period", "weight_val_col"),
      weight_item_col = "region",
      forceAggregation = TRUE
    ) %>%
    select("region", "period", "variable", "value") %>%
    rename(enduse = "variable")



  # OUTPUT ---------------------------------------------------------------------

  data <- data %>%
    as.magpie()

  return(data)

}
