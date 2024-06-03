#' Convert WEO enduse shares w.r.t. to global final energy demand
#'
#' @param x readWEO object
#'
#' @author Hagen Tockhorn
#'
#' @importFrom madrat toolGetMapping
#' @importFrom quitte aggregate_map

convertWEO <- function(x) {

  # READ DATA-------------------------------------------------------------------

  data <- as.quitte(x)

  # region mapping
  regmapping <- toolGetMapping(name  = "regionmappingWEO.csv",
                               type  = "regional",
                               where = "mredgebuildings")

  # fe weights
  fe <- calcOutput("ShareETP",
                   subtype = "enduse",
                   aggregate = FALSE,
                   feOnly = TRUE) %>%
    as.quitte()


  # CORRECTIONS-----------------------------------------------------------------

  data <- data %>%
    select("region", "period", "enduse", "value") %>%
    rename(variable = "enduse") %>%
    mutate(unit = NA)

  # Taken from EDGE-B by Antoine Levesque:
  # "OCD's appliances and lighting is too high compared to electricity consumption"

  dataCorr <- data %>%
    mutate(value = ifelse(.data[["region"]] == "Other OECD" &
                            .data[["variable"]] %in% c("appliances", "lighting"),
                          .data[["value"]] * 0.56,
                          .data[["value"]])) %>%
    mutate(value = ifelse(.data[["region"]] == "Middle East" &
                            .data[["variable"]] %in% c("appliances", "lighting", "space_cooling"),
                          .data[["value"]] * 0.65,
                          .data[["value"]])) %>%
    mutate(value = ifelse(.data[["region"]] == "Southeast Asia" &
                            .data[["variable"]] %in% c("appliances", "lighting", "space_cooling"),
                          .data[["value"]] * 0.66,
                          .data[["value"]]))


  # PROCESS DATA----------------------------------------------------------------

  fe <- fe %>%
    select("region", "period", "enduse", "value") %>%
    rename("variable" = "enduse")


  dataDisagg <- dataCorr %>%
    aggregate_map(mapping = regmapping,
                  by = c(region = "RegionCode"),
                  weights = fe %>%
                    rename(weight_val_col = "value"),
                  weight_item_col = "region",
                  forceAggregation = TRUE) %>%
    rename(enduse = "variable")


  data <- dataDisagg %>%
    select("region", "period", "enduse", "value")


  # OUTPUT----------------------------------------------------------------------

  data <- data %>%
    as.quitte() %>%
    as.magpie %>%
    toolCountryFill()

  return(data)
}
