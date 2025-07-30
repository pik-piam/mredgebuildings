#' Read household survey data on energy carrier use for cooking, lighting and appliances ownership rates
#'
#' @references https://hdl.handle.net/10419/301069
#'
#' @param subtype Character specifying end-use
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_longer
#' @importFrom madrat toolCountry2isocode toolGetMapping
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie

readHouseholdSurveys <- function(subtype = c("cooking", "lighting", "appliances")) {

  # read data sheet
  data <- switch(subtype,
                 cooking    = read.xlsx("Cooking_fuels.xlsx"),
                 lighting   = read.xlsx("Lighting_fuels.xlsx"),
                 appliances = read.xlsx("Appliances.xlsx"))

  # region mapping
  regionmap <- toolGetMapping("regionmappingHouseholdSurveys.csv",
                              type = "regional",
                              where = "mredgebuildings")

  # standardize column names
  colnames(data) <- tolower(colnames(data))

  # change country codes
  data <- data %>%
    mutate(region = toolCountry2isocode(country = .data$country_long,
                                        mapping = setNames(as.list(regionmap$regionTarget),
                                                           regionmap$region))) %>%
    select(-"country_long") %>%
    rename("period" = "year")

  data <- if (subtype == "appliances") {
    data %>%
      pivot_longer(cols = -all_of(c("region", "period")),
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(variable = sub(".01", "", .data$variable),
             value = replace_na(.data$value, 0))
  } else {
    data %>%
      pivot_longer(cols = -all_of(c("region", "period")),
                   names_to = "carrier",
                   values_to = "value") %>%
      mutate(carrier = sub(".\\", "", .data$carrier),
             enduse  = subtype)
  }

  data <- data %>%
    mutate(period = ifelse(is.na(.data$period), median(.data$period, na.rm = TRUE), .data$period)) %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
