#' Read household survey data on energy carrier use for cooking, lighting and appliances ownership rates
#'
#' @references https://hdl.handle.net/10419/301069
#'
#' @param subtype Character specifying end-use
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr %>% .data all_of mutate pull rename select
#' @importFrom madrat toolCountry2isocode toolGetMapping
#' @importFrom magclass as.magpie
#' @importFrom openxlsx read.xlsx
#' @importFrom quitte as.quitte
#' @importFrom tidyr pivot_longer replace_na

readHouseholdSurveys <- function(subtype = c("cooking", "lighting", "appliances")) {

  # read data sheet
  data <- switch(subtype,
                 cooking    = "Cooking_fuels.xlsx",
                 lighting   = "Lighting_fuels.xlsx",
                 appliances = "Appliances.xlsx") %>%
    read.xlsx(sep.names = " ")

  # standardize column names
  colnames(data) <- tolower(colnames(data))

  # change country codes
  data <- data %>%
    rename("region" = "country_long",
           "period" = "year")

  data <- if (subtype == "appliances") {
    data %>%
      pivot_longer(cols = -all_of(c("region", "period")),
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(variable = sub("\\.01$", "", .data$variable),
             value = replace_na(.data$value, 0))
  } else {
    data %>%
      pivot_longer(cols = -all_of(c("region", "period")),
                   names_to = "carrier",
                   values_to = "value") %>%
      mutate(carrier = sub("\\.", "", .data$carrier),
             enduse  = subtype)
  }

  data <- data %>%
    mutate(period = as.integer(.data$period),
           period = ifelse(is.na(.data$period), median(.data$period, na.rm = TRUE), .data$period)) %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
