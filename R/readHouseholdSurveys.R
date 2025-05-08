#' Read household survey data on energy carrier use for cooking and lighting
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

readHouseholdSurveys <- function(subtype = c("cooking", "lighting")) {

  # read data sheet
  data <- switch(subtype,
                 cooking  = read.xlsx("Cooking_fuels.xlsx"),
                 lighting = read.xlsx("Lighting_fuels.xlsx"))

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
    rename("period" = "year") %>%
    pivot_longer(cols = -all_of(c("region", "period")), names_to = "carrier", values_to = "value") %>%
    mutate(carrier = sub("\\.", "_", .data$carrier),
           enduse = subtype,
           period = ifelse(is.na(.data$period), median(.data$period, na.rm = TRUE), .data$period)) %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
