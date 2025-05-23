#' Convert VHK data
#'
#' @author Robin Hasse
#'
#' @param x MagPie object
#' @returns MagPIE object
#'
#' @importFrom dplyr %>% .data  mutate distinct across all_of filter arrange
#'   case_when
#' @importFrom magclass as.magpie
#' @importFrom madrat toolCountry2isocode toolCountryFill
#' @export

convertVHK <- function(x) {
  data <- as_tibble(x) %>%
    filter(!.data$region %in% c("EU"),
           !is.na(.data$value)) %>%
    mutate(region = case_when(.data$region == "BU" ~ "BG",
                              .data$region == "EI" ~ "IE",
                              .default = .data$region),
           region = toolCountry2isocode(.data$region)) %>%
    distinct(across(all_of(c("region", "period", "variable"))),
             .keep_all = TRUE) %>%
    arrange(.data$region, .data$period) %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value",
              tidy = FALSE) %>%
    toolCountryFill(verbosity = 2)
  return(data)
}
