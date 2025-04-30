#' Convert data from MessageIX-Buildings
#'
#' @param x read-in data object
#' @param subtype specifies data base category
#'
#' @export

convertMessageIXBuildings <- function(x, subtype) {

  # READ-IN DATA ---------------------------------------------------------------

  data <- as.quitte(x)

  # MessageIX region mapping
  mapMessage <- toolGetMapping("regionmappingMessageIX.csv",
                               type = "regional", where = "mredgebuildings")


  # PROCESS DATA ---------------------------------------------------------------

  # convert regions to ISO country level
  data <- data %>%
    left_join(mapMessage %>%
                select("CountryCode", "X11regions"),
              by = c("region" = "X11regions"),
              relationship = "many-to-many") %>%
    select(-"region") %>%
    rename("region" = "CountryCode")


  # last historical point in MessageIX is 2015
  if (subtype == "uvalue") {
    data <- data %>%
      mutate(period = 2015)
  }



  # OUTPUT ---------------------------------------------------------------------

  data <- data %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(NA, verbosity = 2, no_remove_warning = c("YUG", "ANT"))


  return(data)
}
