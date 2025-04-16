#' Calculate end use shares from WEO 2015
#'
#' Table 10.2 in the WEO 2015 report lists the chare of regional enduses in the
#' global final energy demand. Here, we calculate the share of enduses in the
#' final energ demand for buildings in each region.
#'
#' @author Robin Hasse
#'
#' @importFrom madrat readSource
#' @importFrom magclass dimSums
#' @export

calcShareWEO <- function() {

  # WEO mapping
  regmappingWEO <- toolGetMapping(name  = "regionmappingWEO.csv",
                                  type  = "regional",
                                  where = "mredgebuildings")
  # calculate shares
  weo <- readSource("WEO", subtype = "Buildings", convert = FALSE)
  shares <- weo / dimSums(weo)

  shares <- shares %>%
    as_tibble() %>%
    filter(.data$region != "World") %>%
    left_join(regmappingWEO, by = c("region" = "RegionCode"), relationship = "many-to-many") %>%
    select(-"region", "region" = "CountryCode", "period", "enduse", "value") %>%
    as.quitte() %>%
    as.magpie()



  return(list(x = shares,
              min = 0,
              max = 1,
              unit = "1",
              isocountries = FALSE,
              description = "regional end use shares in buildings final energy demand"))
}
