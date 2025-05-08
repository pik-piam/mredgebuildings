#' Read data from the European Census Hub
#'
#' The Census Hub of the European Statistical System provides census data from
#' 2011 on population and housing of EU member states. 2021 census data is
#' announced to be published there too.
#' - typeVintage: number of dwellings of different construction periods and
#' building types (single, two, multiple dwellings). Show data on: dwellings,
#' location: nations, Topics: Type of building, Period of construction
#' (select all) -> CSV (Separator: Comma)
#'
#' @source https://ec.europa.eu/CensusHub2/
#'
#' @param subtype census subset
#' @returns MAgPIE object with data
#'
#' @author Robin Hasse
#'
#' @importFrom madrat toolSubtypeSelect
#' @importFrom dplyr %>% select rename mutate .data distinct across all_of
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

readCensusHub <- function(subtype) {

  # pick file
  files <- list(typeVintage = c("csvoutput_HC53_2025_01_07_10_52.csv",
                                "csvoutput_DF_HC37_2021_2025_04_29_11_01.csv"))



  # FUNCTIONS ------------------------------------------------------------------

  removeTrailingComma <- function(x) {
    sub(",$", "", x)
  }

  removeCommaInText <- function(x) {
    gsub(", ", " ", x)
  }

  readCsv <- function(file) {
    readLines(file) %>%
      removeCommaInText() %>%
      removeTrailingComma() %>%
      textConnection() %>%
      read.csv(na.strings = ":")
  }



  # READ -----------------------------------------------------------------------

  data <- toolSubtypeSelect(subtype, files) %>%
    lapply(readCsv) %>%
    do.call(what = rbind) %>%
    select(-"FLAGS", -"FOOTNOTES", -"PROVIDER") %>%
    rename(region = "GEO",
           period = "TIME",
           value = "VALUE") %>%
    mutate(value = as.numeric(.data[["value"]])) %>%
    distinct(across(-all_of("value")), .keep_all = TRUE) %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
