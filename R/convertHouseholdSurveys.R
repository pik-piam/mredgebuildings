#' Convert household survey data on energy carrier use for cooking, lighting and appliances ownership rates
#'
#' @references https://hdl.handle.net/10419/301069
#'
#' @param x data.frame containing survey data
#' @param subtype specifies data subset
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr %>% .data across all_of as_tibble filter group_by left_join
#'   mutate reframe rename select ungroup
#' @importFrom madrat calcOutput toolCountry2isocode toolCountryFill
#'   toolGetMapping
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte
#' @importFrom tidyr replace_na

convertHouseholdSurveys <- function(x, subtype) {

  # READ-IN DATA ---------------------------------------------------------------

  #GDP per capita
  gdppop <- calcOutput("GDPpc",
                       scenario = "SSP2",
                       average2020 = FALSE,
                       aggregate = FALSE)


  # carrier variable mapping
  carrierMap <- toolGetMapping("carrierMap_HouseholdSurveys.csv",
                               type  = "sectoral",
                               where = "mredgebuildings")

  # region mapping
  regionmap <- toolGetMapping("regionmappingHouseholdSurveys.csv",
                              type = "regional",
                              where = "mredgebuildings")


  # PROCESS DATA ---------------------------------------------------------------

  # rename regions to ISO alpha3 codes
  data <- x %>%
    as_tibble() %>%
    mutate(region = toolCountry2isocode(country = .data$region,
                                        mapping = setNames(as.list(regionmap$regionTarget),
                                                           regionmap$region))) %>%
    filter(!is.na(.data$value))


  if (subtype == "appliances") {
    data <- data %>%
      as.magpie() %>%
      toolCountryFill(verbosity = 2)

    return(data)
  }


  # aggregate carrier shares
  data <- data %>%
    left_join(carrierMap, by = "carrier") %>%
    group_by(across(all_of(c("region", "period", carrier = "carrierTarget", "enduse")))) %>%
    reframe(value = sum(.data$value, na.rm = TRUE))


  # shares that could not be allocated to a carrier type will be distributed among the others
  dataDistribute <- data %>%
    filter(is.na(.data$carrier)) %>%
    select(-"carrier") %>%
    rename("valueDis" = "value")

  dataRedistributed <- data %>%
    filter(!is.na(.data$carrier)) %>%
    left_join(dataDistribute, by = c("region", "period", "enduse")) %>%
    mutate(valueDis = replace_na(.data$valueDis, 0)) %>%
    group_by(across(all_of(c("region", "period")))) %>%
    mutate(value = .data$value + .data$valueDis * .data$value) %>%
    ungroup() %>%
    select(-"valueDis")


  # split biomass
  if ("biomass" %in% unique(dataRedistributed$carrier)) {
    dataRedistributed <- dataRedistributed %>%
      filter(.data$carrier == "biomass") %>%
      as.magpie() %>%
      toolSplitBiomass(gdppop, "biomass", dim = "carrier") %>%
      as_tibble() %>%
      filter(!is.na(.data$value)) %>%
      rbind(dataRedistributed %>%
              filter(.data$carrier != "biomass"))
  }


  # renormalize
  dataNormalized <- dataRedistributed %>%
    group_by(across(all_of(c("region", "period", "enduse", "carrier")))) %>%
    reframe(value = sum(.data$value, na.rm = TRUE)) %>%
    group_by(across(all_of(c("region", "period", "enduse")))) %>%
    mutate(value = proportions(.data$value)) %>%
    ungroup()


  # OUTPUT ---------------------------------------------------------------------

  data <- dataNormalized %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(verbosity = 2)

  return(data)
}
