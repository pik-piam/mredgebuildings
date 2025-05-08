#' Convert household survey data on energy carrier use for cooking and lighting
#'
#' @references https://hdl.handle.net/10419/301069
#'
#' @param x data.frame containing survey data
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr left_join mutate group_by across all_of ungroup reframe as_tibble
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping

convertHouseholdSurveys <- function(x) {

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


  # PROCESS DATA ---------------------------------------------------------------

  # aggregate carrier shares
  data <- x %>%
    as_tibble() %>%
    filter(!is.na(.data$value)) %>%
    left_join(carrierMap, by = "carrier") %>%
    group_by(across(all_of(c("region", "period", "carrierTarget", "enduse")))) %>%
    reframe(value = sum(.data$value, na.rm = TRUE)) %>%
    rename("carrier" = "carrierTarget")


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
    mutate(value = .data$value + .data$valueDis / n()) %>%
    ungroup() %>%
    select(-"valueDis")


  # split biomass
  dataSplitBiomass <- dataRedistributed %>%
    filter(.data$carrier == "biomass") %>%
    as.magpie() %>%
    toolSplitBiomass(gdppop, "biomass", dim = "carrier") %>%
    as_tibble() %>%
    filter(!is.na(.data$value))


  # renormalize
  dataNormalized <- dataSplitBiomass %>%
    rbind(dataRedistributed %>%
            filter(.data$carrier != "biomass")) %>%
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
