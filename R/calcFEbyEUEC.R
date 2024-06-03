#' Historic Final Energy Demand by Carrier and Enduse
#'
#' Takes the historic final energy demand by carriers from IEA and disaggregates
#' it into different end uses.
#'
#' The disaggregation is performed such that the aggregated carrier-specific FE
#' values of the IEA IO regions are met as a minimum requirement. In an ideal case,
#' the enduse-specific FE shares are met as well.
#'
#' @note
#' For now, existing disaggregated final energy data with respect to carriers and
#' enduses combined is replaced in the final output. However, since the Odyssee
#' data is largely underestimating the real IEA FE targets, these shall serve as
#' lower boundaries for the disaggregation.
#' Such feature has been implemented in \code{toolDisaggregate} but is not running
#' smoothly yet.
#'
#' @returns data.frame with historic energy demands
#'
#' @author Hagen Tockhorn, Robin Hasse
#'
#' @importFrom dplyr mutate semi_join right_join left_join as_tibble filter
#'   select %>% .data pull
#' @importFrom madrat toolCountryFill calcOutput toolGetMapping
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

calcFEbyEUEC <- function() {

  # READ-IN DATA ---------------------------------------------------------------

  # FE Data
  ieaIO <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings",
                      aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE)


  # FE EU Data
  feOdyssee <- calcOutput("ShareOdyssee",
                          subtype = "enduse_carrier",
                          feOnly = TRUE,
                          aggregate = FALSE) %>%
    as.quitte()


  # FE IEA EEI data
  feIEAEEI <- calcOutput("IEA_EEI",
                         subtype = "buildings",
                         aggregate = FALSE) %>%
    as.quitte()


  # EU Shares
  sharesEU <- calcOutput("Shares",
                         subtype = "enduse_nonthermal",
                         aggregate = TRUE,
                         regionmapping = "regionmappingEUshares.csv") %>%
    as.quitte()


  # EU Shares mapping
  regmapping <- toolGetMapping(name  = "regionmappingEUshares.csv",
                               type  = "regional",
                               where = "mredgebuildings") %>%
    select("region", "regionAgg")



  # PARAMETERS -----------------------------------------------------------------

  # Enduse-Carrier combinations which will be systematically excluded
  exclude <- toolGetMapping(name  = "excludeEnduseCarrier.csv",
                            type  = "sectoral",
                            where = "mredgebuildings")


  # PROCESS DATA ---------------------------------------------------------------

  sharesEU <- sharesEU %>%
    select("region", "period", "enduse", "value")

  ieaIO <- ieaIO %>%
    rename(carrier = "variable") %>%
    semi_join(sharesEU, by = c("period"))


  #--- Prepare toolDisaggregate Input

  # combine the already disaggregated data
  feDisagg <- feOdyssee %>%
    left_join(feIEAEEI,
              by = c("region", "period", "carrier", "enduse")) %>%
    mutate(value = ifelse(is.na(.data[["value.x"]]),
                          .data[["value.y"]],
                          .data[["value.x"]])) %>%
    select("region", "period", "carrier", "enduse", "value")


  # Disaggregate FE with EU/EC Shares
  ieaIODis <- ieaIO %>%
    select("region", "period", "carrier", "value") %>%
    mutate(unit = "fe") %>%
    toolDisaggregate(enduseShares  = sharesEU,
                     exclude       = exclude,
                     dataDisagg    = feDisagg,
                     regionMapping = regmapping,
                     outliers      = c("IND", "CHN", "ZAF")) %>%
    select("region", "period", "unit", "carrier", "enduse", "value")


  # existing enduse-carrier shares are applied directly on IEA data
  dataReplaceFull <- feOdyssee %>%
    select("region", "period", "carrier", "enduse", "value") %>%
    group_by(across(all_of(c("region", "carrier", "enduse")))) %>%
    filter(all(!is.na(.data[["value"]]))) %>%
    ungroup() %>%
    group_by(across(all_of(c("region", "period", "carrier")))) %>%
    mutate(share = proportions(.data[["value"]])) %>%
    ungroup() %>%
    select(-"value") %>%
    left_join(ieaIO, by = c("region", "period", "carrier")) %>%
    mutate(replaceValue = .data[["value"]] * .data[["share"]],
           replaceValue = replace_na(.data[["replaceValue"]], 0)) %>%
    select("region", "period", "carrier", "enduse", "replaceValue")


  # existing disaggregated data replaces values from optimization
  data <- ieaIODis %>%
    left_join(dataReplaceFull, by = c("region", "period", "carrier", "enduse")) %>%
    mutate(value = ifelse(is.na(.data[["replaceValue"]]),
                          .data[["value"]],
                          .data[["replaceValue"]])) %>%
    select("region", "period", "unit", "carrier", "enduse", "value")


  # CORRECTIONS ----------------------------------------------------------------

  # For unknown reasons, the enduse share of "space_cooling" for region "Africa"
  # is not met and will therefore be corrected. Since "space_cooling" only corresponds
  # to the carrier "elec", the correction is straight-forward.
  # TODO: check if this can be fixed #nolint

  elecSpaceCoolingShare <- sharesEU %>%
    filter(.data[["region"]] == "Africa",
           .data[["enduse"]] == "space_cooling") %>%
    select("period", "value") %>%
    rename("share" = "value")

  dataCorr <- data %>%
    left_join(regmapping, by = c("region")) %>%
    filter(.data[["regionAgg"]] == "Africa") %>%
    left_join(elecSpaceCoolingShare, by = c("period")) %>%
    select(-"regionAgg") %>%
    group_by(across(-all_of(c("enduse", "carrier", "share", "value")))) %>%
    mutate(value = ifelse(.data[["enduse"]] == "space_cooling",
                          ifelse(!(.data[["carrier"]] == "elec"),
                                 .data[["value"]],
                                 sum(.data[["value"]], na.rm = TRUE) * .data[["share"]]),
                          .data[["value"]] * (1 - .data[["share"]]))) %>%
    ungroup() %>%
    select(-"share")

  dataFull <- rbind(dataCorr,
                    data %>%
                      left_join(regmapping, by = c("region")) %>%
                      filter(.data[["regionAgg"]] != "Africa") %>%
                      select(-"regionAgg"))



  # OUTPUT ---------------------------------------------------------------------

  # Pack Data
  dataFull <- dataFull %>%
    mutate(scenario = "history") %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(1, verbosity = 2)


  return(list(x = dataFull,
              weight = NULL,
              unit = "EJ",
              description = "Historic Final Energy Data from IEA"))
}
