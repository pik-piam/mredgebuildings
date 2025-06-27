#' Process data from IEA End Uses and Efficiency Indicators Database
#'
#' IEA EEI final energy data is processed and mapped w.r.t. carrier and enduse names.
#'
#' As for the buildings sector, data for residential and commercial ("service")
#' buildings is aggregated and the carrier "biomass" is split into traditional
#' and modern biomass w.r.t. to income per capita.
#'
#' @param subtype sector name
#' @param mixData logical indicating whether to mix res/com data points if both are not given
#'
#' @return data.frame containing enduse- and carrier-resoluted energy data
#'
#' @author Hagen Tockhorn
#'
#' @importFrom madrat readSource calcOutput toolGetMapping
#' @importFrom dplyr filter group_by across all_of summarise mutate select pull
#'   rename ungroup %>% .data
#' @importFrom tidyr replace_na
#' @importFrom quitte revalue.levels as.quitte
#' @importFrom magclass complete_magpie as.magpie
#' @importFrom mrcommons toolSplitBiomass


calcIEA_EEI <- function(subtype = c("buildings"), #nolint object_name_linter
                        mixData = FALSE) {

  # PARAMETERS -----------------------------------------------------------------

  subtype <- match.arg(subtype)

  # energy unit conversion PJ -> EJ
  pj2ej <- 1e-3 #nolint object_name_linter



  # READ-IN DATA ---------------------------------------------------------------

  # IEA
  data <- readSource("IEA_EEI", convert = TRUE) %>%
    as.quitte()

  # GDP per capita
  gdppop <- calcOutput("GDPpc",
                       scenario = "SSP2",
                       average2020 = FALSE,
                       aggregate = FALSE)

  # enduse and carrier mapping
  enduseMap <- toolGetMapping(name = "enduseMap_IEA-EEI.csv",
                              type = "sectoral",
                              where = "mredgebuildings") %>%
    pull("EDGE", "IEA_EEI")
  carrierMap <- toolGetMapping(name = "carrierMap_IEA-EEI.csv",
                               type = "sectoral",
                               where = "mredgebuildings") %>%
    pull("EDGE", "IEA_EEI")



  # PROCESS DATA ---------------------------------------------------------------

  if (subtype == "buildings") {

    dataAgg <- data %>%
      # filter residential and service data and do some pre-processing
      rename("carrier" = "ITEM",
             "enduse"  = "ENDUSE") %>%
      filter(.data[["enduse"]] %in% names(enduseMap),
             .data[["carrier"]] %in% names(carrierMap)) %>%
      # revalue carrier/enduse names
      revalue.levels(carrier = carrierMap,
                     enduse  = enduseMap) %>%
      # sum up service and residential data
      group_by(across(-all_of("value"))) %>%
      summarise(value = sum(.data$value, na.rm = isTRUE(mixData)),
                .groups = "drop") %>%
      # only keep region/periods with data
      group_by(across(all_of(c("region", "period", "enduse")))) %>%
      filter(any(.data$value > 0)) %>%
      ungroup() %>%
      # convert unit to EJ
      mutate(value = replace_na(.data[["value"]], 0) * pj2ej)


    # split biomass into traditional + modern biomass
    data <- dataAgg %>%
      select("region", "period", "carrier", "enduse", "value") %>%
      as.quitte() %>%
      as.magpie() %>%
      toolSplitBiomass(gdppop) %>%
      toolCountryFill(verbosity = 2)
  }



  # OUTPUT ---------------------------------------------------------------------

  return(list(x = data,
              weight = NULL,
              unit = "EJ/yr",
              min = 0,
              description = "IEA End Uses and Efficiency Indicators Database"))

}
