#' Process data from IEA End Uses and Efficiency Indicators Database
#'
#' IEA EEI final energy data is processed and mapped w.r.t. carrier and enduse names.
#'
#' As for the buildings sector, data for residential and commercial ("service")
#' buildings is aggregated and the carrier "biomass" is split into traditional
#' and modern biomass w.r.t. to income per capita.
#'
#' @param subtype sector name
#'
#' @return data.frame containing enduse- and carrier-resoluted energy data
#'
#' @author Hagen Tockhorn
#'
#' @importFrom madrat readSource
#' @importFrom dplyr filter group_by across all_of summarise mutate select rename
#' @importFrom tidyr replace_na
#' @importFrom quitte revalue.levels


calcIEA_EEI <- function(subtype = c("buildings")) { #nolint object_name_linter

  # PARAMETERS -----------------------------------------------------------------

  subtype <- match.arg(subtype)

  # enduse mapping
  enduseNames <- c(
    "R_SPACE_H"  = "space_heating",
    "R_SPACE_C"  = "space_cooling",
    "R_WATER_H"  = "water_heating",
    "R_COOKING"  = "cooking",
    "R_LIGHTING" = "lighting",
    "R_APPLIANC" = "appliances",
    "S_SPACE_H"  = "space_heating",
    "S_SPACE_C"  = "space_cooling",
    "S_LIGHTING" = "lighting"
  )

  # carrier mapping
  carrierNames <- c(
    "E_OIL"   = "petrol",
    "E_GAS"   = "natgas",
    "E_COAL"  = "coal",
    "E_WOOD"  = "biomass",
    "E_DHEAT" = "heat",
    "E_ELEC"  = "elec"
  )

  # energy unit conversion PJ -> EJ
  pj2ej <- 1e-3 #nolint object_name_linter



  # READ-IN DATA ---------------------------------------------------------------

  # IEA
  data <- readSource("IEA_EEI", convert = TRUE) %>%
    as.quitte()


  # GDP per capita
  gdppop <- calcOutput("GDPPop", aggregate = FALSE) %>%
    as.quitte() %>%
    select(-"model", -"scenario", -"unit")



  # PROCESS DATA ---------------------------------------------------------------

  if (subtype == "buildings") {

    dataAgg <- data %>%
      # filter residential and service data and do some pre-processing
      rename("carrier" = "ITEM",
             "enduse"  = "ENDUSE") %>%
      filter(.data[["enduse"]] %in% names(enduseNames),
             .data[["carrier"]] %in% names(carrierNames)) %>%

      # revalue carrier/enduse names
      revalue.levels(carrier = carrierNames,
                     enduse = enduseNames) %>%
      # sum up service and residential data
      group_by(across(-all_of("value"))) %>%
      summarise(value = sum(ifelse(all(is.na(.data[["value"]])),
                                   NA,
                                   sum(.data[["value"]], na.rm = TRUE))),
                .groups = "drop") %>%
      ungroup() %>%

      # convert unit to EJ
      mutate(value = .data[["value"]] * pj2ej)


    # split biomass into traditional + modern biomass
    dataBio <- dataAgg %>%
      select("region", "period", "carrier", "enduse", "value") %>%
      rename("variable" = "carrier") %>%
      toolSplitBiomass(gdppop, varName = "biomass") %>%
      rename("carrier" = "variable")

    # prepare for output
    data <- dataBio %>%
      as.quitte() %>%
      as.magpie()
  }



  # OUTPUT ---------------------------------------------------------------------

  return(list(x = data,
              weight = NULL,
              unit = "EJ",
              description = "IEA End Uses and Efficiency Indicators Database"))

}
