#' Historic carrier-Enduse Shares w.r.t Final Energy
#'
#' Merges and transforms the calculated shares from the Datasets:
#' Odyssee
#' IEA_ETP
#' TCEP
#' WEO
#'
#' Enduse shares are extrapolated using a linear regression where the ETP datapoint
#' as well as selected regions from TCEP serves as origin and the entire TCEP dataset
#' provides the growth factor.
#'
#' Carrier shares are exported w.r.t. to all carriers per region per period. In the case of
#' "carrierCorrection = TRUE", carrier shares are exported w.r.t. their contribution
#' to each individual enduse. The reason for the latter is that the implemantation
#' here is closer to EDGE-B, but didn't work well with further data processing
#' steps (e.g. toolDisaggregate) where EC shares are not necessarily used.
#' Hence, this case is by default disabled.
#'
#' In the thermal case, the enduse "appliances" is transformed to "refrigerators"
#' using the region-specific refrigerator share used in EDGE-B. Higher-resoluted
#' data was not available.
#'
#' @param subtype specifies share
#' @param carrierCorrection allows additional corrections
#' @param feOnly specifies if shares or quantities are returned
#' @param feWeights calculate additional FE weights for share aggregation
#'
#' @note The parameter "feOnly" is only applicable to IEA_ETP and TCEP data,
#' since this is the necessary data to do a full disaggregation of EU and EC
#' data, whereas Odyssee already gives disaggregated data for countries of the
#' European Union.
#'
#' @note As done in EDGE-B, enduse-disaggregated FE data from WEO for the regions
#' MIE, AFR and JAP were used instead for values from IEA ETP.
#'
#' @returns data.frame with historic energy demands
#'
#' @author Hagen Tockhorn, Robin Hasse
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate as_tibble filter select rename group_by across
#'   all_of ungroup %>% .data left_join summarise group_modify cross_join
#' @importFrom tidyr replace_na unite complete
#' @importFrom madrat toolGetMapping calcOutput readSource toolCountryFill
#' @importFrom magclass time_interpolate as.magpie
#' @importFrom quitte inline.data.frame as.quitte factor.data.frame
#'   interpolate_missing_periods


calcShares <- function(subtype = c("carrier_nonthermal",
                                   "carrier_thermal",
                                   "enduse_nonthermal",
                                   "enduse_thermal"),
                       carrierCorrection = FALSE,
                       feOnly = FALSE,
                       feWeights = TRUE) {



  # PARAMETERS -----------------------------------------------------------------

  subtype <- match.arg(subtype)

  shareOf  <- strsplit(subtype, "_")[[1]][1]
  thermVar <- strsplit(subtype, "_")[[1]][2]



  #---Corrections for specific carrier shares
  # nolint start
  correction <- inline.data.frame(
    "region;        enduse;                                carrier; value",
    ".*;            water_heating;                         heat;    0",
    "(OCD|EUR|USA); (water_heating|cooking);               biomod;  0",
    "EUR;           (water_heating|cooking);               biotrad; 0",
    "RUS;           space_heating;                         heat;    0.60",
    "RUS;           (water_heating|cooking);               natgas;  0.70",
    "RUS;           (water_heating|cooking);               elec;    0.07",
    "RUS;           space_heating;                         elec;    0.03",
    "RUS;           (space_heating|water_heating|cooking); coal;    0.04",
    "RUS;           (space_heating|water_heating|cooking); petrol;  0.10",
    "IND;           (space_heating|water_heating);         biotrad; 0.80",
    "IND;           (space_heating|water_heating|cooking); coal;    0.10",
    "IND;           (space_heating|water_heating);         petrol;  0.12",
    "IND;           (space_heating|water_heating);         natgas;  0",
    "IND;           (space_heating|water_heating);         elec;    0.02",
    "IND;           cooking;                               natgas;  0.04",
    "IND;           cooking;                      (biotrad|petrol); 0.50") %>%
    unite("regex", all_of(c("region", "enduse", "carrier")), sep = "\\.")
  # nolint end


  # Enduse-Carrier combinations which will be systematically excluded
  exclude <- toolGetMapping(name  = "excludeEnduseCarrier.csv",
                            type  = "sectoral",
                            where = "mredgebuildings")


  # Percentage of Appliances for Refrigerators
  fridgeShare <- rbind(data.frame(RegionCode = "USA", share  = 0.12),
                       data.frame(RegionCode = c("EUR", "OCD", "RUS", "JPN"), share = 0.17),
                       data.frame(RegionCode = c("CHN", "IND", "NCD", "AFR", "MIE", "OAS"), share = 0.3))


  # Regions taken into account from WEO
  regWEO <- c("Africa", "Middle East", "Japan")



  # READ-IN DATA ---------------------------------------------------------------

  #--- Main Datasets

  # Shares

  # IEA
  ieaIO <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings",
                      aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE)

  # Odyssee
  sharesOdyssee <-
    calcOutput("ShareOdyssee", subtype = shareOf, aggregate = FALSE) %>%
    as.quitte()

  # IEA ETP
  sharesETP <-
    calcOutput("ShareETP", subtype = shareOf, feOnly = FALSE, aggregate = FALSE) %>%
    as.quitte()

  # WEO
  sharesWEO <- readSource("WEO", subtype = "Buildings") %>%
    as.quitte()

  if (shareOf == "enduse") {
    # TCEP
    sharesTCEP <- calcOutput("ShareTCEP", aggregate = FALSE) %>%
      as.quitte()
  }


  # FE

  # IEA ETP
  feETP <-
    calcOutput("ShareETP", subtype = shareOf, feOnly = TRUE, aggregate = FALSE) %>%
    as.quitte()

  if (shareOf == "enduse") {
    # TCEP
    dataTCEP <- readSource("TCEP") %>%
      as.quitte()
  }


  #--- Mappings

  # EDGE mapping
  regmappingEDGE <- toolGetMapping(name  = "regionmappingEDGE.csv",
                                   type  = "regional",
                                   where = "mredgebuildings")

  # ETP mapping
  regmappingETP <- toolGetMapping(name  = "regionmappingIEA_ETP.csv",
                                  type  = "regional",
                                  where = "mredgebuildings")

  # WEO mapping
  regmappingWEO <- toolGetMapping(name  = "regionmappingWEO.csv",
                                  type  = "regional",
                                  where = "mredgebuildings")




  # FUNCTIONS ------------------------------------------------------------------

  normalize <- function(df, shareOf) {
    df <- df %>%
      group_by(across(-all_of(c(shareOf, "value")))) %>%
      mutate(value = .data[["value"]] / sum(.data[["value"]], na.rm = TRUE)) %>%
      ungroup()
  }


  addThermal <- function(df, mapping, fridgeShare) {
    df <- df %>%
      filter(.data[["enduse"]] != "lighting") %>%
      left_join(regmappingEDGE %>%
                  select(-"RegionCodeEUR", -"RegionCodeEUR_ETP", -"X") %>%
                  rename(region = "CountryCode") %>%
                  left_join(fridgeShare, by = "RegionCode") %>%
                  select(-"RegionCode"),
                by = "region") %>%
      mutate(value = ifelse(.data[["enduse"]] != "appliances",
                            .data[["value"]],
                            .data[["value"]] * .data[["share"]]),
             enduse = ifelse(.data[["enduse"]] == "appliances",
                             "refrigerators",
                             as.character(.data[["enduse"]]))) %>%
      select(-"share")

    if (!feOnly) {
      df <- normalize(df, shareOf)
      return(df)
    }

    return(df)
  }


  # PROCESS DATA ---------------------------------------------------------------

  # Adjust ETP Mapping
  regmappingETP <- regmappingETP %>%
    mutate(EEAReg = ifelse(.data[["EEAReg"]] == "rest",
                           .data[["OECD"]],
                           .data[["EEAReg"]]))

  if (shareOf == "enduse") {
    # Analogous to EDGE-B, the TCEP Dataset is used to determine a growth / evolution
    # factor of a given Enduse Share. This factor will be used to extrapolate a
    # second Datapoint for the ETP Dataset (highest priority), enabling a linear
    # regression of the time evolution. WEO datapoints were used for selected regions.

    # Define TCEP regions for enduse shares
    regWEO <- regmappingWEO %>%
      filter(.data[["RegionCode"]] %in% regWEO) %>%
      pull("CountryCode")

    # combine ETP and WEO enduse shares
    shares <- sharesETP %>%
      select("region", "period", "enduse", "value") %>%
      left_join(sharesWEO %>%
                  filter(.data[["region"]] %in% regWEO,
                         .data[["period"]] %in% unique(sharesETP$period)) %>%
                  select("region", "period", "enduse", "value"),
                by = c("region", "period", "enduse")) %>%
      mutate(value = ifelse(is.na(.data[["value.y"]]),
                            .data[["value.x"]],
                            .data[["value.y"]])) %>%
      select(-"value.x", -"value.y")



    if (!feOnly) {
      # Extrapolate ETP FE Data
      evolutionFactor <- sharesTCEP %>%
        left_join(regmappingETP %>%
                    select("CountryCode", "EEAReg") %>%
                    rename(region = "CountryCode",
                           regionAgg = "EEAReg"),
                  by = "region") %>%
        select("region", "period", "enduse", "regionAgg", "value") %>%
        group_by(across(all_of(c("regionAgg", "enduse", "period")))) %>%
        reframe(value = sum(.data[["value"]], na.rm = TRUE)) %>%
        group_by(across(all_of(c("regionAgg", "enduse")))) %>%
        reframe(factor = .data[["value"]] / dplyr::lead(.data[["value"]])) %>%
        filter(!is.na(.data[["factor"]])) %>%
        left_join(regmappingETP %>%
                    select("CountryCode", "EEAReg") %>%
                    rename(region = "CountryCode",
                           regionAgg = "EEAReg"),
                  by = "regionAgg", relationship = "many-to-many") %>%
        select(-"regionAgg")


      sharesStart <- shares %>%
        left_join(evolutionFactor, by = c("region", "enduse")) %>%
        mutate(value = .data[["value"]] * .data[["factor"]],
               period = 2000) %>%
        select(-"factor") %>%
        filter(!is.na(.data[["value"]]))


      sharesFull <- rbind(sharesStart,
                          shares %>%
                            filter(!is.na(.data[["value"]])))


      sharesFull <- sharesFull %>%
        factor.data.frame() %>%
        as.quitte() %>%
        interpolate_missing_periods(period = seq(1990, 2020)) %>%
        group_by(across(all_of(c("region", "enduse")))) %>%
        group_modify(~ extrapolateMissingPeriods(.x, key = "value")) %>%
        ungroup() %>%
        select("region", "period", "enduse", "value")


      # NOTE: The linear regression might lead to negative values which will be
      # filled up with zeros and then re-normalized.
      # (However, this is a very practical fix...)

      sharesFull <- sharesFull %>%
        mutate(value = ifelse(.data[["value"]] < 0, 1e-6, .data[["value"]]))


      # Merge Data
      data <- sharesOdyssee %>%
        select("region", "period", "enduse", "value") %>%
        left_join(sharesFull, by = c("region", "period", shareOf)) %>%
        mutate(value = ifelse(is.na(.data[["value.x"]]),
                              .data[["value.y"]],
                              .data[["value.x"]])) %>%
        select(-"value.x", -"value.y")
    }

    if (feOnly || feWeights) {
      # Extrapolate ETP FE Data
      evolutionFactor <- dataTCEP %>%
        left_join(regmappingETP %>%
                    select("CountryCode", "EEAReg") %>%
                    rename(region = "CountryCode",
                           regionAgg = "EEAReg"),
                  by = "region") %>%
        group_by(across(all_of(c("regionAgg", "enduse", "period")))) %>%
        summarise(value = sum(.data[["value"]], na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(across(all_of(c("regionAgg", "enduse")))) %>%
        summarise(factor = .data[["value"]] / dplyr::lead(.data[["value"]])) %>%
        ungroup() %>%
        filter(!is.na(.data[["factor"]])) %>%
        left_join(regmappingETP %>%
                    select("CountryCode", "EEAReg") %>%
                    rename(region = "CountryCode",
                           regionAgg = "EEAReg"),
                  by = "regionAgg") %>%
        select(-"regionAgg")


      dataETPstart <- feETP %>%
        left_join(evolutionFactor, by = c("region", "enduse")) %>%
        mutate(value = .data[["value"]] * .data[["factor"]],
               period = 2000) %>%
        select(-"factor") %>%
        filter(!is.na(.data[["value"]]))


      dataETPfull <- rbind(dataETPstart,
                           feETP %>%
                             filter(!is.na(.data[["value"]])))


      dataETPfull <- dataETPfull %>%
        select(-"unit") %>%
        quitte::factor.data.frame() %>%
        as.quitte() %>%
        interpolate_missing_periods(period = seq(1990, 2020)) %>%
        group_by(across(all_of(c("region", "enduse")))) %>%
        group_modify(~ extrapolateMissingPeriods(.x, key = "value")) %>%
        ungroup() %>%
        select("region", "period", "enduse", "value")


      # NOTE: The linear regression might lead to negative values which will be
      # filled up with small values and then re-normalized.
      # (However, this is a very practical fix...)

      dataETPfull <- dataETPfull %>%
        mutate(value = ifelse(.data[["value"]] < 0, 1e-6, .data[["value"]]))

      if (feOnly) {data  <- dataETPfull} #nolint
      else        {regFE <- dataETPfull} #nolint
    }
  }


  #---Carrier Resolution needs special Attention
  if (shareOf == "carrier") {
    # Merge Data
    data <- sharesOdyssee %>%
      select(-"unit", -"variable", -"model", -"scenario") %>%
      left_join(sharesETP, by = c("region", "period", shareOf)) %>%
      mutate(value = ifelse(is.na(.data[["value.x"]]),
                            .data[["value.y"]],
                            .data[["value.x"]])) %>%
      select(-"value.x", -"value.y", -"unit", -"variable", -"model", -"scenario")

    # Interpolate Data
    data <- data %>%
      interpolate_missing_periods(period = seq(1990, 2020), expand.values = TRUE)


    if (carrierCorrection == TRUE) {
      # The Carrier resolution will be extended by a further added resolution for
      # Enduses, which are however not disaggregated. This only serves the purpose
      # of facilitating specific corrections.
      # However, this made the handling in toolDisaggregate more error-prone.

      # NOTE: I haven't followed this approach any further since toolDisaggregate
      # was changed to only take enduse data from an external dataset for its
      # calculation. Hence, "carrierCorrection = FALSE" by default.

      # Add EDGE Mapping
      data <- left_join(data,
                        regmappingEDGE %>%
                          select(-"RegionCodeEUR", -"RegionCodeEUR_ETP", -"X") %>%
                          rename(region = "CountryCode"),
                        by = "region")


      # Load Enduses
      enduses <- calcOutput("ShareOdyssee", subtype = "enduse", aggregate = FALSE) %>%
        as.quitte(na.rm = TRUE) %>%
        select("enduse") %>%
        unique()


      # Cross-join Enduses to Carriers to give further level of resolution
      data <- cross_join(data, enduses)

      # Re-normalize to give Enduse-specific Carrier Share
      data <- normalize(data, shareOf)



      #---Correct some specific EC-EU combinations

      # Specific Exclusion
      data <- data %>%
        unite(col = "EUEC", .data[["enduse"]], .data[["carrier"]], sep = "-", remove = FALSE) %>%
        # anti_join(data.frame("EUEC" = c(exclude, excludeNEW)), by = "EUEC") %>%
        mutate(value = ifelse(.data[["EUEC"]] %in% exclude,
                              0,
                              .data[["value"]])) %>%
        select(-"EUEC")


      # (1) Make "space_cooling" and "appliances" fully electric
      # (2) If only one of "water_heating", "space_heating" is non-empty, use  one for the other
      data <- data %>%
        spread("enduse", "value") %>%
        mutate(space_cooling = ifelse(.data[["carrier"]] == "elec", 1, 0),
               appliances    = ifelse(.data[["carrier"]] == "elec", 1, 0)) %>%
        mutate(space_heating = ifelse(is.na(.data[["space_heating"]]) & !is.na(.data[["water_heating"]]),
                                      .data[["water_heating"]],
                                      .data[["space_heating"]]),
               water_heating = ifelse(is.na(.data[["water_heating"]]) & !is.na(.data[["space_heating"]]),
                                      .data[["space_heating"]],
                                      .data[["water_heating"]])) %>%
        gather("enduse", "value", -any_of(colnames(data))) %>%
        mutate(value = replace_na(.data[["value"]], 0))


      # Make more specific corrections
      for (i in rownames(correction)) {
        data <- data %>%
          mutate(value = unlist(ifelse(
            grepl(correction[i, "regex"],
                  paste(.data[["RegionCode"]], .data[["enduse"]], .data[["carrier"]],
                        sep = ".")),
            correction[i, "value"],
            .data[["value"]]
          )))
      }


      # Remove Mapping Column
      data <- select(data, -"RegionCode")

      # Re-Normalize
      data <- normalize(data, shareOf)
    }
  }


  #---Thermal Part
  if (thermVar == "thermal") {
    if (shareOf == "enduse") {
      data <- data %>%
        addThermal(regmappingEDGE, fridgeShare)

      if (feWeights) {
        regFE <- regFE %>%
          addThermal(regmappingEDGE, fridgeShare)
      }
    }
  }

  #---Weights: Regional Shares of FE
  # Weights consist of the share of each region's FE demand relative to global FE demand.
  # FE data is taken from ieaIO.
  regShare <- ieaIO %>%
    mutate(carrier = .data[["variable"]]) %>%
    select("region", "period", "carrier", "value") %>%
    group_by(across(all_of(c("period", "region")))) %>%
    filter(!all(.data[["value"]] == 0)) %>%
    summarise(value = sum(.data[["value"]], na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(across(all_of(c("region")))) %>%
    complete(period = unique(data$period)) %>%
    ungroup() %>%
    interpolate_missing_periods(expand.values = TRUE) %>%
    group_by(across(all_of(c("period")))) %>%
    mutate(value = proportions(.data[["value"]]))

  regFE <- regFE %>%
    mutate(value = replace_na(.data[["value"]], 0))


  if (feWeights) {
    data <- data %>%
      mutate(value = replace_na(.data[["value"]], 0))
  }




  # OUTPUT ---------------------------------------------------------------------


  data <- data %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(1, verbosity = 2)

  regShare <- regShare %>%
    as.magpie() %>%
    collapseDim() %>%
    time_interpolate(getItems(data, 2)) %>%
    toolCountryFill(1, verbosity = 2)

  regFE <- regFE %>%
    as.magpie() %>%
    collapseDim() %>%
    time_interpolate(getItems(data, 2)) %>%
    toolCountryFill(1, verbosity = 2)



  if (isTRUE(feOnly)) {
    weight <- regShare
    unit <- "EJ"
    max <- NULL

    description <- "Final energy demand of carrier or end use in buildings"
  } else {
    weight <- regFE
    unit <- "1"
    max <- 1
    description <- "Share of carrier or end use in buildings final energy demand"
  }


  return(list(x = data,
              weight = weight,
              unit = unit,
              min = 0,
              max = max,
              description = description))
}
