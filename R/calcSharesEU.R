#' Calculate historical energy end-use shares based on final energy demands
#'
#' Merges and transforms the calculated shares from the datasets:
#' Odyssee 2024
#' IEA Energy Efficiency Indicators 2024
#' IEA Tracking Clean Energy Progress
#' IEA World Energy Outlook 2014
#'
#' End-use shares are extrapolated using a log-linear regression on the existing
#' data points while values in-between existing data are interpolated linearly.
#' Data is merged following a strict priority hierarchy:
#' 1. Odyssee - Provides detailed European data
#' 2. IEA Energy Efficiency Indicators (EEI) - Selected countries, global scope
#' 3. IEA China data
#' 4. IEA World Energy Outlook (WEO) - Only used if EEI has fewer than 2 data points
#'       for a particular region-enduse combination
#' 5. IEA Energy Technology Perspectives (ETP) - Only used if EEI has fewer than 2 data points
#'       and no WEO data is available
#'
#' For specific regions (Africa, Middle East), data points from WEO
#' are prioritized due to their regional specificity. These data points are extrapolated using
#' growth factors determined by Tracking Clean Energy Progress (TCEP) to ensure consistent
#' time series extension.
#'
#' End-use shares are extrapolated using a log-linear regression on existing data points,
#' while values between existing data points are interpolated linearly. The function preserves
#' dataset consistency by avoiding mixing data sources when sufficient data points are available
#' from a single source.
#'
#' In the thermal case, the enduse "appliances" is transformed to "refrigerators"
#' using the region-specific refrigerator share used in EDGE-B. Higher resolved
#' data was not available.
#'
#' @param thermal logical, if \code{TRUE}, the end-use "appliances" shall be transformed
#' into "refrigerators" with appropriate share
#' @param endOfHistory An integer specifying the upper temporal boundary for historical data
#'
#' @returns data.frame with historic end-use shares for each energy carrier
#'
#' @author Hagen Tockhorn, Robin Hasse
#'
#' @importFrom dplyr mutate as_tibble filter select rename group_by across lead
#'   all_of ungroup %>% .data left_join reframe cross_join .data
#' @importFrom tidyr replace_na unite complete
#' @importFrom madrat toolGetMapping calcOutput readSource toolCountryFill
#' @importFrom magclass time_interpolate as.magpie dimSums getItems
#' @importFrom quitte as.quitte interpolate_missing_periods
#'
#' @export

calcSharesEU <- function(thermal = FALSE,
                         endOfHistory = 2025) {


  # PARAMETERS -----------------------------------------------------------------

  # Regions taken into account from WEO
  regWEO <- c("Africa", "Middle East")

  # Lower temporal boundary for historical data
  periodBegin <- 1990



  # READ-IN DATA ---------------------------------------------------------------

  ## Main Datasets ====

  # Shares

  # Odyssee
  sharesOdyssee <-
    calcOutput("ShareOdyssee", subtype = "enduse", aggregate = FALSE, warnNA = FALSE) %>%
    as_tibble(na.rm = TRUE)

  # IEA ETP
  sharesETP <-
    calcOutput("ShareETP", subtype = "enduse", feOnly = FALSE, aggregate = FALSE, warnNA = FALSE) %>%
    as_tibble(na.rm = TRUE)

  # IEA EEI
  sharesEEI <-
    calcOutput("ShareIEA_EEI", subtype = "enduse", aggregate = FALSE, warnNA = FALSE) %>%
    as_tibble(na.rm = TRUE)

  # IEA WEO
  sharesWEO <- calcOutput("ShareWEO", aggregate = FALSE) %>%
    as_tibble(na.rm = TRUE)

  # IEA China
  sharesChina <- calcOutput("ShareChina", aggregate = FALSE, warnNA = FALSE) %>%
    as_tibble(na.rm = TRUE)

  # IEA TCEP
  sharesTCEP <- calcOutput("ShareTCEP", aggregate = FALSE, warnNA = FALSE) %>%
    as_tibble(na.rm = TRUE)


  # Final Energy Weights

  # IEA Energy Balances
  fe <- calcOutput("IOEdgeBuildings",
                   subtype = "output_EDGE_buildings",
                   aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE)


  ## Mappings =====

  # EDGE-B regional mapping
  regmappingEDGE <- toolGetMapping(name  = "regionmappingEDGE.csv",
                                   type  = "regional",
                                   where = "mredgebuildings")

  # WEO regional mapping
  regmappingWEO <- toolGetMapping(name  = "regionmappingWEO.csv",
                                  type  = "regional",
                                  where = "mredgebuildings")

  # Percentage of Appliances for Refrigerators
  fridgeShare <- toolGetMapping(name  = "fridgeShare.csv",
                                type  = "sectoral",
                                where = "mredgebuildings")



  # FUNCTIONS ------------------------------------------------------------------

  .normalize <- function(df, shareOf) {
    df <- df %>%
      group_by(across(-all_of(c(shareOf, "value")))) %>%
      mutate(value = .data[["value"]] / sum(.data[["value"]], na.rm = TRUE)) %>%
      ungroup()
  }

  # Get growth factors from TCEP for specific regions
  .getGrowth <- function(df, regmap, regionAgg = "RegionCode") {
    regmap <- regmap %>%
      select(region = "CountryCode", regionAgg = !!regionAgg)

    df %>%
      left_join(regmap, by = "region") %>%
      # Filter out rows where regionAgg is NA
      filter(!is.na(.data[["regionAgg"]])) %>%
      select("region", "period", "enduse", "regionAgg", "value") %>%
      group_by(across(all_of(c("regionAgg", "enduse", "period")))) %>%
      reframe(value = sum(.data[["value"]], na.rm = TRUE)) %>%
      group_by(across(all_of(c("regionAgg", "enduse")))) %>%
      reframe(growth = (lead(.data[["value"]]) /
                          .data[["value"]])^(1 / c(diff(.data[["period"]]), NA))) %>%
      filter(!is.na(.data[["growth"]])) %>%
      left_join(regmap, by = "regionAgg", relationship = "many-to-many") %>%
      select(-"regionAgg")
  }


  # PROCESS DATA ---------------------------------------------------------------

  # Identify WEO regions and their ISO countries
  weoRegionsISO <- regmappingWEO %>%
    filter(.data$RegionCode %in% regWEO) %>%
    pull("CountryCode")

  # Calculate growth factors from TCEP for WEO regions only
  extGrowthFactors <- .getGrowth(sharesTCEP, regmappingWEO) %>%
    filter(.data$region %in% weoRegionsISO)



  # TEMPORARY FIX --------------------------------------------------------------

  # Correct data point in WEO dataset
  # The available WEO data gives 0.00% share of space_cooling in AFR on the global
  # FE demand. While this might be reasonable, IEA TCEP estimates the same share
  # for Africa & Middle East to approx. 12% in 2011. While this might be overestimated
  # for Africa due to sig. lower AC penetration, around 3-4% might be within a reasonable range.
  # Further, the 2016 IEA report "The Future of Cooling" states that most african countries
  # have a space cooling share of <5%.

  sharesWEO <- sharesWEO %>%
    mutate(coolShareAFR = 0.03) %>%
    left_join(regmappingWEO, by = c("region" = "CountryCode")) %>%
    mutate(value = case_when(
      (.data$RegionCode == "Africa" & .data$enduse == "space_cooling") ~ .data$coolShareAFR,
      (.data$RegionCode == "Africa" & .data$enduse != "space_cooling") ~ .data$value * (1 - .data$coolShareAFR),
      .default = .data$value
    )) %>%
    select(-"RegionCode", -"coolShareAFR")


  # ----------------------------------------------------------------------------



  # filter relevant regions from WEO
  sharesWEO <- sharesWEO %>%
    left_join(regmappingWEO, by = c("region" = "CountryCode")) %>%
    filter(.data$RegionCode %in% regWEO) %>%
    select(-"RegionCode")

  # fill zero values with NAs in IEA EEI
  sharesEEI <- sharesEEI %>%
    mutate(value = ifelse(.data$value == 0, NA, .data$value))


  # Combine multiple datasets
  # Note: Since the 2025 data point from IEA ETP is a projection and deviates in trend
  #       from other historical data points, we only include it if no second data point
  #       (next to IEA ETP 2014) can be included from a different data set.
  #       WEO and ETP data are only used if EEI provides fewer than 2 data points
  #       for a particular region-enduse combination. This preserves dataset
  #       purity when EEI has sufficient coverage. In the case where WEO data is used,
  #       the data points are not mixed with those of IEA ETP.

  shares <- sharesOdyssee %>%
    interpolate_missing_periods(period = seq.int(periodBegin, endOfHistory, 1)) %>%
    left_join(sharesETP %>%
                filter(.data$region != "CHN" | .data$enduse %in% c("water_heating")) %>%
                rename("valueETP" = "value"),
              by = c("region", "period", "enduse")) %>%
    left_join(sharesChina %>%
                rename("valueChina" = "value"),
              by = c("region", "period", "enduse")) %>%
    left_join(sharesEEI %>%
                rename("valueEEI" = "value"),
              by = c("region", "period", "enduse")) %>%
    left_join(sharesWEO %>%
                filter(.data$region != "CHN") %>%
                rename("valueWEO" = "value"),
              by = c("region", "period", "enduse")) %>%
    left_join(sharesTCEP %>%
                rename("valueTCEP" = "value"),
              by = c("region", "period", "enduse")) %>%

    # Find region-enduse pairs that have data in any non-ETP dataset
    group_by(across(all_of(c("region", "enduse")))) %>%
    mutate(
      hasOtherData = any(
        .data$period != 2014 & (
          (!is.na(.data$value) & .data$value != 0) |             # Odyssee data
            (!is.na(.data$valueChina) & .data$valueChina != 0) | # China data
            (!is.na(.data$valueWEO) & .data$valueWEO != 0) |     # WEO data
            (!is.na(.data$valueEEI) & .data$valueEEI != 0)       # EEI data
        )
      ),

      # Remove ETP 2025 data for pairs that have other data or are WEO regions
      valueETP = ifelse((.data$period == 2025 & .data$hasOtherData) |
                          (.data$region %in% weoRegionsISO), NA, .data$valueETP),

      # Count EEI data points for this region-enduse combination
      eeiCount = sum(!is.na(.data$valueEEI) & .data$valueEEI != 0)
    ) %>%
    ungroup() %>%

    # Apply priority rules with the WEO region condition
    mutate(value = case_when(
      !is.na(.data$value) & .data$value != 0                            ~ .data$value,      # 1. Odyssee
      !is.na(.data$valueEEI)                                            ~ .data$valueEEI,   # 2. IEA EEI
      !is.na(.data$valueChina)                                          ~ .data$valueChina, # 3. IEA China
      (!is.na(.data$valueWEO) & .data$eeiCount < 2)                     ~ .data$valueWEO,   # 4. IEA WEO
      (!is.na(.data$valueETP) & .data$eeiCount < 2)                     ~ .data$valueETP,   # 5. IEA ETP
      .default                                                          = .data$value       # Fallback to Odyssee
    )) %>%
    select("region", "period", "enduse", "value")


  # Extra-/Interpolate missing periods
  sharesFilled <- shares %>%
    .fillTimeseries(extGrowthFactors, periods = seq.int(periodBegin, endOfHistory, 1)) %>%
    .normalize("enduse")


  # Transform end-use "appliances" into "refrigerator" if specified
  if (isTRUE(thermal)) {
    sharesFilled <- sharesFilled %>%
      toolAddThermal(regmappingEDGE, fridgeShare, feOnly = FALSE, "enduse") %>%
      .normalize("enduse")
  }


  # Process final energy weights for regional aggregation
  weights <- fe %>%
    group_by(across(all_of(c("region", "period")))) %>%
    reframe(value = sum(.data$value, na.rm = TRUE)) %>%
    interpolate_missing_periods(period = seq.int(periodBegin, endOfHistory, 1), expand.values = TRUE) %>%
    right_join(sharesFilled %>%
                 rename("share" = "value"),
               by = c("region", "period")) %>%
    mutate(value = .data$value * .data$share) %>%
    select(-"share") %>%
    replace_na(list(value = 0))


  # OUTPUT ---------------------------------------------------------------------

  # Shares
  data <- sharesFilled %>%
    droplevels() %>%
    as.quitte() %>%
    as.magpie()

  # Final Energy Weights
  weights <- weights %>%
    droplevels() %>%
    as.quitte() %>%
    as.magpie()


  return(list(x = data,
              weight = weights,
              unit = "1",
              min = 0,
              max = 1,
              description = "End-use shares in buildings final energy demand",
              aggregationArguments = list(zeroWeight = "allow")))
}



#' Fill missing values in time series data
#'
#' Fills missing values in a time series dataset using interpolation for gaps between
#' known values and growth-based extrapolation for extending the series.
#' The function uses linear interpolation between known values and calculates
#' appropriate growth rates for extrapolation based on available data points.
#' For regions with 3+ data points, it fits a log-linear model, while for
#' regions with 2 data points, it uses a simple growth rate calculation.
#' For regions within the extGrowthFactors data frame, the growth rates are used.
#'
#' @param df A data frame containing region, enduse, period, and value columns
#' @param extGrowthFactors A data frame containing external growth factors
#' @param periods Vector of time periods to include in the result (default: 1990:2025)
#'
#' @return A complete data frame with filled values for all region-enduse-period combinations
#'
#' @importFrom dplyr filter group_by group_modify left_join ungroup %>%
#' @importFrom stats lm coef

.fillTimeseries <- function(df, extGrowthFactors = NULL, periods = 1990:2025) {   #nolint cyclocomp_linter

  # Create complete template with all possible combinations of region, enduse, and period
  template <- expand.grid(
    region = unique(df$region),
    enduse = unique(df$enduse),
    period = periods,
    stringsAsFactors = FALSE
  )

  # Join template with original data and process each region-enduse group
  result <- template %>%
    left_join(df, by = c("region", "enduse", "period")) %>%

    # Group by region and enduse to handle each time series separately
    group_by(across(all_of(c("region", "enduse")))) %>%
    group_modify(function(data, key) {
      regionName <- as.character(key$region)
      enduseName <- as.character(key$enduse)

      # Extract rows with known values for this region-enduse combination
      knownData <- data %>%
        filter(!is.na(.data$value))

      # Return unchanged if no known values exist for this combination
      if (nrow(knownData) == 0) return(data)

      # If all known values are zero, fill all missing values with zero
      if (all(knownData$value == 0)) {
        data$value[is.na(data$value)] <- 0
        return(data)
      }

      # Extract non-zero data points for growth rate calculation
      nonZero <- knownData %>%
        filter(.data$value > 0)

      # Default growth rate (no change)
      growthRate <- 1.0

      # Check if growth factor exists for this region/enduse in the provided data
      useExternalGrowth <- FALSE
      if (!is.null(extGrowthFactors)) {
        matchingGrowth <- extGrowthFactors[
          extGrowthFactors$region == regionName &
            extGrowthFactors$enduse == enduseName,
        ]

        if (nrow(matchingGrowth) > 0) {
          growthRate <- matchingGrowth$growth[1]
          useExternalGrowth <- TRUE

          # Validate growth rate and use default if invalid
          if (is.na(growthRate) || !is.finite(growthRate) || growthRate <= 0) {
            growthRate <- 1.0
            useExternalGrowth <- FALSE
          }
        }
      }

      # If no external growth rate is available, calculate from data
      if (!useExternalGrowth) {
        # Calculate appropriate growth rate based on available data points
        if (nrow(nonZero) >= 3) {
          # For 3+ data points: use log-linear regression model.
          # Default to growth rate of 1.0 if model fitting fails.
          try({
            fit <- lm(log(value) ~ period, data = nonZero)
            growthRate <- exp(coef(fit)[2])
          })

        } else if (nrow(nonZero) == 2) {
          # Calculate annual growth rate between the two points
          growthRate <- (nonZero$value[2] / nonZero$value[1]) ^ (1 / (nonZero$period[2] - nonZero$period[1]))
        } else {
          stop(paste("Not sufficient data points for trend fitting and no external growth factors given.",
                     " Region: ", regionName, " Enduse: ", enduseName))
        }

        # Validate growth rate and use default if invalid
        if (is.na(growthRate) || !is.finite(growthRate) || growthRate <= 0) growthRate <- 1.0
      }

      # Vectorized filling of missing values
      knownPeriods <- knownData$period

      # Fill missing values using interpolation or extrapolation
      data <- data %>%
        arrange(.data$period) %>%
        mutate(
          idx = findInterval(.data$period, knownPeriods),
          nearestEarlier = ifelse(.data$idx > 0, knownPeriods[.data$idx], NA),
          nearestLater = ifelse(.data$idx < length(knownPeriods), knownPeriods[.data$idx + 1], NA),

          # Get corresponding values for nearest periods
          earlierValue = ifelse(is.na(.data$nearestEarlier), NA,
                                knownData$value[match(.data$nearestEarlier, knownData$period)]),
          laterValue = ifelse(is.na(.data$nearestLater), NA,
                              knownData$value[match(.data$nearestLater, knownData$period)])
        ) %>%
        mutate(
          value = case_when(
            # Keep existing values
            !is.na(.data$value) ~ .data$value,

            # Linear interpolation between known values
            !is.na(.data$nearestEarlier) & !is.na(.data$nearestLater) ~ {
              ifelse(.data$earlierValue == 0 | .data$laterValue == 0,
                     0,
                     .data$earlierValue +
                       ((.data$period - .data$nearestEarlier) / (.data$nearestLater - .data$nearestEarlier)) *
                         (.data$laterValue - .data$earlierValue))
            },

            # Forward and backward extrapolation
            !is.na(.data$nearestEarlier) | !is.na(.data$nearestLater) ~ {
              baseValue <- ifelse(!is.na(.data$nearestEarlier), .data$earlierValue, .data$laterValue)
              refPeriod <- ifelse(!is.na(.data$nearestEarlier), .data$nearestEarlier, .data$nearestLater)
              ifelse(baseValue == 0,
                     0,
                     baseValue * (growthRate ^ (.data$period - refPeriod)))
            },

            # Default case
            .default = NA
          )
        ) %>%
        select(-"nearestEarlier", -"nearestLater", -"earlierValue", -"laterValue", -"idx")

      return(data)
    }) %>%
    ungroup()

  return(result)
}
