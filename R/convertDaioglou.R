#' convertDaioglou
#'
#' Tidy data from Daioglou et al. 2012. Currently, only the specific floor
#' space (m2/cap) is available from household data file. UN sources are dropped,
#' if there is data from other sources for the same region. For the remaining
#' data, if there is data from multiple UN Source for a region and period (i.e.
#' different cities), the average of those sources is considered. This neglects
#' the differences in population size of the cities. Selected data points are
#' removed additionally.
#'
#' @param subtype <dataFile>.<variable>
#' @param x MAgPIE object with data from Daioglou et al. #nolint
#'
#' @return clean MAgPIE object with unique data points
#'
#' @author Robin Hasse, Antoine Levesque
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter mutate group_by summarise across all_of select %>%
#'   .data
#' @importFrom madrat toolCountry2isocode toolCountryFill
#' @importFrom magclass as.magpie
#' @export

convertDaioglou <- function(x, subtype = "households.specific floor space") {

  data <- x %>%
    as.quitte() %>%
    filter(!(is.na(.data[["value"]]))) %>%
    mutate(region1 = gsub("Czech", "Czech Republic", .data[["region1"]]),
           region1 = gsub("Korea, south", "Korea", .data[["region1"]]),
           region = .data[["region1"]],
           region = toolCountry2isocode(.data[["region"]]))

  if (subtype == "households.specific floor space") {

    # remove data from UN if there are other sources for this region
    sourceUN <- unique(grep("UN habitat", data[["source"]], value = TRUE))
    countryUN <- data %>%
      filter(.data[["source"]] %in% sourceUN) %>%
      getElement("region") %>%
      unique()
    countryUNplus <- data %>%
      filter(.data[["region"]] %in% countryUN,
             !.data[["source"]] %in% sourceUN) %>%
      getElement("region") %>%
      unique()
    data <- data %>%
      filter(!(.data[["region"]] %in% countryUNplus &
                 .data[["source"]] %in% sourceUN))

    # take mean if there are multiple UN sources
    data <- data %>%
      filter(.data[["source"]] %in% sourceUN) %>%
      mutate(source = as.character(.data[["source"]])) %>%
      group_by(across(all_of(
        c("region", "period", "demographic", "quintile")
      ))) %>%
      summarise(value = mean(.data[["value"]]),
                source = ifelse(length(.data[["source"]]) > 1,
                                "UN habitat (mean)",
                                .data[["source"]]),
                .groups = "drop") %>%
      rbind(data %>%
              filter(!.data[["source"]] %in% sourceUN) %>%
              select("region", "period", "demographic", "quintile", "value",
                     "source"))

    # remove specific data points
    data <- filter(data,
      !paste(.data[["region"]], .data[["demographic"]], .data[["period"]],
             .data[["source"]], sep = "-") %in%
        c("CHN-Urban-2007-National Bureau of Statistics of China",
          "CHN-Rural-2007-National Bureau of Statistics of China, GINI from WIIDC2",
          "CHN-Rural-2004-National Bureau of Statistics of China, GINI from WIIDC2"),
      !(.data[["region"]] == "USA" &
          .data[["source"]] != "120 Years of US Residential Housing Stock and Floor Space"),
      .data[["region"]] != "ATG",
      !(.data[["region"]] == "CAN" & .data[["period"]] == 1990 &
          .data[["source"]] == "Natural Resrouces Canada, WDI"),
      !(.data[["region"]] == "GBR" & .data[["period"]] == 1998 &
          grepl("Department for Energy and Climate Change", .data[["source"]]))
    )

    # take mean of remaining duplicates (GBR, 1998)
    data <- data %>%
      group_by(across(all_of(
        c("region", "period", "demographic", "quintile")
      ))) %>%
      summarise(value = mean(.data[["value"]]), .groups = "drop")

  } else if (grepl("^share_", subtype)) {
    # explicit zero shares and unit conversion
    data <- data %>%
      group_by(across(all_of(c("region", "period", "demographic", "quintile")))) %>%
      complete(.data[["variable"]]) %>%
      mutate(value = replace_na(.data[["value"]], 0) / 100)
  } else {
    stop("This subtype is not available.")
  }

  # convert to MAgPIE object
  data <- data  %>%
    as.magpie() %>%
    toolCountryFill(verbosity = 2)

  return(data)
}
