#' Read in energy efficiency values from ETSAP
#'
#' @export
readETSAP <- function() {

  data <- read.csv2("IEA_ETSAP_Table2.csv", sep = ";", stringsAsFactors = FALSE)

  revalueVec <- c(
    "Roof.U.value..W.m2K." = "roof",
    "Wall.U.value..W.m2K." = "wall",
    "Window.U.value..W.m2K." = "window"
  )

  revalueRegion <- c(
    "Beijing" = "CHN",
    "EU" = "EUR",
    "Shanghai" = "CHN",
    "USA" = "USA"
  )

  regionSelection <- c("Shanghai", "Beijing", "USA", "EU")

  uvalue <- data %>%
    select("region" = "Region", names(revalueVec)) %>%
    gather("variable", "value", names(revalueVec)) %>%
    revalue.levels(variable = revalueVec) %>%
    separate(col = "value", into = c("value1", "value2"), sep = "-", fill = "right") %>%
    mutate(value2 = ifelse(is.na(.data$value2), .data$value1, .data$value2),
           value1 = as.double(.data$value1),
           value2 = as.double(.data$value2),
           value = 0.5 * (.data$value1 + .data$value2)) %>%
    select(-"value1", -"value2") %>%
    calc_addVariable("uvalue" = "window * 0.25 + roof * 0.375 + wall * 0.375", only.new = TRUE)  %>%
    # !!! ETSAP estimates for DEU and SWE are approximately half as high as the data for European Regions only
    # !!! We therefore multiply the value by 2 to get comparable estimates
    mutate(value = .data$value * 2) %>%
    filter(.data$region %in% regionSelection) %>%
    revalue.levels(region = revalueRegion) %>%
    group_by(across(all_of("region"))) %>%
    reframe(uvalue = mean(.data$value))

  x <- as.magpie(uvalue, spatial = 1, temporal = NA)

  return(x)
}
