#' Read GEM Global Exposure Model
#'
#' Building-stock exposure figures from the GEM (Global Earthquake Model) global
#' exposure model.
#'
#' @return MAgPIE object
#'
#' @author Hagen Tockhorn
#'
#' @importFrom utils read.csv
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% bind_rows mutate .data
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

readGEM <- function() {

  # PARAMETERS -----------------------------------------------------------------

  # unit per raw data column
  unitMap <- c("BUILDINGS"                  = "number",
               "DWELLINGS"                  = "number",
               "OCCUPANTS_TOTAL"            = "persons",
               "TOTAL_AREA_SQM"             = "m2",
               "AVG_BUILDING_AREA_SQM"      = "m2",
               "AVG_BLDG_COST_PER_AREA_USD" = "USD/m2",
               "CARBON_BUILDINGS_TON"       = "tCO2",
               "BLDG_REPL_COST_USD"         = "USD",
               "COST_CONTENTS_USD"          = "USD",
               "COST_STRUCTURAL_USD"        = "USD",
               "COST_NONSTRUCTURAL_USD"     = "USD",
               "TOTAL_REPL_COST_USD"        = "USD")

  # columns that never carry a value
  dropCols <- c("NAME_0", "REGION")

  sourceYear <- 2025


  # FUNCTIONS ------------------------------------------------------------------


  # read one Exposure_Summary_Adm0.csv into long format
  getFileContent <- function(file) {
    data <- read.csv(file, stringsAsFactors = FALSE)

    # region and sector
    colnames(data)[colnames(data) == "ID_0"] <- "region"
    if ("OCCUPANCY" %in% colnames(data)) {
      colnames(data)[colnames(data) == "OCCUPANCY"] <- "sector"
    } else {
      data$sector <- "total"
    }

    data <- data[, !colnames(data) %in% dropCols, drop = FALSE]

    pivot_longer(data,
                 cols      = -c("region", "sector"),
                 names_to  = "variable",
                 values_to = "value")
  }


  # READ-IN DATA ---------------------------------------------------------------

  files <- list.files("global_exposure_model",
                      pattern = "Exposure_Summary_Adm0.csv",
                      recursive  = TRUE,
                      full.names = TRUE)


  # PROCESS DATA ---------------------------------------------------------------

  data <- files %>%
    lapply(getFileContent) %>%
    bind_rows()

  data %>%
    mutate(value  = as.numeric(.data$value),
           period = sourceYear,
           unit   = unname(unitMap[.data$variable])) %>%
    as.quitte() %>%
    as.magpie()
}
