#' Read data from MessageIX-Buildings
#'
#' @param subtype specifies data base category
#'
#' @importFrom openxlsx read.xlsx
#'
#' @export

readMessageIXBuildings <- function(subtype = "uvalue") {

  # READ AND PROCESS DATA ------------------------------------------------------

  if (subtype == "uvalue") {
    data <- read.csv("input_U_values.csv")

    data <- data %>%
      rename("region" = "name",
             "variable" = "arch",
             "value" = "u_val") %>%
      mutate(region = substr(.data$region, 1, 3))

  } else if (subtype == "floorByCohort") {
    data <- read.xlsx("Mastrucci2021_SI.xlsx",
                      sheet = "floor_by_cohort")

    data <- data %>%
      pivot_longer(cols = matches("\\d{4}"),
                   names_to = "period",
                   values_to = "value") %>%
      mutate(Variable = tolower(.data$Variable),
             Unit = "Mm2")

    colnames(data) <- tolower(colnames(data))
  }


  # OUTPUT ---------------------------------------------------------------------

  x <- data %>%
    as.quitte() %>%
    as.magpie()

  return(x)
}
