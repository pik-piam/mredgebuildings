#' Read data from MessageIX-Buildings
#'
#' @param subtype specifies data base category
#'
#' @export

readMessageIXBuildings <- function(subtype = "uvalue") {

  # READ-IN DATA ---------------------------------------------------------------

  data <- switch(subtype,
                 "uvalue"        = read.csv("input_U_values.csv"),
                 "floorByCohort" = read.xlsx("Mastrucci2021_SI.xlsx",
                                             sheet = "floor_by_cohort"))


  # PROCESS DATA ---------------------------------------------------------------

  if (subtype == "uvalue") {
    data <- data %>%
      rename("region" = "name",
             "variable" = "arch",
             "value" = "u_val") %>%
      mutate(region = substr(.data$region, 1, 3))

  } else if (subtype == "floorByCohort") {
    data <- data %>%
      pivot_longer(cols = as.character(seq(2015, 2050, 5)),
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
