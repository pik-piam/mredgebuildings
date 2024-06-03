#' Get historic GDP per Capita for SSP Scenarios
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn


calcGDPPop <- function() {

  # LOAD DATA ------------------------------------------------------------------

  gdp <- calcOutput("GDPPast", aggregate = FALSE, average2020 = FALSE, file = "f_gdp.cs4r") %>%
    as.quitte()

  pop <- calcOutput("PopulationPast", aggregate = FALSE, file = "f_pop.cs4r") %>%
    as.quitte()


  # PROCESS DATA ---------------------------------------------------------------

  # Join and Calculate
  gdpPop <- gdp %>%
    select(-"unit", -"model", -"variable", -"scenario") %>%
    left_join(pop %>%
                select(-"unit", -"model", -"variable", -"scenario"),
              by = c("region", "period")) %>%
    mutate(value = .data[["value.x"]] / .data[["value.y"]],
           variable = "gdppop in constant 2005 Int$PPP") %>%
    select(-"value.x", -"value.y")



  # OUTPUT ---------------------------------------------------------------------

  gdpPop <- gdpPop %>%
    select("region", "period", "variable", "value") %>%
    as.magpie()

  pop <- pop %>%
    select("region", "period", "variable", "value") %>%
    as.magpie()

  return(list(x = gdpPop,
              weight = pop,
              min = 0,
              description = "GDP per capita",
              unit = "USD2005/cap"))
}
