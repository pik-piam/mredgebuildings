#' Get historic GDP per Capita for SSP Scenarios
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn


calcGDPPop <- function() {

  # LOAD DATA ------------------------------------------------------------------

  gdp <- calcOutput("GDP", aggregate = FALSE, average2020 = FALSE) %>%
    mselect(variable = "gdp_SSP2") %>%
    as.quitte()

  pop <- calcOutput("PopulationPast", aggregate = FALSE) %>%
    as.quitte()


  # PROCESS DATA ---------------------------------------------------------------

  # Join and Calculate
  gdpPop <- gdp %>%
    select(-"unit", -"model", -"variable", -"scenario") %>%
    inner_join(pop %>%
                 select(-"unit", -"model", -"variable", -"scenario"),
               by = c("region", "period")) %>%
    mutate(value = .data[["value.x"]] / .data[["value.y"]],
           variable = "gdppop in constant 2017 Int$PPP") %>%
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
