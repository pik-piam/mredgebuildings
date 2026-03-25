#' Exchange rate between EUR and USD
#'
#' Convert EUR in given year to internal unit
#'
#' @param x value in EUR
#' @param year integer, reference year
#' @param iso3c region
#' @param ... further arguments to [GDPuc::convertSingle]
#' @returns value in constant 2017 US$MER
#'
#' @author Robin Hasse

eur2usd <- function(x = 1, year = 2020, iso3c = "DEU", ...) {
  iso3c <- as.character(iso3c)
  GDPuc::convertSingle(x = x,
                       iso3c = iso3c,
                       year = year,
                       unit_in = paste("constant", year, "EUR"),
                       unit_out = "constant 2017 US$MER",
                       ...)
}
