#' calcSurface
#'
#' Calculates country surface
#'
#' @return Surface by country
#'
#' @author Antoine Levesque
#'
#' @importFrom madrat readSource
#' @export

calcSurface <- function() {
  data <- readSource("WDI", subtype = "AG.SRF.TOTL.K2")[, 2010, ]
  return(list(x = data,
              weight = NULL,
              unit = "km2",
              description = "Surface by country"))
}
