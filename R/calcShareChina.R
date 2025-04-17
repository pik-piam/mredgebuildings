#' Calculate shares from final energy end-use demand for China
#'
#' The data is obtained from the 2021 IEA report "An Energy Sector Roadmap to Carbon
#' Neutrality in China" from Figure 3.25
#'
#' @author Robin Hasse
#'
#' @importFrom madrat readSource toolCountryFill
#' @importFrom magclass dimSums

calcShareChina <- function() {
  x <- readSource("IEA_China")
  x <- x[, , "other", invert = TRUE]
  shares <- x / dimSums(x, "enduse")
  shares <- toolCountryFill(shares, verbosity = 2)
  w <- toolCountryFill(x, 1, verbosity = 2)
  list(x = shares,
       weight = w,
       min = 0,
       max = 1,
       unit = "1",
       description = "Share of end uses in Chinese buildings final energy demand")
}
