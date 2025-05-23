#' Discount rate
#'
#' Discount rate including implicit discounting
#'
#' @source https://doi.org/10.1016/j.eneco.2011.07.010
#'
#' @param granularity character, name of BRICK granularity
#'
#' @author Robin Hasse
#'
#' @importFrom magclass new.magpie getSets<- collapseDim mselect dimSums

calcDiscountRate <- function(granularity = NULL) {

  # CREATE EMPTY OBJECT --------------------------------------------------------

  x <- new.magpie(names = c("SFH", "MFH", "Com"))
  getSets(x)[3] <- "typ"



  # FILL VALUES ----------------------------------------------------------------

  # Giraudet et al. 2012
  # average of tenants (higher) and owners (lower)
  x[, , "SFH"] <- 0.13 # mostly owners
  x[, , "MFH"] <- 0.32 # mostly tenants

  # TODO: research a proper value. This is just a first assumption # nolint: todo_comment_linter
  x[, , "Com"] <- 0.08



  # RETURN ---------------------------------------------------------------------

  x <- collapseDim(x)
  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    mselect(period = "y2022", collapseNames = TRUE) %>%
    dimSums(c(1, 2))

  # aggregate to BRICK granularity
  agg <- toolAggregateBrick(x, granularity, feBuildings)


  return(list(x = agg$x,
              min = 0,
              isocountries = FALSE,
              unit = "1/yr",
              description = "discount rate incl. implicit discounting"))
}
