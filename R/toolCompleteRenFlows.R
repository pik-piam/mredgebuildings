#' Complete Matrix of flows from old to new to cover given items
#'
#' Extrapolate from data on selected technologies to all technologies required
#'
#' We assume that there is a high status quo probability that people just do
#' identical replacement. If they don't, they chose the system with
#' probabilities proportional to the general shares.
#'
#' @param x MagPIE object, renovation flows to complete
#' @param from MagPIE object with origin quantities
#' @param to MagPIE object with destination quantities
#' @returns MagPIE object
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getNames<- getNames getSets<- getSets dimSums
#' @importFrom dplyr %>%
#' @export

toolCompleteRenFlows <- function(x, from, to) {

  # FUNCTIONS ------------------------------------------------------------------

  .proportions <- function(x) {
    ifelse(x == 0, 0, proportions(x))
  }


  isIdent <- function(x) {
    grepl("(.+)\\.\\1", getNames(x))
  }


  getStatusQuoShare <- function(x, identRepl) {
    statusQuo <- (identRepl / dimSums(x, 3.1) - .proportions(dimSums(x, 3.1))) /
      (1 - .proportions(dimSums(x, 3.1)))
    sum(statusQuo * .proportions(dimSums(x, 3.1)))
  }


  getIdentReplaceQuantities <- function(x) {
    identRepl <- x[, , isIdent(x)]
    getNames(identRepl) <- sub("\\..+$", "", getNames(identRepl))
    getSets(identRepl) <- getSets(identRepl)[c(1, 2, 4)]
    return(identRepl)
  }



  # PREPARE --------------------------------------------------------------------

  # normalise
  from <- .proportions(from)
  to <- .proportions(to)

  identRepl <- getIdentReplaceQuantities(x)
  statusQuo <- getStatusQuoShare(x, identRepl)

  # rescale matrix down to the fraction that is represented
  x <- x *
    sum(from[, , getItems(x, 3.2)])



  # CALCULATE ------------------------------------------------------------------

  # blown-up matrix from sales shares
  xFull <- do.call(mbind, lapply(getItems(from, 3), function(hs) {
    add_dimension(to, add = "old", nm = hs)
  }))

  # apply status quo factor
  xFull <- xFull * (1 - statusQuo) +
    ifelse(isIdent(xFull), statusQuo, 0)

  # renormalise
  xFull <- xFull * from

  # overwrite values that exist in EHI data
  xFull[, , getNames(x)] <- ifelse(x == 0,
                                   0.1 * xFull[, , getNames(x)],
                                   x)

  # normalise
  xFull <- .proportions(xFull)

  return(xFull)
}
