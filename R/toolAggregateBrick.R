#' Aggregate BRICK input data to lower granularity
#'
#' @param x MagPIE object at BRICK default resolution
#' @param granularity character, name of BRICK granularity
#' @param weight magPIE object with weights, if \code{NULL} (default), an
#'   extensive variable is assumed and values are summed instead of averaged
#' @returns named list with aggregated value and weights
#'
#' @author Robin Hasse
#'

toolAggregateBrick <- function(x, granularity = NULL, weight = NULL) {

  # FUNCTIONS ------------------------------------------------------------------

  # select elements in given dimension
  .mselect <- function(m, dim, rel) {
    args <- c(list(m), magclass::setNames(list(rel$dim), dim))
    do.call(magclass::mselect, args)
  }


  .aggregate <- function(m, maps, dim, w) {
    if (is.null(m)) {
      return(NULL)
    }

    rel <- maps[[dim]]
    m <- .mselect(m, dim, rel)

    if (identical(rel$dim, rel$dimAgg)) {
      return(m)
    }

    if (!is.null(w)) {
      w <- if (dim %in% magclass::getSets(w)) {
        .mselect(w, dim, rel)
      } else {
        warning("Dimension ", dim, " is missing in weight. Using unweighted average.")
        1
      }
    }

    toolAggregate(m, rel = rel, weight = w,
                  from = "dim", to = "dimAgg",
                  dim = dim, partrel = TRUE)
  }

  .expandWeight <- function(weight, x, dims2agg) {
    if (is.null(weight)) return(NULL)
    missingDims <- setdiff(dims2agg, magclass::getSets(weight))
    if (length(missingDims) > 0) {
      warning(sprintf("Weight is missing the dimensions %s so unweighted average is assumed.",
                      paste(missingDims, collapse = ", ")))
    }
    for (dim in missingDims) {
      weight <- do.call(mbind, lapply(getItems(x, dim), function(nm) {
        magclass::add_dimension(weight, add = dim, nm = nm)
      }))
    }
    weight
  }



  # AGGREGATE ------------------------------------------------------------------

  if (!is.null(granularity)) {

    maps <- toolGetBrickMapping(granularity)
    dims2agg <- intersect(magclass::getSets(x), names(maps))
    weight <- .expandWeight(weight, x, dims2agg)

    for (dim in dims2agg) {
      x <- .aggregate(x, maps, dim, weight)
      weight <- .aggregate(weight, maps, dim, NULL)
    }
  }



  # RETURN ---------------------------------------------------------------------

  return(list(x = x, weight = weight))

}
