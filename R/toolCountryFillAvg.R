#' Tool: CountryFillAvg
#'
#' This function expects a MAgPIE object with ISO country codes in the spatial
#' dimension. It applies toolCountryFill but fills with the average across
#' regions.
#'
#' @author Robin Hasse
#'
#' @param x MAgPIE object with ISO country codes in the spatial dimension
#' @param ... arguments passed to toolCountryFill
#'
#' @importFrom madrat toolCountryFill
#' @importFrom magclass getNames getSets getSets<-
#' @export

toolCountryFillAvg <- function(x, ...) {

  # catch arguments and specify fill
  args <- c(as.list(environment()), list(...))
  args[["fill"]] <- NA

  # average across regions
  avg <- colMeans(x, na.rm = TRUE, dims = 1)

  # fill missing countries with NA
  x <- do.call(toolCountryFill, args)
  x <- x["dummy", , invert = TRUE]
  x <- x["GLO", , invert = TRUE]

  # blow up regions of avg to same size as x
  avg <- do.call(mbind, lapply(getItems(x, 1), setItems, x = avg, dim = 1))

  # fill NAs with average
  x[is.na(x)] <- avg[is.na(x)]

  # restore set names
  dx <- ndim(x)
  if (dx >= 3) {
    getSets(x)[3:dx] <- getSets(avg)[3:dx]
  }

  return(x)
}
