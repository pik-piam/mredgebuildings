#' Convert unit
#'
#' Takes MAgPIE object with a variable dimension and a unit dimension,
#' and converts the unit according to a unit conversion table.
#'
#' @author Robin Hasse
#'
#' @param x MAgPIE object
#' @param unitConversion data.frame with the columns from, to, factor
#' @param removeUnit boolean remove unit after conversion for clean variable
#' names
#'
#' @importFrom magclass getItems getItems<- getSets dimReduce as.magpie
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate
#' @export

toolUnitConversion <- function(x, unitConversion, removeUnit = FALSE) {

  # check input
  stopifnot(is.magpie(x) & is.data.frame(unitConversion))
  if (!all(c("from", "to", "factor") %in% colnames(unitConversion))) {
    stop("'unitConversion' has to be a data.frame with the columns ",
         "'from', 'to', and 'factor'.")
  }

  # all units in data
  units <- getItems(x, "unit")
  if (!all(units %in% unitConversion$from)) {
    stop("There is no conversion defined for the following units: ",
         paste(setdiff(units, unitConversion$from)), collapse = ", ")
  } else if (length(units) == 0) {
    stop("No units were found: ",
         "No unit dimension exists and units are not contained in the variable dimension as <variable>_<unit>.")
  }

  # prepare look up table
  uConv <- as.data.frame(unitConversion[, which(colnames(unitConversion) != "from")])
  rownames(uConv) <- unitConversion[["from"]]

  # conversion
  x <- as.quitte(x)
  x <- x[!is.na(x[["value"]]), ]
  x[["unit"]] <- as.character(x[["unit"]])
  x[["value"]] <- ifelse(is.na(x[["unit"]]),
                         x[["value"]],
                         x[["value"]] * uConv[x[["unit"]], "factor"])
  x[["unit"]] <- as.factor(ifelse(removeUnit | is.na(x[["unit"]]), "", uConv[x[["unit"]], "to"]))
  x <- dimReduce(as.magpie(x, spatial = "region"))

  return(x)
}
