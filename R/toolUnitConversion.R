#' Convert unit
#'
#' Takes MAgPIE object with a variable dimension that has the unit as suffix
#' separated with '_' and converts the unit according to a unit conversion
#' table.
#'
#' @author Robin Hasse
#'
#' @param x MAgPIE object
#' @param unitConversion data.frame with the columns from, to, factor
#' @param dim variable dimension that contains the unit as suffix
#' @param removeUnit boolean remove unit after conversion for clean variable
#' names
#'
#' @importFrom magclass getItems getItems<- getSets dimReduce as.magpie
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate
#' @export

toolUnitConversion <- function(x, unitConversion, dim = 3.1,
                               removeUnit = FALSE) {
  # check input
  stopifnot(is.magpie(x) & is.data.frame(unitConversion))
  if (!all(c("from", "to", "factor") %in% colnames(unitConversion))) {
    stop("'unitConversion' has to be a data.frame with the columns ",
         "'from', 'to', and 'factor'.")
  }

  separateUnit <- "unit" %in% getSets(x)

  # all units in data
  units <- if (separateUnit) {
    getItems(x, "unit")
  } else {
    unique(gsub("^.*_", "", grep("_", getItems(x, dim), value = TRUE)))
  }
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
  dimName <- getSets(x)[[paste0("d", dim)]]
  x <- as.quitte(x)
  x <- x[!is.na(x[["value"]]), ]
  if (separateUnit) {
    x[["unit"]] <- as.character(x[["unit"]])
    x[["value"]] <- ifelse(is.na(x[["unit"]]),
                           x[["value"]],
                           x[["value"]] * uConv[x[["unit"]], "factor"])
    x[["unit"]] <- as.factor(ifelse(removeUnit | is.na(x[["unit"]]), "", uConv[x[["unit"]], "to"]))
  } else {
    x <- suppressWarnings(separate(x, dimName, c(dimName, "unit"), sep = "_"))
    x[["value"]] <- ifelse(is.na(x[["unit"]]),
                           x[["value"]],
                           x[["value"]] * uConv[x[["unit"]], "factor"])
    x[[dimName]] <- paste0(x[[dimName]],
                           ifelse(removeUnit | is.na(x[["unit"]]),
                                  "",
                                  paste0("_", uConv[x[["unit"]], "to"])))
    x["unit"] <- NULL
  }
  x <- dimReduce(as.magpie(x, spatial = "region"))

  return(x)
}
