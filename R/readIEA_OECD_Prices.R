#' read energy carrier prices
#'
#' @param subtype character, data set
#'
#' @author Robin Hasse
readIEA_OECD_Prices <- function(subtype) { # nolint: object_name_linter.

  # FUNCTIONS ------------------------------------------------------------------

  .dropSubAnnualPeriods <- function(x) {
    x[grepl("^\\d{4}$", x$period), ]
  }

  .nameCols <- function(x) {
    names(x)[1] <- "region"
    names(x)[ncol(x)] <- "value"
    x[x$value == "..", "value"] <- NA
    x$value <- as.numeric(x$value)
    periodCol <- which(apply(x, 2, function(vec) all(grepl("\\d{4}$", vec))))
    if (length(periodCol) == 1) {
      names(x)[periodCol] <- "period"
      x <- .dropSubAnnualPeriods(x)
      x$period <- as.numeric(x$period)
    } else if (length(periodCol) > 1) {
      stop("Found more than one period column.")
    }
    x
  }

  .reorderCols <- function(x) {
    first <- intersect(names(x), c("region", "period"))
    last  <- intersect(names(x), "value")
    middle <- setdiff(names(x), c(first, last))
    x[, c(first, middle, last)]
  }



  # READ -----------------------------------------------------------------------

  folder <- file.path("OECD Energy Prices and Taxes quarterly 3Q2022", subtype)
  file <- list.files(folder, pattern = "\\.TXT$")
  x <- data.table::fread(file.path(folder, file), data.table = FALSE)



  # OUTPUT ---------------------------------------------------------------------

  x <- .nameCols(x)
  x <- .reorderCols(x)
  magclass::as.magpie(x, spatial = "region", tidy = TRUE)
}
