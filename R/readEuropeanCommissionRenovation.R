#' EU report on historic renovation
#'
#' This function reads data on renovation from a pdf file of an EC report from
#' 2019 (Tables 2 - 13). There is no period dimension. The data represents
#' averages between 2012 and 2016.
#'
#' @param subtype character, source to read from: `"csv"` (default) or `"pdf"`
#'   (requires the suggested package pdftools)
#' @return MAgPIE object with renovation data
#'
#' @author Robin Hasse
#'
#' @source https://op.europa.eu/en/publication-detail/-/publication/97d6a4ca-5847-11ea-8b81-01aa75ed71a1
#'
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{ a <- readSource(type = "EuropeanCommissionRenovation")
#' }
#'
#' @importFrom tidyr gather
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom utils read.csv write.csv
#' @export

readEuropeanCommissionRenovation <- function(subtype = "csv") {

  file <- "MJ0319963ENN.en"

  if (subtype == "pdf") {

    # this subtype only works with pdftools
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      stop("pdftools is required but not available. Try using this function ",
           'with subtype = "csv".')
    }

    # Read table from pdf text
    readTable <- function(pdf, page, lines, cols, variable, unit, subsector) {

      # remove text outside the table
      table <- strsplit(pdf[page], "\n")[[1]][lines]
      table <- suppressWarnings(do.call("rbind", lapply(table, function(row) {
        strsplit(gsub("^ *| *$|,", "", row), "\\s{2,}")[[1]]
      })))

      # clean two-line rows
      nameLine <- apply(table, 1, function(line) length(unique(line)) == 1)
      dataLine <- unlist(lapply(seq_along(nameLine), function(i) {
        if (i == 1 | i == length(nameLine)) return(FALSE)
        identical(nameLine[(i - 1):(i + 1)], c(TRUE, FALSE, TRUE))
      }))
      if (sum(nameLine) != 2 * sum(dataLine)) stop("Check your line range!")
      table[dataLine, -1] <- table[dataLine, 1:(ncol(table) - 1)]
      table[dataLine, 1] <- unlist(lapply(which(dataLine), function(i) {
        paste(table[c(i - 1, i + 1), 1], collapse = " ")
      }))
      table <- table[!nameLine, ]

      # find percentages
      percent <- unlist(apply(table, c(1, 2), grepl, pattern = "%"))
      table[percent] <- unlist(lapply(table[percent], sub,
                                      pattern = "%", replacement = ""))
      # make numeric
      table <- data.frame(table)
      table[, -1] <- apply(table[, -1], c(1, 2),
                           function(x) as.numeric(sub("-", NA, x)))

      # convert percentages to absolute
      table[, -1] <- table[, -1] * ifelse(percent[, -1], 0.01, 1)

      # add columns and long format
      colnames(table) <- c("region", cols)
      table <- gather(table, "renovation", "value", -"region")
      table[["variable"]] <- variable
      table[["unit"]] <- unit
      table[["subsector"]] <- subsector

      return(table)
    }


    # Read Table across 2 pages
    readTables <- function(pdf, page1, page2, lines1, lines2,
                           cols, variable, unit, subsector) {
      rbind(
        readTable(pdf, page1, lines1, cols, variable, unit, subsector),
        readTable(pdf, page2, lines2, cols, variable, unit, subsector)
      )
    }


    # load pdf file
    pdf <- pdftools::pdf_text(paste0(file, ".pdf"))

    # read data from tables
    cols <- c("All renovations",
              "Energy related - Total",
              "Energy related - below Threshold",
              "Energy related - Light",
              "Energy related - Medium",
              "Energy related - Deep",
              "Non-energy related - Total")
    data <- rbind(
      readTables(pdf, 22, 23, 37:43, 1:24, cols[c(-1, -7)], "renovation rate",     "1/yr",           "residential"),
      readTable(pdf,  24,     13:43,       cols[c(-1, -7)], "renovation rate",     "1/yr",           "commercial"),
      readTable(pdf,  28,     11:39,       cols[c(-1, -7)], "relative PE savings", "1",              "residential"),
      readTables(pdf, 29, 30, 31:50, 1:13, cols[c(-1, -7)], "specific PE savings", "kWh/yr.m2",      "residential"),
      readTables(pdf, 30, 31, 31:44, 1:17, cols[c(-1, -7)], "PE savings",          "TOE/yr",         "residential"),
      readTable(pdf,  32,     11:39,       cols[c(-1, -7)], "relative PE savings", "1",              "commercial"),
      readTables(pdf, 33, 34, 33:48, 1:15, cols[c(-1, -7)], "specific PE savings", "kWh/yr.m2",      "commercial"),
      readTables(pdf, 34, 35, 36:47, 1:19, cols[c(-1, -7)], "PE savings",          "TOE/yr",         "commercial"),
      readTables(pdf, 36, 37, 35:40, 1:28, cols,            "investment",          "million EUR/yr", "residential"),
      readTables(pdf, 37, 38, 47:55, 1:22, cols[-1],        "specific investment", "EUR/yr.m2",      "residential"),
      readTables(pdf, 39, 40, 23:38, 1:13, cols,            "investment",          "million EUR/yr", "commercial"),
      readTables(pdf, 40, 41, 32:52, 1:10, cols[-1],        "specific investment", "EUR/yr.m2",      "commercial")
    )

    write.csv(data, paste0(file, ".csv"), row.names = FALSE)

  } else if (subtype == "csv") {
    fileCsv <- paste0(file, ".csv")
    if (file.exists(fileCsv)) {
      data <- read.csv(fileCsv)
    } else {
      stop("Can't find ", fileCsv, " in the source folder. Try running this ",
           'function with subtype = "pdf". This will create the csv for the ',
           "next time.")
    }
  } else {
    stop("Unknown subtype: '", subtype,
         "'. It has to be either 'csv' (default) or 'pdf' (requires) pdftools.")
  }

  # convert to magpie object
  data <- data %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
