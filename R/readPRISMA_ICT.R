#' Read PRISMA ICT Data
#'
#' Reads ICT and data center electricity demand data from PRISMA project Excel file.
#' Supports different regional resolutions and data center counts.
#'
#' @param subtype Character string specifying data type: "R12" for 12-region ICT data,
#'   "R5" for 5-region data center data, or "nDC" for data center counts
#'
#' @returns A magpie object containing the requested ICT/data center data
#'
#' @author Hagen Tockhorn
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom dplyr mutate rename select %>% all_of
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#'
readPRISMA_ICT <- function(subtype) { # nolint: object_name_linter

  subtype <- switch(subtype,
                    "R12" = "Output_R12",
                    "R5"  = "R5 DC",
                    "nDC" = "Num. DC",
                    subtype)

  fileName <- "R12_Clean IAM  Version_Finalised.xlsx"

  if (subtype %in% c("Output_R12", "R5 DC")) {
    # Read and process energy data sheets
    data <- read.xlsx(fileName, sheet = subtype, check.names = FALSE, sep.names = " ") %>%
      (\(df) setNames(df, gsub(" \\(twh\\)", "", tolower(names(df)))))() %>%
      mutate(unit = "TWh") %>%
      rename("period" = "year") %>%
      pivot_longer(cols = -all_of(c("region", "period", "scenario", "unit")),
                   names_to = "variable", values_to = "value")

    if (subtype == "Output_R12") {
      data$region <- sub("R12_", "", data$region)
    }

  } else if (subtype == "Num. DC") {
    # Read data center counts sheet (skip metadata rows)
    data <- read.xlsx(fileName, sheet = subtype, check.names = FALSE,
                      sep.names = " ", startRow = 3) %>%
      (\(df) {
        # Process paired columns: odd = country, even = count
        pairList <- lapply(seq(1, ncol(df) - 1, by = 2), function(i) {
          dfPair <- df[, c(i, i + 1)]
          names(dfPair) <- c("region", "value")
          dfPair <- dfPair[!is.na(dfPair$region) & !is.na(dfPair$value), ]
          if (nrow(dfPair) > 0) dfPair[-nrow(dfPair), ] else dfPair  # Remove aggregate row
        })
        do.call(rbind, pairList)
      })() %>%
      mutate(variable = "number of DCs",
             period = 2024) %>%
      unique() %>%
      select("region", "period", "variable", "value")
  }

  data <- data %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
