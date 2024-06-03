#' Read Data from Primary Fuel and Useful Energy Data Base (PFUDB)
#'
#' This was adapted from EDGE function 'getPFUDB.R'.
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% rename
#' @importFrom rlang .data
#' @importFrom tidyr unite
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export


readPFUDB <- function() {

  file <- "pfudb_antoine_corr_TJ.csv"

  # Read Data
  data <- read.csv(file)

  # Make columns lowercase
  colnames(data) <- tolower(colnames(data))

  data <- data %>%
    rename(period = "year", region = "country") %>%
    gather("unit", "value", "fe", "ue") %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
