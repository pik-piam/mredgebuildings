#' Read TCEP data base
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr %>% select mutate
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom readxl read_excel
#' @export


readTCEP <- function() {

  file <- "TCEP2014_figure_01_39_data.xlsx"

  data <- read_excel(file)

  data <- data %>%
    gather(key = "variable", value = "value", setdiff(colnames(data), c("region", "period"))) %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
