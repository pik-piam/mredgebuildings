#' Read AC ownership rates per household from IEA Sources
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr %>% .data all_of mutate rename select
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte
#' @importFrom tidyr pivot_longer

readIEA_acOwnership <- function() { # nolint: object_name_linter

  data <- read.csv2("iea_ac-ownership-rates.csv", check.names = FALSE)

  colnames(data) <- tolower(colnames(data))

  data <- data %>%
    select(-"source") %>%
    rename("period" = "year") %>%
    pivot_longer(cols = -all_of(c("region", "period")),
                 names_to = "variable",
                 values_to = "value") %>%
    mutate(value = .data$value / 100)

  data <- data %>%
    as.quitte %>%
    as.magpie()

  return(data)

}
