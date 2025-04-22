#' Read national floor space from EEA
#'
#' Floor space for many European countries between 1990 and 2009 in thousand m2.
#'
#' @return magpie object with floor space
#'
#' @author Robin Hasse
#'
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{ a <- readSource(type = "EEAfloorspace")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom tidyr gather
#' @importFrom dplyr %>% select mutate .data
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie collapseDim
#' @export

readEEAfloorspace <- function() {

  data <- read.csv("EEA_FloorspaceEuropeCON015_indicator_16.1_SCP022.csv",
                   na.strings = "n.a.") %>%
    select(-"X") %>%
    rename(region = "Total.floor.area") %>%
    gather("period", "value", -"region") %>%
    mutate(period = as.integer(gsub("X", "", .data[["period"]])),
           value = as.numeric(.data[["value"]])) %>%
    as.quitte() %>%
    as.magpie() %>%
    collapseDim()

  return(data)
}
