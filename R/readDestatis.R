#' Read data from the Federal Statistical Office of Germany
#'
#' @param subtype character, data code
#'   \itemize{
#'    \item \code{"31121-0005"}: Construction work completed regarding new
#'                               buildings
#'   }
#' @returns MagPIE object
#'
#' @author Robin Hasse
#'
#' @importFrom utils read.csv tail
#' @importFrom dplyr %>% select rename mutate .data
#' @importFrom magclass as.magpie
#' @export

readDestatis <- function(subtype) {

  file <- switch(subtype,
    `31121-0005` = "31121-0005_flat.csv",
    stop("unknown subtype: ", subtype)
  )

  data <- read.csv2(file, fileEncoding = "ISO-8859-1")

  cols <- stats::setNames(
    grep("X\\d+_Auspraegung_Code", colnames(data), value = TRUE),
    data %>%
      select(grep("X\\d+_Merkmal_Code", colnames(data), value = TRUE)) %>%
      unique() %>%
      unname()
  )

  data %>%
    select(c(period = "Zeit", cols, value = tail(colnames(data), 1))) %>%
    rename(region = "DINSG") %>%
    mutate(value = ifelse(.data[["value"]] %in% c("-", "."),
                          NA,
                          .data[["value"]]),
           value = as.numeric(.data[["value"]])) %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value")
}
