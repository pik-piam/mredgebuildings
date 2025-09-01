#' Read Odyssee data base on households and services
#'
#' Copied from website: The Odyssee database contains detailed energy
#' consumption by end-use and their drivers as well as energy efficiency and
#' CO2-related indicators. Latest available data is provided by national
#' representatives, such as energy agencies or statistical organization, from
#' all EU countries as well as Norway, Serbia, Switzerland and the United
#' Kingdom.
#'
#' @source https://odyssee.enerdata.net/database/
#' @note To download new data, log into the website, select all items of all
#'   levels and download the data as
#'   \code{CSV (1 observation = 1 row, delimeter: ",")}. If this exceeds the
#'   download limit, download multiple files, e.g. by splitting along periods.
#' @note Variables are labels with the item code but full names can be found in
#' the source data
#'
#' @param subtype dataset version
#'
#' @returns magpie object
#'
#' @author Pascal Führlich, Robin Hasse
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% select mutate .data
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

readOdyssee <- function(subtype = "250109") {

  files <- list("250109" = c("Enerdata_Odyssee_250109_150210.csv",  # households: 2010 - 2023
                             "Enerdata_Odyssee_250109_145827.csv",  # households: 1990 - 2009
                             "Enerdata_Odyssee_250109_150428.csv"), # services
                "220405" = c("export_enerdata_9259_031531.csv",     # households
                             "export_enerdata_9259_031431.csv"))    # services

  skipRows <- if (subtype == "220405") 1 else 0

  files[[subtype]] %>%
    lapply(read.csv, na.strings = c("n.a.", ""), skip = skipRows) %>%
    do.call(what = rbind) %>%
    select(region = if (subtype == "220405") "ISO.code" else "ISO.Code",
           period = "Year",
           variable = if (subtype == "220405") "Item.code" else "Item.Code",
           value = "Value",
           unit = "Unit") %>%
    mutate(value = as.numeric(.data[["value"]])) %>%
    filter(!is.na(.data[["value"]])) %>%
    as.quitte() %>%
    as.magpie()
}
