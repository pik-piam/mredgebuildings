#' Read heating systems sales in Spain from FEGECA
#'
#' @param subtype Character, report to read. if NULL, the most recent report is
#'   used for every data point.
#' @returns magpie object
#'
#' @author Robin Hasse
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% mutate filter select across all_of row_number arrange n
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte
#' @export

readFEGECA <- function(subtype = NULL) {

  # names of files must be orderable
  files <- c(`2019` = "2017_2018_2019.csv",
             `2020` = "2020.csv",
             `2021` = "2021.csv",
             `2022` = "2022.csv",
             `2023` = "2023.csv")

  data <- if (is.null(subtype)) {
    # consider all files and always use the most recent information
    do.call(rbind, lapply(names(files), function(fileName) {
      read.csv(files[[fileName]], encoding = "UTF-8") %>%
        mutate(file = fileName)
    })) %>%
      group_by(across(all_of(c("period", "variable", "unit")))) %>%
      arrange(.data[["file"]]) %>%
      filter(row_number() == n()) %>%
      ungroup() %>%
      select(-"file")
  } else if (subtype %in% names(files)) {
    # read only given report
    read.csv(files[[subtype]], encoding = "ANSI")
  } else {
    stop("Invalid subtype: '", subtype, "'. Available subtypes: NULL, ",
         paste(names(files), collapse = ", "))
  }

  # remove columns
  data[["comment"]] <- NULL
  data[["source"]] <- NULL

  as.quitte(data) %>%
    as.magpie() %>%
    collapseDim(1)
}
