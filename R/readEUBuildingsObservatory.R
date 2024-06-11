#' Read in energy efficiency values of building shell in residential and
#' non-residential buildings by the EU Buildings Observatory (2017)
#'
#' @export
readEUBuildingsObservatory <- function() {

  res <- read.csv2(file.path("2017", "Uvalues_Res_export-eu-buildings-20170503115017.csv"),
                   na.strings = "-", sep = ",", comment.char = "#", stringsAsFactors = FALSE) %>%
    mutate("variable" = "uvalue_res")

  com <- read.csv2(file.path("2017", "Uvalues_NonRes_export-eu-buildings-20170503115017.csv"),
                   na.strings = "-", sep = ",", comment.char = "#", stringsAsFactors = FALSE) %>%
    mutate("variable" = "uvalue_com")

  data <- rbind(res, com)
  periodCols <- grep("X\\d{4}", colnames(data), value = TRUE)

  data <- data %>%
    select(-c("ISO.code", "Unit", "Source", "Long.source", "Link", "Data.quality", "Comment")) %>%
    gather("period", "value", all_of(periodCols)) %>%
    mutate(
      "period" = as.integer(gsub("X", "", .data$period)),
      "value" = suppressWarnings(as.numeric(.data$value))
    ) %>%
    na.omit()

  x <- as.magpie(data, spatial = 1, temporal = 3)

  return(x)
}
