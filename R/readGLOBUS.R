#' Read GLOBUS building stock
#'
#' Building stock projections for residential, non-residential and total building stock in
#' million m2 under the NR (no renovation) scenario from the GLOBUS building turnover model.
#'
#' @references https://doi.org/10.1016/j.ynexs.2024.100019
#'
#' @return MAgPIE object containing building stock data
#'
#' @author Hagen Tockhorn
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom zoo na.locf
#' @importFrom tidyr pivot_longer
#'
#' @export

readGLOBUS <- function() {
  data <- read.xlsx("1_building stock under the NR scenario.xlsx",
                    sheet = "building stock",
                    colNames = FALSE)

  region <- na.locf(as.character(data[1, ]), na.rm = FALSE)
  colnames(data) <- paste(region, data[2, ], sep = "@@")
  colnames(data)[1] <- "period"

  data[-c(1, 2), ] %>%
    pivot_longer(-"period", names_to = c("region", "variable"), names_sep = "@@", values_to = "value") %>%
    mutate(period   = as.integer(.data$period),
           value    = as.numeric(.data$value),
           variable = sub(" (million m2)", "", .data$variable, fixed = TRUE),
           unit     = "Mm2") %>%
    as.quitte() %>%
    as.magpie()
}
