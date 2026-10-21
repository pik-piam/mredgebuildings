#' Read GLOBUS building stock
#'
#' Building stock projections for residential, non-residential and total building stock in
#' million m2 under the NR (no renovation) scenario from the GLOBUS building turnover model.
#'
#' @references https://doi.org/10.1016/j.ynexs.2024.100019
#'
#' @param subtype character, either \code{"total"} (building stock in million m2, default)
#'   or \code{"perCapita"} (population and floor area per capita in m2)
#'
#' @return MAgPIE object containing building stock data
#'
#' @author Hagen Tockhorn
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom zoo na.locf
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#' @importFrom dplyr mutate .data
#' @importFrom magclass as.magpie
#'
#' @export

readGLOBUS <- function(subtype = "total") {

  sheet <- switch(subtype,
                  total     = "building stock",
                  perCapita = "per capita floorspace",
                  stop("Invalid subtype '", subtype, "'. Must be either 'total' or 'perCapita'."))

  units <- c("million m2" = "Mm2",
             "m2"         = "m2/cap")

  data <- read.xlsx("1_building stock under the NR scenario.xlsx",
                    sheet = sheet,
                    colNames = FALSE)

  regionmap <- toolGetMapping(name  = "regionmappingGLOBUS.csv",
                              type  = "regional",
                              where = "mredgebuildings")

  region <- na.locf(as.character(data[1, ]), na.rm = FALSE)
  colnames(data) <- paste(region, data[2, ], sep = "@@")
  colnames(data)[1] <- "period"

  data[-c(1, 2), ] %>%
    pivot_longer(-"period", names_to = c("region", "variable"), names_sep = "@@", values_to = "value") %>%
    mutate(period   = as.integer(.data$period),
           value    = as.numeric(.data$value),
           unit     = ifelse(grepl("\\(.*\\)$", .data$variable),
                             units[sub("^.*\\((.*)\\)$", "\\1", .data$variable)],
                             "cap"),
           variable = sub(" \\(.*\\)$", "", .data$variable)) %>%
    left_join(regionmap %>%
                select(-"CountryCode") %>%
                unique(),
              by = c("region" = "Region")) %>%
    select(-"region") %>%
    rename("region" = "RegionCode") %>%
    as.quitte() %>%
    as.magpie()
}
