#' Read UN data on Household Size & Composition
#'
#' Compilation of indicators on household size and membership composition. The
#' estimates are based on 814 unique data points from 172 countries or areas,
#' representing approximately 97 per cent of the world’s population in 2019,
#' collected between 1960 to 2018.
#'
#' @source https://www.un.org/development/desa/pd/data/household-size-and-composition
#'
#' @param subtype household variable
#' @return magpie object
#'
#' @author Robin Hasse
#'
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{ a <- readSource(type = "UNHouseholds")
#' }
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr select
#' @export

readUNHouseholds <- function(subtype) {

  data <- suppressMessages(read_xlsx(
    "undesa_pd_2019_houseshold_size_and_composition_dataset.xlsx",
    sheet = "UN HH Size and Composition 2019", skip = 4, na = ".."
  )) %>%
    select(-"ISO Code", -"Data source category") %>%
    rename(region = "Country or area",
           period = "Reference date (dd/mm/yyyy)")

  # filter variable
  data <- switch(subtype,
    avgHHSize         = select(data, "region", "period", 3),
    HHBySize          = select(data, "region", "period", 4:7),
    femaleHead        = select(data, "region", "period", 8),
    HHByAgeOFHead     = select(data, "region", "period", 9:12),
    HHOneMemberAge    = select(data, "region", "period", 13:16),
    HHMembersAge      = select(data, "region", "period", 17:22),
    avgHHMembersOfAge = select(data, "region", "period", 23:27),
    BasicHHType       = select(data, "region", "period", 28:32),
    IntergenHHType    = select(data, "region", "period", 28:41),
    stop("'", subtype, "'", "is not a valid subtype.")
  )

  # prepare MAgPIE object
  data <- data %>%
    gather("item", "value", -"region", -"period") %>%
    filter(!is.na(.data[["value"]])) %>%
    mutate(variable = subtype,
           period = format(.data[["period"]], "%Y")) %>%
    group_by(across(c("region", "period", "variable", "item"))) %>%
    summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie() %>%
    collapseDim()

  return(data)
}
