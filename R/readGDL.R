#' Read Global Data Lab
#'
#' Different databases each comprising the harmonised results of
#' regional/national surveys. Available subtypes are
#' - SHDI: Subnational Human development Database
#' - AreaDatabase: Subnational development Indicators (low/medium income countries)
#'
#' @references https://doi.org/10.1038/sdata.2019.38
#' @references https://globaldatalab.org/asset/286/Smits%20GDL%20Working%
#' 20Paper%2016-101%20v360.pdf
#' @source https://globaldatalab.org/
#'
#' @param subtype <database>.<variable>
#' @return magpie object
#'
#' @author Robin Hasse
#'
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{ a <- readSource(type = "GDL")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% select rename filter any_of across group_by summarise
#'   .data
#' @importFrom tidyr gather
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

readGDL <- function(subtype) {

  # split subtype
  database <- gsub("\\..*$", "", subtype)
  variable <- if (database == subtype) {
    NULL
  } else {
    gsub("^.*\\.", "", subtype)
  }

  # raw data
  data <- read.csv(switch(database,
    SGDI = "SHDI-SGDI-Total 5.0.csv",
    AreaDatabase = "GDL-AreaData400 (1).csv",
    stop("'", subtype, "'", "is not a valid subtype.")
  ))

  # variable definition
  varNames <- read.csv(switch(database,
    SGDI = "SHDI-SGDI-5.0-Vardescription.csv",
    AreaDatabase = "GDL-AreaData400-Variabledescription (1).csv"
  ))

  # columns to remove
  removeCols <- varNames %>%
    filter(.data[["Category"]] == "System") %>%
    getElement("Variable") %>%
    setdiff(c("GDLCODE", "year"))

  # remove redundant information, take mean of duplicates (multiple sources)
  data <- data %>%
    select(-any_of(c(removeCols, "contin"))) %>%
    rename(region = "GDLCODE",
           period = "year") %>%
    gather("variable", "value", -"region", -"period") %>%
    filter(!is.na(.data[["value"]])) %>%
    group_by(across(c("region", "period", "variable"))) %>%
    summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie()

  # filter variables
  if (is.null(variable)) {
    return(data)
  }
  vars <- grep(variable, getItems(data, dim = 3), value = TRUE)
  if (length(vars) == 0) {
    stop("No variables found that match '", variable, "'.")
  }
  data <- data[, , vars]

  return(data)
}
