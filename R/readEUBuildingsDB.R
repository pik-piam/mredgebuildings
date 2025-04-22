#' Read EU Buildings Database
#'
#' Compilation of various data sources from the European Commission. Because of
#' the considereable size of this database, a subtype has to be provided. The
#' subtype represents the category of items. It is possible to append selected
#' variables for further filtering.
#'
#' @param subtype character <category>.<variable>
#' @return magpie object
#'
#' @author Robin Hasse
#'
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{ a <- readSource(type = "EUBuildingsDB")
#' }
#'
#' @importFrom madrat toolSubtypeSelect
#' @importFrom utils read.csv
#' @importFrom dplyr %>% slice mutate filter matches .data
#' @importFrom tidyr unite gather
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie mselect getNames dimSums getItems
#' @export

readEUBuildingsDB <- function(subtype = "") {

  # split subtype
  category <- strsplit(subtype, ".", fixed = TRUE)[[1]][1]
  variable <- strsplit(subtype, ".", fixed = TRUE)[[1]][2]

  # available source files
  files <- list.files(pattern = "\\.csv$")
  files <- stats::setNames(files, gsub(".csv", "", files))
  file <- toolSubtypeSelect(category, files)

  # read csv file and stack regions
  data <- read.csv(file, skip = 1, header = FALSE, encoding = "ISO-8859-1")
  for (j in 1:2) {
    data[, j] <- iconv(data[, j], "ISO-8859-1", "ASCII", "byte")
  }
  startLines <- which(data[, 1] == "") - 1
  data <- do.call("rbind", lapply(seq_along(startLines), function(i) {
    endLine <- ifelse(i == length(startLines),
                      nrow(data) - 4,
                      startLines[i + 1] - 1)
    chunk <- data[startLines[i]:endLine, ]
    chunk <- chunk %>%
      slice(-(1:2)) %>%
      `colnames<-`(tolower(c("variable", chunk[2, -1]))) %>%
      mutate(region = chunk[1, 1])
    return(chunk)
  }))

  # drop meta data, merge variable and unit, convert to magpie
  # drop EU region as it contains duplicates
  data <- data %>%
    select(matches("period|region|variable|unit|\\d{4}")) %>%
    gather("period", "value", matches("\\d{4}")) %>%
    filter(!is.na(.data[["value"]]),
           .data[["region"]] != "EU") %>%
    mutate(unit = gsub("\\. | ", "-", .data[["unit"]])) %>%
    unite("variable", "variable", "unit") %>%
    as.quitte() %>%
    as.magpie()

  if (identical(variable, as.character(NA))) {
    return(data)
  }

  # further filtering of variables
  variableRegEx <- list(
    `BuildingStockCharacteristics.residentialVintageShares` =
      "^Share of dwellings built (before|after|between \\d{4} and) \\d{4}",
    `BuildingStockCharacteristics.shareHouseholdSize` =
      "^(Number|Share) of dwelling.* (with single-person households|occupied by)"
  )
  if (subtype %in% names(variableRegEx)) {
    data <- mselect(data, variable = grep(variableRegEx[[subtype]],
                                          getNames(data), value = TRUE))
  } else {
    stop("'", variable, "' is not a valid variable for this subtype. ",
         "Valid subtypes with variable filtering are: ",
         paste(names(variableRegEx), collapse = ", "))
  }

  # remove regions and time steps that are all NA
  regions <- getItems(data, 1)[which(dimSums(is.na(data), c(2, 3)) < dim(data)[2] * dim(data)[3])]
  periods <- getItems(data, 2)[which(dimSums(is.na(data), c(1, 3)) < dim(data)[1] * dim(data)[3])]
  data <- data[regions, periods, ]

  return(data)
}
