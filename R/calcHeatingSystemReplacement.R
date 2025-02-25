#' Calculate Matrix of heating system replacement shares
#'
#' @returns MagPIE object with shares
#'
#' @author Robin Hasse
#'
#' @importFrom magclass complete_magpie mselect dimSums mbind
#' @importFrom madrat toolAggregate readSource toolGetMapping
#' @export

calcHeatingSystemReplacement <- function() {

  # READ -----------------------------------------------------------------------

  replaceShare <- list(
    allHeating     = readSource("EHI", "consumerStudy2021.table2-2", convert = FALSE),
    boilerToBoiler = readSource("EHI", "consumerStudy2021.table2-5", convert = FALSE)
  )

  # total number of respondents
  nRespondents <- c(
    allHeating     = 2181,
    boilerToBoiler = 1276
  )



  # JOIN -----------------------------------------------------------------------

  replaceMatrix <- lapply(names(replaceShare), function(tbl) {
    replaceShare[[tbl]] * nRespondents[[tbl]]
  }) %>%
    stats::setNames(names(replaceShare))

  # replace boiler to boiler replacement by more detailed breakdown
  replaceMatrix <- mbind(
    replaceMatrix[["allHeating"]][, , "Boiler.Boiler", invert = TRUE],
    replaceMatrix[["boilerToBoiler"]]
  )



  # AGGREGATE ------------------------------------------------------------------

  # map to BRICK
  map <- toolGetMapping("technologyMapping_EHI_consumerStudy.csv",
                        type = "sectoral", where = "mredgebuildings") %>%
    filter(.data[["technologyBRICK"]] != "")
  replaceMatrix <- replaceMatrix %>%
    mselect(old = map$technology, new = map$technology) %>%
    toolAggregate(map, from = "technology", to = "technologyBRICK", dim = 3.1) %>%
    toolAggregate(map, from = "technology", to = "technologyBRICK", dim = 3.2, partrel = TRUE)

  # remove zero that the aggregation introduced
  replaceMatrix <- replaceMatrix[, , replaceMatrix > 0]

  # disaggregate flows between HP and boilers to boiler technologies
  replaceMatrix <- do.call(mbind, lapply(getNames(replaceMatrix), function(n) {
    if (!grepl("_", n)) {
      return(NULL)
    }
    flows <- strsplit(n, "\\.")[[1]] %>%
      stats::setNames(getSets(replaceMatrix)[3:4]) %>%
      lapply(function(x) strsplit(x, "_")[[1]])
    dimDisagg <- names(flows)[lapply(flows, length) > 1]
    dimKeep <- setdiff(names(flows), dimDisagg)
    elementDisagg <- paste(flows[[dimDisagg]], collapse = "_")
    flows[[dimDisagg]] <- intersect(flows[[dimDisagg]],
                                    getItems(replaceMatrix, dimDisagg))
    toolAggregate(replaceMatrix[, , n],
                  rel = data.frame(from = elementDisagg,
                                   to = flows[[dimDisagg]]),
                  from = "from", to = "to", dim = dimDisagg,
                  weight = replaceMatrix %>%
                    mselect(flows[dimDisagg]) %>%
                    dimSums(dimKeep))
  })) %>%
    mbind(replaceMatrix[, , grep("_", getNames(replaceMatrix), value = TRUE), invert = TRUE])

  # normalise back
  replaceMatrix <- proportions(replaceMatrix) %>%
    complete_magpie(0)



  # RETURN ---------------------------------------------------------------------

  return(list(x = replaceMatrix,
              weight = as.magpie(1),
              min = 0,
              max = 1,
              description = "Share of respondents that replaced a heating system",
              unit = "1"))
}
