#' Get BRICK aggregation mappings
#'
#' Provides all mappings needed to aggregate BRICK input data to a lower
#' granularity.
#'
#' @param granularity character, name of BRICK granularity
#' @returns named list of brick dimension aggregation mappings
#'
#' @author Robin Hasse
#'
#' @importFrom madrat toolGetMapping
toolGetBrickMapping <- function(granularity) {

  # FUNCTIONS ------------------------------------------------------------------

  .pick <- function(x, ...) {
    lst <- list(...)
    for (col in names(lst)) {
      x <- x[x[[col]] %in% lst[[col]], setdiff(names(x), col), drop = FALSE]
    }
    return(x)
  }


  .pickGranularity <- function(maps, granularities, granularity) {
    dims2agg <- as.list(.pick(granularities, granularity = granularity))
    dims2agg <- dims2agg[!is.na(dims2agg)]
    lapply(setNames(nm = names(dims2agg)), function(dim) {
      .pick(maps[[dim]], dimGranularity = dims2agg[[dim]])
    })
  }



  # READ MAPPINGS --------------------------------------------------------------

  granularities <- toolGetMapping("granularity.csv", type = "sectoral", where = "brick")

  # read each mapping individually to ensure madrat caching
  maps <- list(
    vin = toolGetMapping("agg_vin.csv", type = "sectoral", where = "brick"),
    typ = toolGetMapping("agg_typ.csv", type = "sectoral", where = "brick"),
    loc = toolGetMapping("agg_loc.csv", type = "sectoral", where = "brick")
  )



  # PICK GRANULARITY -----------------------------------------------------------

  .pickGranularity(maps, granularities, granularity)
}
