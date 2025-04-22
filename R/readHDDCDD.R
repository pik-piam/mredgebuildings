#' Read HDD CDD
#'
#' Read heating and cooling degree days; past and scenario data as magclass
#' objects
#'
#' @param subtype Temperature threshold for computing HDD and CDD
#' @return magpie object HDD CDD
#' @author Antoine Levesque
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#'
#' \dontrun{ a <- readSource(type="HDDCDD")
#' }
#' @importFrom utils read.csv
#' @importFrom tidyr gather
#' @importFrom dplyr mutate rename .data
#' @importFrom quitte as.quitte removeColNa

readHDDCDD <- function(subtype = 18) {

  rcps <- paste0("rcp", c("2p6", "4p5", "6p0", "8p5"))
  variables <- c("HDD", "CDD")
  ssps <- paste0("ssp", 1:5)

  dfFuture <- do.call("rbind", do.call("rbind", do.call("rbind",
    lapply(rcps, function(rcp) {
      lapply(variables, function(var) {
        lapply(ssps, function(ssp) {
          paste0("GFDL-ESM2M_", rcp, "_", var, "_", subtype, "_", ssp, ".csv") %>%
            read.csv(na.strings = "--") %>%
            removeColNa() %>%
            gather("region", "value", -.data[["year"]]) %>%
            mutate(scenario = ssp,
                   rcp      = rcp,
                   variable = var,
                   tlimit   = subtype) %>%
            rename(period = "year") %>%
            as.quitte()
        })
      })
    })
  )))

  dfPast <- do.call("rbind", lapply(variables, function(var) {
    paste0("GSWP3_historical_", var, "_", subtype, ".csv") %>%
      read.csv(na.strings = "--") %>%
      removeColNa() %>%
      gather("region", "value", -"year") %>%
      mutate(scenario = "history",
             rcp      = "history",
             variable = var,
             tlimit   = subtype) %>%
      rename(period = "year") %>%
      as.quitte()
  }))

  mdata <- rbind(dfPast, dfFuture) %>%
    as.magpie()

  return(mdata)
}
