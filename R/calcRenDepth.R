#' Calculate renovation depth
#'
#' The renovation depth is currently calculated based on the ratio of specific
#' useful energy demand after and before the renovation. This is valid under the
#' assumption that there is no change in behaviour (rebound effect) which we
#' currently make. Once adaptive behaviour is considered, we should use the
#' ratio of U-values.
#'
#' @author Robin Hasse
#'
#' @importFrom madrat toolGetMapping
#' @importFrom dplyr %>% .data left_join filter mutate select
#' @importFrom magclass as.magpie collapseDim
#' @export

calcRenDepth <- function() {

  bsMap <- toolGetMapping("dim_bs.csv",
                          type = "sectoral", where = "brick")

  renDepth <- expand.grid(bs = bsMap[["bs"]],
                          bsr = c(bsMap[["bs"]], "0")) %>%
    left_join(bsMap, by = c("bs")) %>%
    left_join(bsMap, by = c(bsr = "bs"), suffix = c("Before", "After")) %>%
    filter(.data[["energyLadderBefore"]] >= .data[["energyLadderAfter"]] |
             .data[["bsr"]] == 0) %>%
    mutate(value = ifelse(.data[["bsr"]] == "0",
                          0,
                          1 - .data[["relDemAfter"]] / .data[["relDemBefore"]])) %>%
    select("bs", "bsr", "value") %>%
    as.magpie() %>%
    collapseDim()

  return(list(x = renDepth,
              isocountries = FALSE,
              min = 0,
              max = 1,
              unit = "1",
              description = "Renovation depth"))
}
