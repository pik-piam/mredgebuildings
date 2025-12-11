#' Calculate Status Quo Share
#'
#' The share of heating systems that are replaced with the same technology
#' during heating system replacement for each technology.
#'
#' Quantifying the status quo bias requires detailed data on heating system
#' replacement activities with initial and final technology. There are very few
#' and mostly rough sources on this. We therefore assume technology-specific
#' and temporally and spatially invariant shares of initial systems that are
#' identically replaced. This leaves enough degrees of freedom to the model to
#' reflect region-specific stocks and preferences while representing the status
#' quo bias. It might make sense to model the decision as a two-stage process in
#' which agents first decide whether to just replace identically and thus follow
#' the bias or decide for a new system independent of their old one. This is not
#' yet reflected in this calculation.
#'
#' @author Robin Hasse
#'
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom quitte as.quitte
#' @importFrom tidyr complete
#' @importFrom dplyr %>% .data select mutate across all_of group_by ungroup
#'   filter

calcStatusQuoShare <- function() {
  .calcIdentReplShare <- function(df) {
    df %>%
      group_by(.data$old) %>%
      mutate(value = proportions(.data$value)) %>%
      ungroup() %>%
      filter(.data$old == .data$new) %>%
      select(hs = "old", "value")
  }

  .extrapolToAllHs <- function(df, hsFull) {
    out <- complete(df, hs = hsFull)
    # for district heating, we assume the highest observed share
    out[is.na(out$value) & out$hs == "dihe", "value"] <- max(df$value)
    # for the rest, we assume the lowest
    out[is.na(out$value), "value"] <- min(df$value)
    out
  }

  hsFull <- toolGetMapping("dim_hs.csv", type = "sectoral", where = "brick") %>%
    getElement("hs") %>%
    unique()

  data <- calcOutput("HeatingSystemReplacement", aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE) %>%
    mutate(across(all_of(c("old", "new")), as.character)) %>%
    .calcIdentReplShare() %>%
    .extrapolToAllHs(hsFull) %>%
    as.magpie(tidy = TRUE)

  return(list(x = data,
              min = 0,
              max = 0,
              isocountries = FALSE,
              unit = "1",
              description = "Share of heating systems that are identically replaced"))
}
