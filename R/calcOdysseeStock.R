#' Calculate Odyssee building stock variables
#'
#' Residential stock variables are given in number of dwellings, flats or
#' houses while commercial stock is given in Floor space. This function
#' computes all stock variables in terms of floor space considering dwelling
#' size.
#'
#' The data can be inter- and extrapolated using IDEES floor space data. As we
#' trust the absolute level less than Odyssee, we only consider the growth to
#' fill vales.
#'
#' @param extrapolate logical, if TRUE, floor space growth rates from JRC_IDEES
#'   are used to inter- and extrapolate the data
#' @returns MagPIE object with building stock data
#'
#' @author Robin Hasse
#'
#' @importFrom magclass add_columns collapseDim mbind
#' @importFrom madrat readSource toolGetMapping
#' @importFrom dplyr %>% case_when .data
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte interpolate_missing_periods removeColNa
#' @export

calcOdysseeStock <- function(extrapolate = FALSE) {

  # FUNCTIONS ------------------------------------------------------------------

  pick <- function(x, var) mselect(x, variable = var, collapseNames = TRUE)


  naIfBeyondData <- function(x, period, data) {
    x[is.na(data[match(x, period)])] <- NA
    return(x)
  }

  .extrapol <- function(x, t, ref, tref) { # nolint: object_usage_linter.
    ref * x[match(tref, t)] / ref[match(tref, t)]
  }

  .interpol <- function(x, t, ref, before, after) { # nolint: object_usage_linter.
    pos <- (t - before) / (after - before)
    ref *
      ((1 - pos) * x[match(before, t)] + pos * x[match(after, t)]) /
      ((1 - pos) * ref[match(before, t)] + pos * ref[match(after, t)])
  }



  # READ -----------------------------------------------------------------------

  m <- toolGetMapping("stockMap_Odyssee.csv",
                      type = "sectoral", where = "mredgebuildings",
                      returnPathOnly = TRUE) %>%
    read.csv(na.strings = "")

  data <- readSource("Odyssee")



  # CALC FLOOR SPACE -----------------------------------------------------------

  varsComplete <- m[is.na(m[["sizeCode"]]), "quantityCode"]
  varsIncomplete <- setdiff(m[["quantityCode"]], varsComplete)

  x <- do.call(what = mbind, lapply(varsIncomplete, function(varQuan) {
    varSize <- m[m[["quantityCode"]] == varQuan, "sizeCode"]
    (pick(data, varQuan) * pick(data, varSize)) %>%
      add_dimension(3.1, "variable", paste(varQuan, varSize, sep = "*"))
  })) %>%
    mbind(collapseDim(data[, , varsComplete], dim = "unit"))



  # EXTRAPOLATE ----------------------------------------------------------------

  if (isTRUE(extrapolate)) {

    m <- m %>%
      mutate(variable = ifelse(.data$quantityCode %in% varsIncomplete,
                               paste(.data$quantityCode, .data$sizeCode,
                                     sep = "*"),
                               .data$quantityCode),
             ideesCode = dot2Dash(.data$ideesCode)) %>%
      select("variable", "ideesCode")

    ideesCodes <- dot2Dash(unique(m[["ideesCode"]]))

    idees <- mbind(readSource("JRC_IDEES", subtype = "Residential_2021") %>%
                     mselect(code = ideesCodes),
                   readSource("JRC_IDEES", subtype = "Tertiary_2021") %>%
                     mselect(code = ideesCodes)) %>%
      collapseDim(keepdim = "code") %>%
      as.quitte() %>%
      removeColNa() %>%
      left_join(m, by = c(code = "ideesCode"),
                relationship = "many-to-many")

    x <- as.quitte(x)
    xInterpol <- x %>%
      interpolate_missing_periods(expand.values = TRUE) %>%
      removeColNa()
    x <- x %>%
      removeColNa() %>%
      right_join(xInterpol, by = c("region", "period", "variable"),
                 suffix = c("", "Interpol")) %>%
      full_join(idees, by = c("region", "period", "variable"),
                suffix = c("Odyssee", "IDEES")) %>%
      group_by(across(all_of(c("region", "variable")))) %>%
      arrange(.data$period) %>%
      mutate(section = cumsum(c(diff(c(FALSE, is.na(.data$valueOdyssee))) != 0))) %>%
      group_by(across(all_of(c("region", "variable", "section")))) %>%
      mutate(before = min(.data$period) - 1,
             after  = max(.data$period) + 1) %>%
      group_by(across(all_of(c("region", "variable")))) %>%
      mutate(before = naIfBeyondData(.data$before, .data$period, .data$valueIDEES),
             after  = naIfBeyondData(.data$after,  .data$period, .data$valueIDEES),
             fill = case_when(
               !is.na(.data$before) & !is.na(.data$after) ~
                 .interpol(.data$valueInterpol, .data$period, .data$valueIDEES, .data$before, .data$after),
               !is.na(.data$before) ~
                 .extrapol(.data$valueInterpol, .data$period, .data$valueIDEES, .data$before),
               !is.na(.data$after) ~
                 .extrapol(.data$valueInterpol, .data$period, .data$valueIDEES, .data$after),
               .default = NA
             )) %>%
      ungroup() %>%
      mutate(value = ifelse(is.na(.data$valueOdyssee),
                            ifelse(is.na(.data$valueIDEES), NA, .data$fill),
                            .data$valueOdyssee)) %>%
      select("region", "period", "variable", "value") %>%
      as.magpie(spatial = "region", temporal = "period", datacol = "value")
  }



  # RETURN ---------------------------------------------------------------------

  return(list(x = x,
              min = 0,
              unit = "m2",
              description = "Floor space of building stock"))
}
