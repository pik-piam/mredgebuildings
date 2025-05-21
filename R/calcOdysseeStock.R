#' Calculate Odyssee building stock variables
#'
#' Residential stock variables are given in number of dwellings, flats or
#' houses while commercial stock is given in Floor space. This function
#' computes all stock variables in terms of floor space considering dwelling
#' size.
#'
#' The data can be inter- and extrapolated using IDEES floor space data. As we
#' trust the absolute level less than Odyssee, we only consider the growth to
#' fill values.
#'
#' @param interpolate logical, if TRUE, floor space growth rates from JRC_IDEES
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

calcOdysseeStock <- function(interpolate = FALSE) {

  # FUNCTIONS ------------------------------------------------------------------

  pick <- function(x, var) mselect(x, variable = var, collapseNames = TRUE)


  .interpol <- function(x, t, ref, before, after, brwsr = FALSE) { # nolint: object_usage_linter.
    # inter-/extrapolate ref
    refFull <- if (sum(!is.na(ref)) >= 2) {
      stats::approx(x = t, y = ref, xout = t, rule = 2)$y
    } else {
      ref
    }
    factor <- case_when(
      before %in% t & !after %in% t ~
        x[match(before, t)] / ref[match(before, t)],
      !before %in% t &  after %in% t ~
        x[match(after, t)] / ref[match(after, t)],
      .default = {
        pos <- (t - before) / (after - before)
        (
          (1 - pos) * x[match(before, t)] + pos * x[match(after, t)]
        ) / (
          (1 - pos) * refFull[match(before, t)] + pos * refFull[match(after, t)]
        )
      }
    )
    factor * ref
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



  # INTERPOLATE ----------------------------------------------------------------

  if (isTRUE(interpolate)) {

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


    x <- x %>%
      as.quitte() %>%
      removeColNa() %>%
      full_join(idees, by = c("region", "period", "variable"),
                suffix = c("Odyssee", "IDEES")) %>%
      # divide time series into sections of consecutive periods with Odyssee
      # value that are either all NA or not
      group_by(across(all_of(c("region", "variable")))) %>%
      arrange(.data$period) %>%
      mutate(section = cumsum(c(diff(c(FALSE, is.na(.data$valueOdyssee))) != 0))) %>%
      # The NA sections are filled with the IDEES profile scaled by a linearly
      # changing factor that connects the final values towards both sides with
      # existing values before and after the section. If no values exist on
      # either side, they are extrapolated using a time-invariant factor.
      group_by(across(all_of(c("region", "variable", "section")))) %>%
      mutate(before = min(.data$period) - 1,
             after  = max(.data$period) + 1) %>%
      group_by(across(all_of(c("region", "variable")))) %>%
      mutate(fill = .interpol(.data$valueOdyssee,
                              .data$period,
                              .data$valueIDEES,
                              .data$before,
                              .data$after)) %>%
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
