#' Calculate historic renovation flows
#'
#' Combine data on heating system sales and replacement to derive full
#' building state transition data
#'
#' @returns MagPIE object with historic renovation flows
#' @author Robin Hasse
#'
#' @importFrom dplyr %>%
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass mselect dimSums getSets<- getItems add_dimension
#'   time_interpolate add_columns
#' @export

calcRenovation <- function() {

  # FUNCTION -------------------------------------------------------------------

  completeHs <- function(x, hs, dim = 3.1, fill = 0) {
    add_columns(x, addnm = setdiff(hs, getItems(x, dim)), dim = dim,
                fill = fill)
  }


  completeReplacement <- function(hsReplacement, hsSales, stock, hs) {
    ehiRegs <- c("DEU", "FRA", "ITA", "BGR", "POL")
    hsSales <- hsSales %>%
      mselect(region = ehiRegs, period = "y2021") %>%
      dimSums(c(1, 2)) %>%
      completeHs(hs)

    getSets(hsSales)[3] <- "new"

    hsStock <- stock %>%
      mselect(region = ehiRegs, period = "y2020") %>% # 2021 not available
      dimSums(setdiff(getSets(stock), "hs"), na.rm = TRUE)
    getSets(hsStock)[3] <- "old"

    hsReplacement %>%
      toolCompleteRenFlows(from = hsStock, to = hsSales)
  }



  # ASSUMPTIONS ----------------------------------------------------------------

  # TODO: include commercial buildings # nolint: todo_comment_linter.
  resShare <- 0.65

  hsPerDwel <- new.magpie("DEU", names = c("SFH", "MFH"),
                          sets = c("region", "period", "typ"))
  hsPerDwel[] <- c(1, 0.2)
  hsPerDwel <- collapseDim(hsPerDwel)



  # READ -----------------------------------------------------------------------

  hs <- toolGetMapping("dim_hs.csv",
                       type = "sectoral", where = "brick") %>%
    getElement("hs") %>%
    unique()

  hsCon <- calcOutput("Construction", aggregate = FALSE) %>%
    mselect(variable = "nBoilers") %>%
    dimSums(c(3.1, 3.3, 3.4))

  hsSales <- calcOutput("HeatingSystemSales", aggregate = FALSE)

  hsReplacement <- calcOutput("HeatingSystemReplacement",
                              aggregate = FALSE)

  r <- "DEU"
  t <- intersect(getItems(hsCon, 2), getItems(hsSales, 2))

  stock <- calcOutput("BuildingStock", aggregate = FALSE) %>%
    time_interpolate(t) %>%
    completeHs(hs)

  # subtract new construction from heating system sales
  # TODO: extend to more countries (and time steps) # nolint: todo_comment_linter.

  hsRen <- hsSales[r, t, ] - hsCon[r, t, ]
  hsRen[] <- ifelse(hsRen <= 0, 0.1 * hsSales[r, t, ], hsRen)
  getSets(hsRen)[3] <- "hsr"
  hsRen <- completeHs(hsRen, hs)

  # complete replacement data
  hsReplacement <- completeReplacement(hsReplacement, hsSales, stock, hs)

  # shares conditional to new system
  hsReplaceShares <- hsReplacement / dimSums(hsReplacement, 3.1)
  hsReplaceShares[is.na(hsReplaceShares)] <- 0
  getSets(hsReplaceShares)[3:4] <- c("hs", "hsr")

  # apply replacement shares
  hsRenFull <- do.call(mbind, lapply(hs, function(h) {
    add_dimension(hsRen, add = "hs", nm = h) *
      if (h %in% getItems(hsReplaceShares, "hs")) {
        mselect(hsReplaceShares, hs = h)
      } else {
        0
      }
  }))

  # distribute across stock subsets
  disWeight <- hsPerDwel * dimSums(stock[r, t, "dwellings"], c("variable", "hs", "vin", "bs"))
  disWeight <- disWeight / dimSums(disWeight)
  hsRenFullDis <- hsRenFull * resShare * disWeight

  # scale to floor space
  dwelSize <- dimSums(stock[r, t, "floor"], c("variable", "hs", "vin", "bs")) /
    dimSums(stock[r, t, "dwellings"], c("variable", "hs", "vin", "bs"))
  hsRenFullDis <- hsRenFullDis * dwelSize / hsPerDwel / 1E6 # hs units/ yr -> million m2/yr

  hsRenFullDis <- toolCountryFill(hsRenFullDis, verbosity = 2)

  return(list(x = hsRenFullDis,
              weight = NULL,
              min = 0,
              unit = " million m2/yr",
              description = "Historic renovation flows"))
}
