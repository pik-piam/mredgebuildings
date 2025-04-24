#' Calculate Heating and Cooling Degree Days (HDD/CDD)
#'
#' Computes or loads annual heating and cooling degree days (HDDs and CDDs)
#' for given socio-economic scenarios. If \code{readFromSource = TRUE},
#' data is read from the \code{HDDCDD} source. Otherwise, degree days are
#' calculated using the \strong{CLIMBED} package with specified parameters
#' such as temperature limits and standard deviations.
#'
#' @param scenario Character vector specifying the socio-economic scenario(s), e.g., \code{"ssp2"}.
#' @param fromSource Logical. If \code{TRUE}, loads HDD/CDD data from source.
#'        If \code{FALSE}, computes them using the CLIMBED package.
#'
#' @returns A magclass object with annual degree-day values.
#'
#' @author Hagen Tockhorn
#'
#' @importFrom madrat calcOutput readSource toolCountryFill
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter mutate semi_join %>%
#' @importFrom magclass as.magpie
#'
#' @export

calcHDDCDD <- function(scenario = c("ssp2"), fromSource = TRUE) {

  # PARAMETERS -----------------------------------------------------------------

  # nolint start

  # # get madrat sourcefolder -> HDDCDD -> calcHDDCDD
  # sourceDir <- getConfig("sourcefolder")
  # outDir    <- file.path(sourceDir, "HDDCDD", "calcHDDCDD")
  #
  # # limit temperature range
  # tLim <- list("HDD" = seq(9, 19), "CDD" = seq(15, 25))
  #
  # # limit and ambient temperature standard deviation
  # std <- c("tLim" = 2, "tAmb" = 2)
  #
  # # use global parameters
  # globalPars <- TRUE
  #
  # # end of history
  # endOfHistory <- 2025

  # nolint end

  # all scenarios
  allScenarios <- c("historical", scenario)



  # READ-IN DATA ---------------------------------------------------------------

  # population
  pop <- calcOutput("Population", scenario = "SSPs", aggregate = FALSE) %>%
    as.quitte() %>%
    removeColNa()


  if (isTRUE(fromSource)) {
    # read from source
    data <- readSource("HDDCDD", subtype = scenario) %>%
      as.quitte(na.rm = TRUE) %>%
      removeColNa()
  }



  # PROCESS DATA ---------------------------------------------------------------

  # nolint start

  ## Calculate degree days ====

  # if (isFALSE(fromSource)) {
  #   # climate change
  #   dataCC <- getDegreeDays(mappingFile  = "ISIMIPbuildings_fileMapping.csv",
  #                           bait         = TRUE,
  #                           tLim         = tLim,
  #                           std          = std,
  #                           ssp          = allScenarios,
  #                           outDir       = outDir,
  #                           globalPars   = globalPars,
  #                           endOfHistory = endOfHistory,
  #                           noCC         = FALSE) %>%
  #     read.csv()
  #
  #   # no climate change
  #   dataNoCC <- getDegreeDays(mappingFile  = "ISIMIPbuildings_fileMapping.csv",
  #                             bait         = TRUE,
  #                             tLim         = tLim,
  #                             std          = std,
  #                             ssp          = allScenarios,
  #                             outDir       = outDir,
  #                             globalPars   = globalPars,
  #                             endOfHistory = endOfHistory,
  #                             noCC         = TRUE) %>%
  #     read.csv()
  #
  #   # bind data
  #   data <- rbind(dataCC, dataNoCC)
  #
  #
  #   # overwrite "historical" with respective ssp scenario
  #   dataHist <- data %>%
  #     filter(.data$ssp == "historical")
  #
  #   dataScen <- data %>%
  #     filter(.data$ssp != "historical")
  #
  #   data <- do.call(rbind, lapply(unique(dataScen$ssp), function(s) {
  #     rbind(dataHist %>%
  #             mutate(ssp = s),
  #           dataScen %>%
  #             filter(.data$ssp == s))
  #   })) %>%
  #     unique()

  ### replace NAs with zero


  # }

  # nolint end


  # filter population weights
  pop <- pop %>%
    mutate(variable = tolower(.data$variable)) %>%
    filter(.data$variable %in% allScenarios[allScenarios != "historical"]) %>%
    mutate(ssp = .data$variable,
           variable = NULL)


  # reduce temporal resolution
  data <- data %>%
    semi_join(pop, by = c("region", "period"))

  pop <- pop %>%
    semi_join(data, by = c("period"))



  # OUTPUT ---------------------------------------------------------------------

  data <- data %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(fill = 0)

  pop <- pop %>%
    as.quitte() %>%
    as.magpie()


  return(list(x = data,
              weight = pop,
              unit = "K.d/yr",
              min = 0,
              description = "Annual degree-days for various socio-economic and climate scenarios"))
}
