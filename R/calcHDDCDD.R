#' Calculate Heating and Cooling Degree Days (HDD/CDD)
#'
#' Computes or loads annual heating and cooling degree days (HDDs and CDDs)
#' for given socio-economic scenarios.
#'
#' @param scenario Character vector specifying the socio-economic scenario(s), e.g., \code{"ssp2"}.
#' @param fromSource Logical. If \code{TRUE}, loads HDD/CDD data from source.
#'        If \code{FALSE}, computes them using the \strong{CLIMBED} package.
#' @param endOfHistory Integer. Upper temporal boundary for historical data.
#' If it deviates from 2025, \code{fromSource} needs to be set to \code{FALSE}.
#'
#' @returns A magclass object with annual degree-day values.
#'
#' @note
#' For \code{fromSource = FALSE}, the job requires a runtime of >24h and memory > 100G.
#'
#' @author Hagen Tockhorn
#'
#' @importFrom madrat calcOutput readSource toolCountryFill
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter mutate semi_join %>%
#' @importFrom magclass as.magpie
#' @importFrom climbed getDegreeDays
#'
#' @export

calcHDDCDD <- function(scenario = "SSPs",
                       fromSource = TRUE,
                       endOfHistory = 2025) {

  # PARAMETERS -----------------------------------------------------------------

  # get working dir (= output dir) -> climbed
  workingDir <- getwd()
  outDir    <- file.path(workingDir, "climbed")

  # limit temperature range [C]
  tLim <- list("HDD" = seq(9, 19), "CDD" = seq(15, 25))

  # limit and ambient temperature standard deviation [K]
  std <- c("tLim" = 2, "tAmb" = 2)

  # use global parameters
  useGlobalPars <- FALSE

  # all scenarios
  if (scenario == "SSPs") {
    scenario <- c("ssp1", "ssp2", "ssp3", "ssp4", "ssp5")
  }
  allScenarios <- c("historical", scenario)



  # READ-IN DATA ---------------------------------------------------------------

  # population
  pop <- calcOutput("Population", scenario = "SSPs", aggregate = FALSE) %>%
    as.quitte() %>%
    removeColNa()


  if (isTRUE(fromSource)) {
    # read from source
    data <- readSource("HDDCDD", subtype = paste0(scenario, collapse = "|")) %>%
      as.quitte(na.rm = TRUE) %>%
      removeColNa()
  }

  # PROCESS DATA ---------------------------------------------------------------

  ## Calculate degree days ====

  if (isFALSE(fromSource)) {
    # climate change
    dataCC <- getDegreeDays(mappingFile    = "ISIMIPbuildings_fileMapping.csv",
                            bait           = TRUE,
                            tLim           = tLim,
                            std            = std,
                            ssp            = allScenarios,
                            outDir         = outDir,
                            globalPars     = useGlobalPars,
                            endOfHistory   = endOfHistory,
                            noCC           = FALSE,
                            returnPathOnly = FALSE)

    # no climate change
    dataNoCC <- getDegreeDays(mappingFile    = "ISIMIPbuildings_fileMapping.csv",
                              bait           = TRUE,
                              tLim           = tLim,
                              std            = std,
                              ssp            = allScenarios,
                              outDir         = outDir,
                              globalPars     = useGlobalPars,
                              endOfHistory   = endOfHistory,
                              noCC           = TRUE,
                              fileRev        = "noCC",
                              returnPathOnly = FALSE)

    # bind data
    data <- rbind(dataCC, dataNoCC)

    # average historical data to avoid duplicates and overwrite "historical" with respective ssp scenario
    dataHist <- data %>%
      filter(.data$ssp == "historical") %>%
      group_by(across(-all_of("value"))) %>%
      reframe(value = mean(.data$value, na.rm = TRUE))

    dataScen <- data %>%
      filter(.data$ssp != "historical")

    data <- do.call(rbind, lapply(unique(dataScen$ssp), function(s) {
      rbind(dataHist %>%
              mutate(ssp = s),
            dataScen %>%
              filter(.data$ssp == s))
    })) %>%
      unique()

    # replace NAs with zero
    data <- data %>%
      replace_na(list(value = 0))
  }


  # filter population weights
  pop <- pop %>%
    filter(.data$variable %in% toupper(setdiff(allScenarios, "historical"))) %>%
    mutate(ssp = .data$variable,
           variable = NULL)


  # reduce temporal resolution and bring SSPs to capital letters
  data <- data %>%
    semi_join(pop, by = c("region", "period")) %>%
    mutate(ssp = toupper(.data$ssp))

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
