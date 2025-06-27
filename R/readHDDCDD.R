#' Read historical and projected degree-day data
#'
#' @param subtype specifies SSP scenarios (e.g. "ssp2" or "ssp1|ssp2")
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter mutate pull
#' @importFrom tidyr replace_na
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie

readHDDCDD <- function(subtype = "ssp2") {
  # READ-IN DATA ---------------------------------------------------------------

  files <- list.files(pattern = subtype, full.names = TRUE)

  data <- do.call(rbind, lapply(files, function(f) {
    # read file
    df <- read.csv(f)

    # get SSP scenario
    scen <- df %>%
      getElement("ssp") %>%
      setdiff("historical")

    # overwrite "historical" in ssp column with scenario
    df %>%
      filter(.data$period >= 1990) %>%
      mutate(ssp = scen,
             rcp = sub("\\.", "_", .data$rcp),
             value = replace_na(.data$value, 0))
  }))

  # average historical data to avoid duplicates
  data <- data %>%
    group_by(across(-all_of("value"))) %>%
    reframe(value = mean(.data$value, na.rm = TRUE))

  # prepare for output
  data <- data %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
