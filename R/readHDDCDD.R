#' Read historical and projected degree-day data
#'
#' @param subtype specifies SSP scenarios
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter mutate pull
#' @importFrom tidyr replace_na
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie

readHDDCDD <- function(subtype = "ssp2") {
  # READ-IN DATA ---------------------------------------------------------------

  files <- list.files(pattern = paste0(subtype, collapse = "|"), full.names = TRUE)

  data <- do.call(rbind, lapply(files, function(f) {
    # read file
    df <- read.csv(f)

    # get SSP scenario
    scen <- df %>%
      filter(.data$ssp != "historical") %>%
      pull("ssp") %>%
      unique() %>%
      unlist()

    # overwrite "historical" in ssp column with scenario
    df %>%
      mutate(ssp = ifelse(.data$ssp == "historical", scen, .data$ssp),
             rcp = sub("\\.", "_", .data$rcp),
             value = replace_na(.data$value, 0))
  }))

  # prepare for output
  data <- data %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
