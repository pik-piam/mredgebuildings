#' Read VHK data
#'
#' multiple EU studies mostly on different categories of appliances
#'
#' @author Robin Hasse
#'
#' @param subtype character, specifies study and the respective csv file to
#'   read: \code{<study>.<file>}
#' @returns MagPIE object
#'
#' @importFrom dplyr %>% .data select mutate distinct across all_of
#' @importFrom magclass as.magpie
#' @export

readVHK <- function(subtype) {
  study <- sub("^(.*)\\.(.*)$", "\\1", subtype)
  file <- sub("^(.*)\\.(.*)$", "\\2", subtype)

  folder <- switch(study,
    `2019_spaceHeating` =
      "2019_Review_study_of_ecodesign_and_energy_labelling_for_space_heating_boilers_and_combination_heaters",
    stop("Unknown study")
  )

  fileName <- paste0(file, ".csv")
  filePath <- file.path(folder, fileName)

  data <- read.csv2(filePath) %>%
    select("region", "period", "variable", "unit", "value") %>%
    distinct(across(all_of(c("region", "period", "variable"))),
             .keep_all = TRUE) %>%
    mutate(value = as.numeric(.data$value)) %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value",
              tidy = FALSE)
  return(data)
}
