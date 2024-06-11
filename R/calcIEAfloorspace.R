#' floor space by subsectors
#'
#' @export
calcIEAfloorspace <- function() {

  x <- readSource("IEAfloorspace", convert = FALSE)

  return(list(
    x = x,
    isocountries = FALSE,
    unit = "billion m2",
    description = paste0(
      "Residential and commercial floor space for big world regions from the ",
      "IEA TCEP report 2014 in billion m2."
    )
  ))
}
