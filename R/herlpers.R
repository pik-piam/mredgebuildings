#' Replace dots by dashes
#'
#' @param x character vector
#' @param rev logical, if TRUE, replace dashes by dots
#' @returns character vector with replaced symbols
#'
#' @author Robin Hasse

dot2Dash <- function(x, rev = FALSE) {
  if (isTRUE(rev)) {
    gsub(pattern = "-", replacement = "\\.", x)
  } else {
    gsub(pattern = "\\.", replacement = "-", x)
  }
}
