#' Join by all common columns
#'
#' Additional columns to join by that have different names can be give. The kind
#' of joint can also be specified.
#'
#' @param x,y A pair of data frames
#' @param by character vector of columns to join by beyond the commonly named
#' @param .direction type of dplyr join, 'left' by default
#' @param exclude character vector with column names that should be excluded
#'   from the search for join columns
#' @param silent boolean, whether to print type of joint the join columns
#' @param ... additional arguments passed to join function
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr left_join right_join full_join anti_join semi_join

join_all <- function(x, # nolint object_name_linter
                     y,
                     by = NULL,
                     .direction = "left",
                     exclude = "value",
                     silent = TRUE,
                     ...) {

  # function name
  fun <- paste0(.direction, "_join")

  # construct by argument
  by <- c(setdiff(intersect(setdiff(colnames(x), names(by)),
                            setdiff(colnames(y), as.character(by))),
                  exclude),
          by)

  # perform join
  xy <- do.call(fun, c(list(x = x, y = y, by = by), list(...)))

  # report function type and effective by argument
  if (isFALSE(silent)) {
    message(fun, " by = ('", paste(as.character(by), collapse = "', '"), "')")
  }

  return(xy)
}
