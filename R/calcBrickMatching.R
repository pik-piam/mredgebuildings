calcBrickMatching <- function(subtype) {
  data <- switch(subtype,
    stock        = readSource("BrickMatching", subtype = "stock"),
    construction = readSource("BrickMatching", subtype = "construction"),
    renovation   = readSource("BrickMatching", subtype = "renovation"),
    demolition   = readSource("BrickMatching", subtype = "demolition"),
    stop("unkown subtype")
  )
  list(x = data,
       weight = NULL,
       min = 0,
       unit = switch(subtype, stock = "million m2", "million m2/yr"),
       description = paste("building", subtype, "floorspace"))
}
