readBrickMatching <- function(subtype = "stock") {
  rlang::arg_match(subtype, c("stock", "construction", "renovation", "demolition"))
  folder <- "2025-02-26"
  read.csv(file.path(folder, paste0(subtype, ".cs4r")), comment.char = "#") %>%
    as.magpie(spatial = "reg", temporal = "ttot", datacol = "value")
}
