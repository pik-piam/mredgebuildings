test_that("refMaps have unambiguous color assignment", {
  pattern <- "^refMap_(.+)\\.csv$"
  folder <- piamutils::getSystemFile("extdata", "sectoral", package = "mredgebuildings")
  files <- list.files(folder, pattern)
  names(files) <- sub(pattern, "\\1", files)
  refMaps <- lapply(files, function(file) utils::read.csv(file.path(folder, file)))
  refColorsOk <- lapply(refMaps, function(map) {
    if (".color" %in% names(map)) {
      varColMap <- unique(map[, c("variable", ".color")])
      !any(duplicated(varColMap$variable))
    } else {
      TRUE
    }
  })
  for (ref in names(refColorsOk)) {
    expect_true(refColorsOk[[!!ref]])
  }
})
