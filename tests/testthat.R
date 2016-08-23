library(testthat)
library(vizlab)

setup <- function() {
  testtmp <- file.path(tempdir(), "testtmp")
  dir.create(testtmp)
  setwd(testtmp)
  # need to copy any other files needed for testing
  file.copy(system.file('testviz/viz.yaml', package='vizlab'), testtmp)
  return(testtmp)
}

cleanup <- function(oldwd, testtmp) {
  setwd(oldwd)
  unlink(testtmp, recursive = TRUE)
  invisible()
}

test_check("vizlab")
