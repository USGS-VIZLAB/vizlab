context("checkVizPackages")

oldwd <- getwd()
#these tests need to use the test viz
testtmp <- setup(copyTestViz=TRUE)

test_that('checkVizPackages catches missing packages', {
  # read in the viz.yaml
  viz.yaml <- yaml::yaml.load_file('viz.yaml')
  shh <- function(...){}
  
  # modify the viz.yaml to include a missing package
  vy <- viz.yaml
  vy$info$`required-packages`$crazy2 <- list(repo='CRAN', version='20.11.45')
  writeLines(yaml::as.yaml(vy), 'viz.yaml')
  expect_error(checkVizPackages(newer=shh, older=shh), "these packages are absent")
  
  # modify the viz.yaml to include an outdated package
  vy <- viz.yaml
  vy$info$`required-packages`$yaml <- list(repo='CRAN', version='1000.0.0')
  writeLines(yaml::as.yaml(vy), 'viz.yaml')
  expect_error(checkVizPackages(newer=shh), "these packages are older than required")
  expect_warning(checkVizPackages(newer=shh, older=warning), "these packages are older than required")
  
  # modify the viz.yaml to include an excessively new package
  vy <- viz.yaml
  vy$info$`required-packages`$yaml <- list(repo='CRAN', version='0.0.1')
  writeLines(yaml::as.yaml(vy), 'viz.yaml')
  expect_warning(checkVizPackages(), "these packages are newer than required")
  expect_error(checkVizPackages(newer=stop), "these packages are newer than required")
})

cleanup(oldwd, testtmp)
