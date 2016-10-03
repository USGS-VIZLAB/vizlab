context("publish viz landing page")

test_that("publishLandingPage works", {
  
  landing_path <- publishLandingPage(index_loc = tempdir())
  index <- readLines(landing_path)
  
  expect_true(any(grepl('USGS-VIZLAB/great-lakes-microplastics', index)))
  expect_true(any(grepl('USGS-VIZLAB/climate-fish-habitat', index)))
  expect_false(any(grepl('USGS-VIZLAB/example', index)))
  expect_false(any(grepl('USGS-VIZLAB/vizlab', index)))
  
})
