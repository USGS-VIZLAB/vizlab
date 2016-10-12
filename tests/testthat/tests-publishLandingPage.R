context("publish viz landing page")

test_that("publishLandingPage works", {

  publishLandingPage()
  index <- readLines('landing/target/index.html')

  expect_true(any(grepl('/microplastics', index)))
  expect_true(any(grepl('/climate-change-walleye-bass', index)))
  expect_false(any(grepl('/example', index)))
  expect_false(any(grepl('href=\"./vizlab', index)))

})

cleanup(oldwd, setuptmp)
