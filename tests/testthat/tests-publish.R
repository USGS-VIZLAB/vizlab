context("publish viz landing page")

test_that("publishLandingPage works", {
  
  publishLandingPage()
  index <- readLines('landing/target/index.html')
  
  expect_true(any(grepl('/microplastics', index)))
  expect_true(any(grepl('/climate-change-walleye-bass', index)))
  expect_false(any(grepl('/example', index)))
  expect_false(any(grepl('href=\"./vizlab', index)))
})

context("publish footer")
test_that("publish.footer works", {
  #fake viz
  viz <- list(publisher="footer", template="footer", depends="footer-style",
              blogsInFooter=FALSE,
              vizzies=list(list(name="Microplastics in the Great Lakes", org="USGS-VIZLAB", repo="great-lakes-microplastics")))
  output <- publish(viz)
  #no blogs
  
  #both
})