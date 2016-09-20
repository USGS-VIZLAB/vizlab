context("analytics")

test_that("scroll class is added to output", {
  viz <- as.viz(list(
    id = "scroll-test",
    location = "test.html",
    output = "<div id=\"scroll-test\"><a href=\"/test\"></a></div>",
    publisher = "section",
    analytics = "scroll"
  ))
  viz <- analytics(viz)
  expect_match(viz[['output']], "class=\"vizScroll\"")
})

test_that("click class is added to output", {
  viz <- as.viz(list(
    id = "click-test",
    location = "test.html",
    output = "<div id=\"click-test\" class=\"hidden\"><a href=\"/test\"></a></div>",
    publisher = "section",
    analytics = "click"
  ))
  viz <- analytics(viz)
  expect_match(viz[['output']], "class=\"hidden vizClick\"")
})
