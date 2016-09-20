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
