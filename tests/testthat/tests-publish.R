context("publish")

test_that("googlefont publisher is dispatched to", {
  viz <- list(
    id="font",
    family="Open Sans",
    weight=c(400,700),
    publisher="googlefont"
  )
  viz <- as.viz(viz)
  viz <- as.publisher(viz)
  fontcode <- publish(viz)
  expect_match(fontcode, ".*googleapis.*Open%20Sans.*400.*700")
})