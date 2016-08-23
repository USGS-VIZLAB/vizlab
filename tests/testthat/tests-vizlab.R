context("vizlab")

oldwd <- getwd()
testtmp <- setup()

test_that("coercion to vizlab object works", {
  viz <- as.viz(list(
    id = "test1",
    location = "test/location.txt"
  ))
  expect_is(object = viz, class = "viz")
})

test_that("viz.yaml can be loaded", {
  viz <- as.viz("siteTextData")
  expect_true(!is.null(viz))
})

test_that("file fetcher has correct components", {
  viz <- as.viz("siteTextData")
  viz <- as.fetcher(viz)
  expect_is(object = viz, class = "file")
  expect_is(object = viz, class = "fetcher")

})

test_that("mimetype switch selects correct reader", {
  viz <- as.viz("siteTextData")
  viz <- as.fetcher(viz)
  viz <- as.reader(viz)
  expect_is(object = viz, class = "yaml")
})

cleanup(oldwd, testtmp)
