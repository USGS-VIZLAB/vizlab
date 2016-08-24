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

context("sciencebase")
test_that("sciencebase item has fields", {
  viz <- as.fetcher(as.viz("Cuyahoga"))
  expect_equal(viz[['remoteItemId']], "575d839ee4b04f417c2a03fe")
  expect_equal(viz[['remoteFilename']], "CuyahogaTDS.csv")
})

test_that("sciencebase validator works", {
  viz <- as.fetcher(list(
    id = "badsb",
    location = "cache/fetch/badsb.csv",
    fetcher = "sciencebase",
    mimetype = "text/csv"
  ))
  expect_error(fetch(viz), "missing")
})

cleanup(oldwd, testtmp)
