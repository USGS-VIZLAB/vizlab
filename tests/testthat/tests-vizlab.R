context("testviz")

oldwd <- getwd()
#these tests need to use the test viz
testtmp <- setup(copyTestViz=TRUE)

test_that("coercion to vizlab object works", {
  viz <- as.viz(list(
    id = "test1",
    location = "test/location.txt"
  ))
  expect_is(object = viz, class = "viz")
})

test_that("viz.yaml can be loaded", {
  viz <- as.viz("site_text_data")
  expect_true(!is.null(viz))
})

test_that("file fetcher has correct components", {
  viz <- as.viz("site_text_data")
  viz <- as.fetcher(viz)
  expect_is(object = viz, class = "file")
  expect_is(object = viz, class = "fetcher")

})

test_that("parameter has correct components", {
  viz <- as.viz("plot_info")
  viz <- as.parameter(viz)
  expect_is(object = viz, class = "parameter")
  
})

test_that("mimetype selects correct reader from default yaml", {
  viz <- as.viz("site_text_data")
  viz <- as.fetcher(viz)
  viz <- as.reader(viz)
  expect_is(object = viz, class = "yaml")
})

test_that("mimetype selects correct resource from default yaml", {
  viz <- as.viz("mainCSS")
  viz <- as.fetcher(viz)
  viz <- as.reader(viz)
  viz <- as.resource(viz)
  expect_is(object = viz, class = "css")
})

test_that("mimetype can select reader from user yaml", {
  reader <- lookupMimetype(mimetype = "text/fake")
  expect_true(reader == "fakeReader")
})

test_that("directories can be read", {
  viz <- as.viz("car_data")
  viz <- as.fetcher(viz)
  viz <- as.reader(viz)
  viz_data <- readData(viz)
})

test_that("watermark fetcher works", {
  viz <- as.viz("usgs_watermark")
  viz <- as.fetcher(viz)
  viz_watermark <- fetch(viz)
})

context("sciencebase")
test_that("sciencebase validator works", {
  viz <- as.fetcher(list(
    id = "badsb",
    location = "cache/fetch/badsb.csv",
    fetcher = "sciencebase",
    mimetype = "text/csv"
  ))
  expect_error(fetch(viz), "missing")
})

context("templates")
test_that("getting template from library works", {
  viz <- as.viz(list(
    id = "test_section",
    template = "list",
    publisher = "section",
    context = list(
      listItems = list(
        "foo",
        "bar",
        "baz"
      )
    )
  ))
  viz <- as.publisher(viz)

  fragment <- publish(viz)
  expect_match(fragment, "<li>foo</li>")
  expect_match(fragment, "<li>bar</li>")
  expect_match(fragment, "<li>baz</li>")
})

cleanup(oldwd, testtmp)
