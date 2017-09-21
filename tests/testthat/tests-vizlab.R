context("testviz")

oldwd <- getwd()
#these tests need to use the test viz
testtmp <- setup(copyTestViz=TRUE)

test_that('timestamp folder actually created', {
  expect_true(file.exists('vizlab/make/timestamps'))
})

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

test_that("parameter has correct components", {
  viz <- as.viz("plot-info")
  viz <- as.parameter(viz)
  expect_is(object = viz, class = "parameter")
  
})

test_that("mimetype selects correct reader from default yaml", {
  viz <- as.viz("siteTextData")
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
  viz <- as.viz("carData")
  viz <- as.fetcher(viz)
  viz <- as.reader(viz)
  viz_data <- readData(viz)
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

context("fetchTimestamps working")

test_that(".url works", {
  tsfile <- locateTimestampFile('foo')
  expect_false(file.exists(tsfile))
  
  # a site with no modified tag
  viz <- as.fetcher(as.viz(list(id="foo", remoteURL="www.google.com", fetcher="url")))
  expect_error(fetchTimestamp(viz), "'last-modified' field not found in headers")
  
  # a site with a last-modified tag
  viz <- as.fetcher(as.viz(list(id="foo", remoteURL="cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html", fetcher="url")))
  fetchTimestamp(viz)
  expect_is(as.POSIXct(readLines(tsfile), tz='UTC'), 'POSIXct')
})

test_that("sciencebase works",{
  tsfile <- locateTimestampFile('Cuyahoga')
  expect_false(file.exists(tsfile))
  
  # with no existing timestamp, fetchTimestamp should create a file
  fetchTimestamp('Cuyahoga')
  expect_true(file.exists(tsfile))
  ts1 <- readTimestamp('Cuyahoga')
  # check against the known timestamp for the Cuyahoga file
  expect_equal(ts1, as.POSIXct("2016-06-12 15:50:15", tz="UTC"))
  
  # fetch timestamp and expect the mtime and the ts contents to stay the same
  mt1 <- file.mtime(tsfile)
  fetchTimestamp('Cuyahoga')
  mt2 <- file.mtime(tsfile)
  expect_equal(mt1, mt2)
  ts2 <- readTimestamp('Cuyahoga')
  expect_equal(ts1, ts2)
  
  # but if oldtimestamp differs, fetchTimestamp should update the value
  writeTimestamp(Sys.time(), 'Cuyahoga')
  mt3 <- file.mtime(tsfile)
  ts3 <- readTimestamp('Cuyahoga')
  Sys.sleep(1)
  fetchTimestamp('Cuyahoga')
  mt4 <- file.mtime(tsfile)
  ts4 <- readTimestamp('Cuyahoga')
  expect_true(mt4 >= mt3)
  expect_true(ts4 <= ts3)
})

test_that("unimplemented fetcher hits timestamp.fetcher", {
  viz <- list(id='trucks', fetcher='notaTSfetcher')
  attr(viz, "class") <- c("notaTSfetcher", "fetcher", "viz")
  expect_error(fetchTimestamp(viz))
})

test_that("makeFiles created", {
  createProfile(directory = "./vizlab")
  createMakefiles()
  expect_true(file.exists('vizlab/make/fetch.make'))
  expect_true(file.exists('vizlab/make/process.make'))
  expect_true(file.exists('vizlab/make/visualize.make'))
  expect_true(file.exists('vizlab/make/publish.make'))
})

cleanup(oldwd, testtmp)
