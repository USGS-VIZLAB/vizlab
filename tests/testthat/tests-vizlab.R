context("vizlab")

setup <- function() {
  testtmp <- file.path(tempdir(), "testtmp")
  dir.create(testtmp)
  setwd(testtmp)
  # need to copy any other files needed for testing
  #just copy everything? should need it eventually for more tests?
  #file.copy(system.file('testviz/viz.yaml', package='vizlab'), testtmp)
 file.copy(Sys.glob(paste0(system.file('testviz', package = 'vizlab'),"/*")),
           testtmp, recursive = TRUE)
  #create timestamp folder
  dir.create('vizlab/make/timestamps', recursive = TRUE)
  
  return(testtmp)
}

cleanup <- function(oldwd, testtmp) {
  setwd(oldwd)
  unlink(testtmp, recursive = TRUE)
  invisible()
}

oldwd <- getwd()
testtmp <- setup()

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
test_that(".file works", {
  #with no timestamp
  check <- fetchTimestamp('siteTextData')
  expect_true(check)
  #with matching timestamp
  ts <- file.info(file.path(testtmp,'data/siteText.yaml'))[['mtime']]
  writeTimestamp(ts, 'vizlab/make/timestamps/siteTextData')
  expect_false(fetchTimestamp('siteTextData'))
  #with a different timestamp
  writeTimestamp(as.POSIXct("1991-10-21 12:00:00"), 'vizlab/make/timestamps/siteTextData')
  expect_false(is.na(ts))
  expect_true(fetchTimestamp('siteTextData'))
})

test_that(".url works", {
  #a site with no modified tag
  viz <- list(id="foo", remoteURL="www.google.com")
  attr(viz , 'class') <- "url"
  expect_true(fetchTimestamp(viz))
})

test_that("sciencebase works",{
  #with no existing timestamp
  expect_true(fetchTimestamp('Cuyahoga'))
})

cleanup(oldwd, testtmp)
