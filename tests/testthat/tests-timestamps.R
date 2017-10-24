context("fetchTimestamps working")

oldwd <- getwd()
testtmp <- setup(copyTestViz=TRUE)
dir.create('vizlab/remake/timestamps', recursive=TRUE, showWarnings=FALSE)

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
  expect_error(fetchTimestamp(viz), 'fetchTimestamp.*must be implemented')
})

cleanup(oldwd, testtmp)
