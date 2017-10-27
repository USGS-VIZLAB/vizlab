context("fetchTimestamps working")

oldwd <- getwd()

test_that(".url works", {
  testtmp <- setup(copyTestViz=TRUE)
  dir.create('vizlab/remake/timestamps', recursive=TRUE, showWarnings=FALSE)

  tsfile <- locateTimestampFile('foo')
  expect_false(file.exists(tsfile))
  
  # a site with no modified tag
  viz <- as.fetcher(as.viz(list(id="foo", remoteURL="www.google.com", fetcher="url")))
  expect_error(fetchTimestamp(viz), "'last-modified' field not found in headers")
  
  # a site with a last-modified tag
  viz <- as.fetcher(as.viz(list(id="foo", remoteURL="cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html", fetcher="url")))
  fetchTimestamp(viz)
  expect_is(as.POSIXct(readLines(tsfile), tz='UTC'), 'POSIXct')
  
  cleanup(oldwd, testtmp)
})

test_that("sciencebase works",{
  testtmp <- setup(copyTestViz=TRUE)
  dir.create('vizlab/remake/timestamps', recursive=TRUE, showWarnings=FALSE)
  
  viz.yaml <- yaml::yaml.load_file('viz.yaml')
  viz.yaml$fetch[[length(viz.yaml$fetch) + 1]] <- list(
    id='cuyahoga_sb',
    location='cache/fetch/cuyahoga_sb.csv',
    fetcher='sciencebase',
    remoteItemId='575d839ee4b04f417c2a03fe',
    remoteFilename='CuyahogaTDS.csv',
    mimetype='text/csv')
  writeLines(yaml::as.yaml(viz.yaml), 'viz.yaml')  
  
  tsfile <- locateTimestampFile('cuyahoga_sb')
  expect_false(file.exists(tsfile))
  
  # with no existing timestamp, fetchTimestamp should create a file
  fetchTimestamp('cuyahoga_sb')
  expect_true(file.exists(tsfile))
  ts1 <- readTimestamp('cuyahoga_sb')
  # check against the known timestamp for the cuyahoga_sb file
  expect_equal(ts1, as.POSIXct("2016-06-12 15:50:15", tz="UTC"))
  
  # fetch timestamp and expect the mtime and the ts contents to stay the same
  mt1 <- file.mtime(tsfile)
  fetchTimestamp('cuyahoga_sb')
  mt2 <- file.mtime(tsfile)
  expect_equal(mt1, mt2)
  ts2 <- readTimestamp('cuyahoga_sb')
  expect_equal(ts1, ts2)
  
  # but if oldtimestamp differs, fetchTimestamp should update the value
  writeTimestamp(Sys.time(), 'cuyahoga_sb')
  mt3 <- file.mtime(tsfile)
  ts3 <- readTimestamp('cuyahoga_sb')
  Sys.sleep(1)
  fetchTimestamp('cuyahoga_sb')
  mt4 <- file.mtime(tsfile)
  ts4 <- readTimestamp('cuyahoga_sb')
  expect_true(mt4 >= mt3)
  expect_true(ts4 <= ts3)
  
  cleanup(oldwd, testtmp)
})

test_that("unimplemented fetcher hits timestamp.fetcher", {
  viz <- list(id='trucks', fetcher='notaTSfetcher')
  attr(viz, "class") <- c("notaTSfetcher", "fetcher", "viz")
  expect_error(fetchTimestamp(viz), 'fetchTimestamp.*must be implemented')
})
cleanup(oldwd, testtmp)
