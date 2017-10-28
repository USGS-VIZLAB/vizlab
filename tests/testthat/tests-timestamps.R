context("fetchTimestamps working")

oldwd <- getwd()

#### flow with remake ####

testtmp <- setup(copyTestViz=TRUE)

test_that("alwaysCurrent doesn't get rebuilt unless missing", {
  # confirm that mayfly_nymph is an alwaysCurrent item
  source('scripts/fetch/mayfly_nymph.R')
  expect_equal(fetchTimestamp.mayfly_nymph, alwaysCurrent)
  
  # first time: fetches
  suppressWarnings(expect_message(vizmake('mayfly_nymph'), 'fetching mayfly_nymph'))
  
  # after that: doesn't fetch
  suppressWarnings(expect_message(vizmake('mayfly_nymph'), '\\[ ----- \\] mayfly_nymph'))
})

test_that("locally outdated always gets built", {
  # TODO this won't work until we resolve #287
  # unless an argument changes
  # viz.yaml <- readLines('viz.yaml')
  # viz.yaml <- gsub('info: insect', 'info: aquatic insect', viz.yaml)
  # writeLines(viz.yaml, 'viz.yaml')
  # suppressWarnings(expect_message(vizmake('mayfly_nymph'), 'a mayfly is a kind of aquatic insect'))
  
  # 
})

test_that("before time to live, even neverCurrent doesn't get rebuilt", {
  # first time: fetches
  suppressWarnings(expect_message(vizmake('never_current'), '[ BUILD ] cache/fetch/never_current.txt', fixed=TRUE))
  suppressWarnings(expect_message(vizmake('cuyahoga'), '[ BUILD ] cache/fetch/cuyahoga.csv', fixed=TRUE))
  
  # put these fetch items within their timetolive intervals
  writeLines(yaml::as.yaml(list(timetolive=list(never_current='5 hours', cuyahoga='5 hours'))), 'preferences.yaml')
  
  # immediately after: doesn't fetch
  suppressWarnings(expect_message(vizmake('never_current'), '[    OK ] cache/fetch/never_current.txt', fixed=TRUE))
  suppressWarnings(expect_message(vizmake('cuyahoga'), '[    OK ] cache/fetch/cuyahoga.csv', fixed=TRUE))
})

test_that("after time to live, timestamp gets checked", {
  # collect the old timestamps
  nc_ts1 <- readTimestamp('never_current')
  cy_ts1 <- readTimestamp('cuyahoga')
  
  # make sure ttl has expired
  writeLines(yaml::as.yaml(list(timetolive=list(never_current='0 secs', cuyahoga='0 secs'))), 'preferences.yaml')
  
  # never-current should have its timestamp refetched, and the new timestamp
  # should be different (iff we've waited at least 1 second)
  Sys.sleep(0.02)
  suppressWarnings(expect_message(vizmake('never_current'), '[ BUILD ] vizlab/remake/timestamps/never_current.txt', fixed=TRUE))
  expect_gt(as.numeric(readTimestamp('never_current')), as.numeric(nc_ts1))
  # after ttl expires, neverCurrent always fetches, even right (>1 sec) after it just fetched
  Sys.sleep(0.02)
  suppressWarnings(expect_message(vizmake('never_current'), '[ BUILD ] cache/fetch/never_current.txt', fixed=TRUE))
  
  # sometimes-current should have its timestamps refetched. here the new
  # timestamp should be the same (because of how we defined
  # fetchTimestamp.cuyahoga), so cuyahoga isn't refetched
  suppressWarnings(expect_message(vizmake('cuyahoga'), '[ BUILD ] vizlab/remake/timestamps/cuyahoga.txt', fixed=TRUE))
  expect_equal(readTimestamp('cuyahoga'), cy_ts1)
  suppressWarnings(expect_message(vizmake('cuyahoga'), '[    OK ] cache/fetch/cuyahoga.csv', fixed=TRUE))
  
  # if we change the remote timestamp for cuyahoga, the new timestamp should be
  # different and the item should get refetched
  Sys.setFileTime('data/pretend_remote/cuyahoga.csv', cy_ts1 + as.difftime(2, units='secs'))
  suppressWarnings(expect_message(vizmake('cuyahoga'), '[ BUILD ] cache/fetch/cuyahoga.csv', fixed=TRUE))
  expect_gt(as.numeric(readTimestamp('cuyahoga')), as.numeric(cy_ts1))
})

cleanup(oldwd, testtmp)

#### timestamp fetchers ####

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
