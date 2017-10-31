context("remake")

oldwd <- getwd()
#these tests need to use the test viz
testtmp <- setup(copyTestViz=TRUE)

test_that('createRemakefile makes remake.yaml', {
  vizlab:::createRemakefile()
  # the file should get made
  expect_true(file.exists('remake.yaml'))
  
  # the file should contain typical top-level remake components
  expect_equal(
    grep('^([[:alpha:]].*):', readLines('remake.yaml'), value=TRUE), 
    c('target_default: Viz', 'packages:', 'sources:', "targets:"))
  
  # the file should contain typical vizlab concepts and phases
  expect_equal(
    grep('# --- (.*) --- #', readLines('remake.yaml'), value=TRUE), 
    paste0('  # --- ', c('parameter','fetch','process','visualize','publish','resource','scripts','Job groups'), ' --- #'))
})

test_that('prepSources can combine and subset scripts', {
  # can combine scripts
  prepSources('scripts/read/my_csv_reader.R', 'scripts/fetch/mayfly_nymph.R', outfile='vizlab/remake/scripts/prep1.R')
  prep1 <- readLines('vizlab/remake/scripts/prep1.R')
  expect_true(any(grepl('readData\\.my_csv_reader', prep1)))
  expect_true(any(grepl('fetch\\.mayfly_nymph', prep1)))
  expect_true(any(grepl('fetchTimestamp\\.mayfly_nymph', prep1)))
  
  # removes comments and whitespace
  expect_false(any(grepl('#', prep1)))
  expect_false(any(prep1 == ''))
  
  # can subset to specific functions
  prepSources('scripts/fetch/mayfly_nymph.R', functions='fetch.mayfly_nymph', outfile='vizlab/remake/scripts/prep2.R')
  prep2 <- readLines('vizlab/remake/scripts/prep2.R')
  expect_true(any(grepl('fetch\\.mayfly_nymph', prep2)))
  expect_false(any(grepl('fetchTimestamp\\.mayfly_nymph', prep2)))
})

test_that('vizmake creates makefile and runs make', {
  try(unlink('remake.yaml'), silent=TRUE)
  
  # the target should have been attempted, with warnings and errors and messages
  expect_message(regexp='Starting build at',
    expect_warning(vizmake('plot_info'), 'these packages are newer than required'))
  
  # the remake file should have been made
  expect_true(file.exists('remake.yaml'))
})

test_that('fetch item can now depend on process item', {
  # the meat of this test is in viz.yaml and fetch/cuyahoga.R, where
  # cuyahoga_diff is made to depend on process/cuyahoga_short. even though this
  # dependency bucks the general trend for data to flow from fetch to process to
  # beyond, it works! we know because there's no error here.
  
    # capture the warnings and messages
  expect_message(regexp='Starting build at',
                 expect_warning(regexp='these packages are newer than required',
                                vizmake('cuyahoga_diff')))
  cuyahoga_diff <- readData('cuyahoga_diff')
  expect_is(cuyahoga_diff, 'data.frame')
  expect_equal(nrow(cuyahoga_diff), nrow(readData('cuyahoga')) - nrow(readData('cuyahoga_short')))
})

cleanup(oldwd, testtmp)
