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
  prepSources('scripts/read/myCsvReader.R', 'scripts/fetch/mayflyNymph.R', outfile='vizlab/remake/scripts/prep1.R')
  prep1 <- readLines('vizlab/remake/scripts/prep1.R')
  expect_true(any(grepl('readData\\.myCsvReader', prep1)))
  expect_true(any(grepl('fetch\\.mayflyNymph', prep1)))
  expect_true(any(grepl('fetchTimestamp\\.mayflyNymph', prep1)))
  
  # removes comments and whitespace
  expect_false(any(grepl('#', prep1)))
  expect_false(any(prep1 == ''))
  
  # can subset to specific functions
  prepSources('scripts/fetch/mayflyNymph.R', functions='fetch.mayflyNymph', outfile='vizlab/remake/scripts/prep2.R')
  prep2 <- readLines('vizlab/remake/scripts/prep2.R')
  expect_true(any(grepl('fetch\\.mayflyNymph', prep2)))
  expect_false(any(grepl('fetchTimestamp\\.mayflyNymph', prep2)))
})

test_that('vizmake creates makefile and runs make', {
  unlink('remake.yaml')
  
  # the target should have been attempted, with warnings and errors and messages
  expect_error(regexp="Some packages are missing: wateRuse, crazy",
    expect_message(regexp="load",
      expect_message(regexp='Starting build at',
        expect_warning(vizmake('plot-info')))))
  
  # the remake file should have been made
  expect_true(file.exists('remake.yaml'))
})

cleanup(oldwd, testtmp)
