context("viz-setup")

cleanup <- function(oldwd, setuptmp) {
  setwd(oldwd)
  unlink(setuptmp, recursive = TRUE)
  invisible()
}

oldwd <- getwd()
testtmp <- file.path(tempdir(), "testtmp")
dir.create(testtmp)
setwd(testtmp)

vizSkeleton(name="my viz")

test_that('top level folders created', {
  expected_toplevel_folders <- c('data', 'figures', 'images', 'layout', 'scripts')
  toplevel_folders <- list.files(all.files=TRUE)
  
  expect_true(all(expected_toplevel_folders %in% toplevel_folders))
})

test_that('toplevel files are created', {
  expected_toplevel_files <- c('.gitignore', 'LICENSE', 'viz.yaml')
  toplevel_files <- list.files(all.files=TRUE)
  
  expect_true(all(expected_toplevel_files %in% toplevel_files))
})

test_that('script subdirectories created', {
  expected_scripts_folders <- c('fetch', 'process', 'read', 'visualize')
  scripts_folders <- list.files('scripts', all.files=TRUE)
  
  expect_true(all(expected_scripts_folders %in% scripts_folders))
})

test_that('layout subdirectories created', {
  expected_layout_folders <- c('css', 'js', 'templates')
  layout_folders <- list.files('layout', all.files=TRUE)
  
  expect_true(all(expected_layout_folders %in% layout_folders))
})

test_that('.empty files created', {
  expect_true(file.exists('data/.empty'))
  expect_true(file.exists('figures/.empty'))
  expect_true(file.exists('images/.empty'))
  expect_false(file.exists('.empty')) #not created in home dir
})

test_that('createProfile works for relative dir', {
  expect_false(file.exists('./vizlab/profile.yaml'))
  createProfile('relative')
  expect_true(file.exists('./vizlab/profile.yaml'))
})

cleanup(oldwd, setuptmp)
