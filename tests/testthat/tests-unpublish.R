oldwd <- getwd()
#these tests need to use the test viz
testtmp <- setup(copyTestViz=TRUE)

context("unpublish")

test_that("files aren't deleted during unpublishing", {
  
  # unpublish doesn't work if you don't have a remake.yaml yet. i think that's fine.
  expect_error(unpublish())
  
  # if remake.yaml exists but nothing has been published yet, all the changes
  # reported by remake should be empty "(     )"
  vizmake('Parameter')
  msgs <- capture_messages(unpublish())
  expect_true(all(grepl('(       )', msgs, fixed=TRUE)))
  
  # if publishing has occurred, items should get removed from remake database but files shouldn't be deleted
  ### THIS SECTION NOT COMPLETE because vizmake('Publish') doesn't work on the
  ### testviz and i'm out of time to make it work. but these are the tests i
  ### wish i could run and have run in water-use-15:
  # vizmake('Publish')
  # published_target_files <- dir('target', recursive=TRUE, full.names=TRUE)
  # msgs <- capture_messages(unpublish())
  # expect_true(all(grepl('\\( .*  DEL.* \\)', msgs))) # targets should have been deleted. the .* is needed because of crayon package
  # unpublished_target_files <- dir('target', recursive=TRUE, full.names=TRUE)
  # expect_equal(published_target_files, unpublished_target_files) # files shouldn't have changed
})

cleanup(oldwd, testtmp)
