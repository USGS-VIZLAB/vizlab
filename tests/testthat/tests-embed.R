context("embed")

test_that("embed publishes", {
  oldwd <- getwd()
  tmpdir <- setup(TRUE)
  embed <- list(
    id = "test",
    publisher = "section",
    template = "brandcomment",
    embed = TRUE
  )
  publish(embed)
  expect_true(file.exists("target/embed/test.html"))
  cleanup(oldwd, tmpdir)
})

