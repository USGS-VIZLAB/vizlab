context("util")

test_that("replaceOrAppend with empty left is equivilent to right", {
  x <- list()
  y <- list(a = 1, b = list(2, 3))
  z <- vizlab:::replaceOrAppend(x, y)
  expect_equal(y, z)
})

test_that("replaceOrAppend replace works", {
  x <- list(a = 1)
  y <- list(a = 7)
  z <- vizlab:::replaceOrAppend(x, y)
  expect_equal(z$a, 7)
})

test_that("replaceOrAppend retain works", {
  x <- list(a = 1)
  y <- list()
  z <- vizlab:::replaceOrAppend(x, y)
  expect_equal(z$a, 1)
})

test_that("replaceOrAppend append works", {
  x <- list(a = list(1,2,3))
  y <- list(a = 4)
  z <- vizlab:::replaceOrAppend(x, y)
  expect_equal(z$a, list(1,2,3,4))
})
