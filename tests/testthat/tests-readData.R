

oldwd <- getwd()
#these tests need to use the test viz
testtmp <- setup(copyTestViz=TRUE)
viz <- as.viz(list(
  depends = c("MayflyNymph", "carData")
))

context("readData")

test_that("readData works as expected", {

  mayfly <<- readData(viz[['depends']][1])
  carData <<- readData(viz[['depends']][2])
  expect_is(mayfly, 'data.frame')
  expect_is(carData, 'list')
  expect_is(carData[[1]], 'data.frame')
  expect_is(carData[[2]], 'data.frame')
})

test_that("inline depends can contain a value", {
  data <- readData("constant")
  expect_equal(data, 42)
})

test_that("inline depends can contain list", {
  data <- readData("constant-list")
  expect_equal(data$color, "#ffffff")
  expect_equal(data$value, 45.5)
  expect_match(data$text, ".*quick.*fox.*lazy.*")
})

context("readDepends")
test_that("readDepends works as expected with viz input",{
  viz.dep <- readDepends(viz)
  expect_equal(mayfly, viz.dep[["MayflyNymph"]])
  expect_equal(viz.dep[["carData"]], carData)
})


test_that("readDepends works as expected with list input",{
  viz.dep <- readDepends(list(
    depends = c("MayflyNymph", "carData")
  ))
  expect_equal(mayfly, viz.dep[["MayflyNymph"]])
  expect_equal(viz.dep[["carData"]], carData)
})

cleanup(oldwd, testtmp)
