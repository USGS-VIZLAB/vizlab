oldwd <- getwd()
#these tests need to use the test viz
testtmp <- setup(copyTestViz=TRUE)
viz <- as.viz(list(
  depends = c("mayfly_nymph", "car_data","plot_info")
))
sourceScripts('scripts/read', verbose=FALSE)

context("readData")

test_that("readData works as expected", {

  mayfly <<- readData(viz[['depends']][1])
  car_data <<- readData(viz[['depends']][2])
  plot_data <<- readData(viz[['depends']][3])
  expect_is(mayfly, 'data.frame')
  expect_is(car_data, 'list')
  expect_is(car_data[[1]], 'data.frame')
  expect_is(car_data[[2]], 'data.frame')
  expect_is(plot_data, 'list')
  expect_true(all(names(plot_data) %in% c("width","height")) )
  
})

context("readDepends")
test_that("readDepends works as expected with viz input",{
  viz.dep <- readDepends(viz)
  expect_equal(mayfly, viz.dep[["mayfly_nymph"]])
  expect_equal(viz.dep[["car_data"]], car_data)
})

context("readDepends")
test_that("readDepends works as expected with parameters input",{
  viz.dep <- readDepends(viz)
  expect_equal(plot_data, viz.dep[["plot_info"]])
})

test_that("readDepends works as expected with list input",{
  viz.dep <- readDepends(list(
    depends = c("mayfly_nymph", "car_data")
  ))
  expect_equal(mayfly, viz.dep[["mayfly_nymph"]])
  expect_equal(viz.dep[["car_data"]], car_data)
})

cleanup(oldwd, testtmp)

