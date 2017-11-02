oldwd <- getwd()
#these tests need to use the test viz
testtmp <- setup(copyTestViz=TRUE)
viz <- as.viz(list(
  depends = c("mayfly_nymph", "car_data","plot_info", "example_svg", "railroad_stations")
))
sourceScripts('scripts/read', verbose=FALSE)

context("readData")

test_that("readData works as expected", {
  mayfly <<- readData('mayfly_nymph')
  car_data <<- readData('car_data')
  plot_data <<- readData('plot_info')
  svg_example <- readData('example_svg')
  rail_stations <- readData('railroad_stations')
  expect_is(mayfly, 'data.frame')
  expect_is(car_data, 'list')
  expect_is(car_data[[1]], 'data.frame')
  expect_is(car_data[[2]], 'data.frame')
  expect_is(plot_data, 'list')
  expect_is(svg_example, c('xml_document', 'xml_node'))
  expect_is(rail_stations, c('SpatialPointsDataFrame', 'sp'))
  expect_true(all(names(plot_data) %in% c("width","height")) )
})

context("readDepends")

test_that("readDepends works as expected with viz input",{
  viz.dep <- readDepends(viz)
  expect_equal(mayfly, viz.dep[["mayfly_nymph"]])
  expect_equal(viz.dep[["car_data"]], car_data)
})

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

