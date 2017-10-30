
oldwd <- getwd()
#these tests need to use the test viz
testtmp <- setup(copyTestViz=TRUE)

context("publish")

test_that("googlefont publisher is dispatched to", {
  viz <- list(
    id="font",
    family="Open Sans",
    weight=c(400,700),
    publisher="googlefont"
  )
  viz <- as.viz(viz)
  viz <- as.publisher(viz)
  fontcode <- publish(viz)
  expect_match(fontcode, ".*googleapis.*Open%20Sans.*400.*700")
})

test_that("publish footer works", {
  output <- publish('footer')
  expect_true(any(grepl('microplastics', output)))
  expect_true(any(grepl('https://owi.usgs.gov/blog/stats-service-map/', output)))
  expect_true(any(grepl('climate-change-walleye-bass', output)))
  expect_true(any(grepl('blog|Blogs', output)))

  #without blogs
  fakeViz <- list(id="footer", publisher="footer", template = "footer-template", blogsInFooter=FALSE,
                  vizzies=list(list(name = "Microplastics in the Great Lakes", org="USGS-VIZLAB",
                                    repo = "great-lakes-microplastics")))
  output <- publish(fakeViz)
  expect_true(any(grepl('microplastics', output)))
  expect_false(any(grepl('blog|Blogs', output)))
})
test_that("Thumb publisher works", {
  expect_error(publish("twitter-thumb")) #incorrect dimensions
  publish('landing-thumb')
  expect_true(file.exists('target/images/landing-thumb.png'))
})

test_that("URL utils", {
  expect_equal(getVizURL(), "https://owi.usgs.gov/vizlab/notsure")
  expect_equal(pastePaths("bar", "baz"), "bar/baz")
  expect_equal(pastePaths("bar/", "baz"), "bar/baz")
})

test_that("Image publisher works with classes", {
  viz <- list(
          id = "img-w-class",
          location = "test.png",
          mimetype = "image/png",
          title = "testtitle",
          alttext = "testalt",
          class = c("figure", "big"),
          relpath = "images/test.png")
  viz <- as.viz(viz)
  viz <- as.publisher(viz)

  with_mock(
    `vizlab::publish.resource` = function(viz) viz,
    expect_match(publish(viz), "<img src=\"images/test\\.png.*alt=\"testalt\" title=\"testtitle\" class=\"figure big\" />")
  )
})

test_that("Image publisher works without classes", {
  viz <- list(
          id = "img-w-class",
          location = "test.png",
          mimetype = "image/png",
          title = "testtitle",
          alttext = "testalt",
          relpath = "images/test.png")
  viz <- as.viz(viz)
  viz <- as.publisher(viz)

  with_mock(
    `vizlab::publish.resource` = function(viz) viz,
    expect_match(publish(viz), ".*images/test\\.png.*alt=\"testalt\" title=\"testtitle\" \\/>")
  )
})

cleanup(oldwd, testtmp)
