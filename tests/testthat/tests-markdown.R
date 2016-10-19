context("markdown")

oldwd <- getwd()
testtmp <- setup(TRUE)
test_that("markdown in context becomes proper html", {
  viz <- as.viz(list(
    id = "test_markdown",
    location = "data/siteText.yaml",
    reader = "md",
    mimetype = "text/yaml"
  ))
  viz <- as.reader(viz)

  fragment <- readData(viz)
  expect_match(fragment$header, "<h1>HEADER</h1>")
  expect_match(fragment$link, ">link</a>")
  expect_match(fragment$nested$keys$with$markdown, ">link</a>")
  expect_match(fragment$list, "<li>bullets</li>")
  expect_match(fragment$list, "<li>or numbered</li>")

})
cleanup(oldwd, testtmp)
