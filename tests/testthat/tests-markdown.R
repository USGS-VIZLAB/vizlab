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

test_that("markdown in viz converts to html", {
  viz <- as.viz(list(
    id = "md_inline",
    location = "data/example.md",
    reader = "md",
    mimetype = "text/markdown"
  ))
  viz <- as.reader(viz)

  fragment <- readData(viz)
  expect_match(fragment, "<h3>Simple markdown document</h3>")
  expect_match(fragment, "<em>auctor.*</em>")
  expect_match(fragment, "<strong>morbi.*</strong>")
  expect_match(fragment, "<pre><code>.*goes_here().*</code></pre>")
})
cleanup(oldwd, testtmp)
