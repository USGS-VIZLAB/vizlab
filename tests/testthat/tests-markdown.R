context("markdown")
test_that("markdown in context becomes proper html", {
  viz <- as.viz(list(
    id = "test_markdown",
    template = "printall",
    publisher = "section",
    context = list(
      text = "# HEADER\
Paragraph of text with [link](https://owi.usgs.gov/ \"OWI\") followed by list\
* list item one\
* list item two"
    )
  ))
  viz <- as.publisher(viz)

  fragment <- publish(viz)
  expect_match(fragment, "<h1>HEADER</h1>")
  expect_match(fragment, ">link</a>")
  expect_match(fragment, "<li>list item one</li>")
})
