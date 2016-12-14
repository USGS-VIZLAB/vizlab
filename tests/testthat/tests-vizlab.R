context("testviz")

oldwd <- getwd()
#these tests need to use the test viz
testtmp <- setup(copyTestViz=TRUE)

test_that('timestamp folder actually created', {
  expect_true(file.exists('vizlab/make/timestamps'))
})

test_that("coercion to vizlab object works", {
  viz <- as.viz(list(
    id = "test1",
    location = "test/location.txt"
  ))
  expect_is(object = viz, class = "viz")
})

test_that("viz.yaml can be loaded", {
  viz <- as.viz("siteTextData")
  expect_true(!is.null(viz))
})

test_that("file fetcher has correct components", {
  viz <- as.viz("siteTextData")
  viz <- as.fetcher(viz)
  expect_is(object = viz, class = "file")
  expect_is(object = viz, class = "fetcher")

})

test_that("mimetype selects correct reader from default yaml", {
  viz <- as.viz("siteTextData")
  viz <- as.fetcher(viz)
  viz <- as.reader(viz)
  expect_is(object = viz, class = "yaml")
})

test_that("mimetype selects correct resource from default yaml", {
  viz <- as.viz("mainCSS")
  viz <- as.fetcher(viz)
  viz <- as.reader(viz)
  viz <- as.resource(viz)
  expect_is(object = viz, class = "css")
})

test_that("mimetype can select reader from user yaml", {
  reader <- lookupMimetype(mimetype = "text/fake")
  expect_true(reader == "fakeReader")
})

test_that("directories can be read", {
  viz <- as.viz("carData")
  viz <- as.fetcher(viz)
  viz <- as.reader(viz)
  viz_data <- readData(viz)
})

context("sciencebase")
test_that("sciencebase item has fields", {
  viz <- as.fetcher(as.viz("Cuyahoga"))
  expect_equal(viz[['remoteItemId']], "575d839ee4b04f417c2a03fe")
  expect_equal(viz[['remoteFilename']], "CuyahogaTDS.csv")
})

test_that("sciencebase validator works", {
  viz <- as.fetcher(list(
    id = "badsb",
    location = "cache/fetch/badsb.csv",
    fetcher = "sciencebase",
    mimetype = "text/csv"
  ))
  expect_error(fetch(viz), "missing")
})

context("templates")
test_that("getting template from library works", {
  viz <- as.viz(list(
    id = "test_section",
    template = "list",
    publisher = "section",
    context = list(
      listItems = list(
        "foo",
        "bar",
        "baz"
      )
    )
  ))
  viz <- as.publisher(viz)

  fragment <- publish(viz)
  expect_match(fragment, "<li>foo</li>")
  expect_match(fragment, "<li>bar</li>")
  expect_match(fragment, "<li>baz</li>")
})

context("fetchTimestamps working")

test_that(".url works", {
  #a site with no modified tag
  viz <- list(id="foo", remoteURL="www.google.com")
  attr(viz , 'class') <- "url"
  expect_true(fetchTimestamp(viz))
})

test_that("sciencebase works",{
  #with no existing timestamp
  expect_true(fetchTimestamp('Cuyahoga'))
  
  #write the timestamp for the Cuyahoga file
  writeTimestamp(as.POSIXct("2016-06-12 15:50:15 CDT"), "vizlab/make/timestamps/Cuyahoga")
  expect_false(fetchTimestamp('Cuyahoga'))
})

test_that("custom fetcher hits timestamp.fetcher", {
  viz <- list()
  attr(viz, "class") <- c("cars", "fetcher", "viz")
  expect_warning(fetchTimestamp(viz))
})

test_that("makeFiles created", {
  createProfile(directory = "./vizlab")
  createMakefiles()
  expect_true(file.exists('vizlab/make/fetch.make'))
  expect_true(file.exists('vizlab/make/process.make'))
  expect_true(file.exists('vizlab/make/visualize.make'))
  expect_true(file.exists('vizlab/make/publish.make'))
})

context("publishers")
test_that("publish footer works", {
  output <- publish('footer')
  expect_true(any(grepl('microplastics', output)))
  expect_true(any(grepl('https://owi.usgs.gov/blog/stats-service-map/', output)))
  expect_true(any(grepl('climate-change-walleye-bass', output)))
  expect_true(any(grepl('blog|Blogs', output)))
  
  #without blogs
  fakeViz <- list(id="footer", publisher="footer", template = "footer", blogsInFooter=FALSE,
                  vizzies=list(list(name = "Microplastics in the Great Lakes", org="USGS-VIZLAB",
                                    repo = "great-lakes-microplastics")))
  output <- publish.footer(fakeViz)
  expect_true(any(grepl('microplastics', output)))
  expect_false(any(grepl('blog|Blogs', output)))
})

cleanup(oldwd, testtmp)
