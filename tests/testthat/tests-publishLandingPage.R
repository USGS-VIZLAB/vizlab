context("publish viz landing page")

oldwd <- getwd()
setuptmp <- setup()

test_that("publishLandingPage works", {
  mock.get.repository.path <- function(org, repo, file) {
    list(ok=TRUE, content=list(html_url=paste0('https://github.com/', org, '/', repo, '/blob/master/', file)))
  }
  with_mock(
    `grithub::get.repository.path` = mock.get.repository.path, 
    `vizlab:::getRepoNames` = function(org) { c("example", "great-lakes-microplastics", "climate-fish-habitat", "vizlab") },
    publishLandingPage()
  )
  index <- readLines('landing/target/index.html', warn = FALSE)

  expect_true(any(grepl('microplastics', index)))
  expect_true(any(grepl('climate-change-walleye-bass', index)))
  expect_false(any(grepl('example', index)))
  expect_false(any(grepl('href=\"./vizlab', index)))
})

cleanup(oldwd, setuptmp)
