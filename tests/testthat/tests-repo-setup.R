# 
# context("repository setup, currently commented out")
# 
# test_repo_name <- "test123"
# 
# # can't have tests automatically run because of authentication + we don't
# # want to create and delete a GitHub repo every time the package tests are run
# 
# github_creds <- getProfileInfo()
# ctx <- grithub::interactive.login(client_id = github_creds$GITHUB_ID, 
#                                   client_secret = github_creds$GITHUB_AUTH,
#                                   scopes=c("repo", "delete_repo"))
# 
# test_that("repo will not be created if it already exists", {
#   expect_error(createNewRepo(repo_name="vizlab"), 
#                "The repo `vizlab` already exists within USGS-VIZLAB")
# })
# 
# test_that("repo will be created if it does not exist", {
#   testrepo <- createNewRepo(repo_name=test_repo_name, description="my test viz", ctx=ctx)
#   expect_true(testrepo[['ok']])
#   expect_equal(paste0('https://api.github.com/repos/USGS-VIZLAB/', test_repo_name), 
#                testrepo[['content']][['url']])
#   expect_true(test_repo_name %in% vizlab:::getRepoNames(org="USGS-VIZLAB"))
# })
# 
# test_that("new labels can be created", {
#   testlabels <- createNewLabels(label_json=system.file("github/labeltemplates.json", package="vizlab"), 
#                                 repo_name=test_repo_name, ctx=ctx)
#   allrepolabels <- grithub::get.repository.labels(owner="USGS-VIZLAB", test_repo_name, ctx=ctx)
#   expect_true(testlabels[[1]][['ok']])
#   expect_true("outreach" %in% unlist(lapply(allrepolabels[['content']], '[[', "name")))
# })
# 
# test_that("new issues can be created", {
#   testissues <- createNewIssues(issue_json=system.file("github/issuetemplates.json", package="vizlab"), 
#                                 repo_name=test_repo_name, ctx=ctx)
#   allrepoissues<- grithub::get.repository.issues(owner="USGS-VIZLAB", test_repo_name, ctx=ctx)
#   expect_true(testissues[[1]][['ok']])
#   expect_true("Social media" %in% unlist(lapply(allrepoissues[['content']], '[[', "title")))
# })
# 
# # cleanup by deleting new repo
# delete_repo <- grithub::delete.repository(owner="USGS-VIZLAB", repo=test_repo_name, ctx=ctx)
