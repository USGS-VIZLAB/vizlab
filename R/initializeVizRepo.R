#'
#' Initialize the GitHub repo for a new viz.
#' 
#' @description run this command to create a GitHub repository with 
#' appropriate issues, description, and README.
#' 
#' @param repo_name string, name for the new repository
#' @param description string, short description of the repository
#' @param org string, GitHub organization to create repository. Defaults to "USGS-VIZLAB"
#' @param issue_json file path indicating the JSON file to be used to define what issues to create
#' @param label_json file path indicating the JSON file to be used to define what labels to create
#' 
#' @return URL for the new GitHub repository.
#' @export
initializeVizRepo <- function(repo_name, description, org="USGS-VIZLAB",
                              issue_json=file.path("inst", "issuetemplates.json"),
                              label_json=file.path("inst", "labeltemplates.json")){
  
  # log in to GitHub & scope to read/write access for repos using profile.yaml
  # for additional scope info, see https://developer.github.com/v3/oauth/#scopes
  github_creds <- getProfileInfo()
  ctx <- interactive.login(client_id = github_creds$GITHUB_ID, 
                           client_secret = github_creds$GITHUB_AUTH,
                           scopes=c("repo"))
  
  new_repo <- createNewRepo(repo_name, description, org, ctx)
  new_labels <- createNewLabels(label_json, repo_name, org, ctx)
  new_issues <- createNewIssues(issue_json, repo_name, org, ctx)
  
  return(new_repo$content$url)
}

#' 
#' Create a new repository for a specified organization.
#' 
#' @param repo_name string, name for the new repository
#' @param description string, short description of the repository
#' @param org string, GitHub organization to create repository. Defaults to "USGS-VIZLAB"
#' @param ctx GitHub context for authentication, see \link[grithub]{get.github.context}
#' 
#' @importFrom grithub get.github.context
#' @importFrom grithub create.organization.repository
#' 
#' @export
createNewRepo <- function(repo_name, description, org="USGS-VIZLAB", ctx = get.github.context()){
  # check if this repo already exists
  if(repo_name %in% vizlab:::getRepoNames(org=org)){
    stop(paste0("The repo `", repo_name, "` already exists within ", org))
  }

  # create the new repository
  new_repo <- create.organization.repository(org=org, name=repo_name, description=description, ctx=ctx)
  
  # make sure that it worked
  stopifnot(repo_name %in% vizlab:::getRepoNames(org=org))
  
  return(new_repo)
}

#' 
#' Create new labels on a repository from a JSON file.
#' 
#' @param label_json file path indicating the JSON file to be used to define what labels to create
#' @param repo_name string, name for the new repository
#' @param org string, GitHub organization to create repository. Defaults to "USGS-VIZLAB"
#' @param ctx GitHub context for authentication, see \link[grithub]{get.github.context}
#' 
#' @importFrom grithub get.github.context
#' @importFrom grithub create.repository.label
#' 
#' @export
createNewLabels <- function(label_json, repo_name, org="USGS-VIZLAB", ctx = get.github.context()){
  # make sure that the repo exists
  stopifnot(repo_name %in% vizlab:::getRepoNames(org=org))
  
  label_content <- readLines(label_json)
  new_labels <- lapply(label_content, create.repository.label, owner=org, repo=repo_name, ctx=ctx)
  
  return(new_labels)
}

#' 
#' Create new issues on a repository from a JSON file.
#' 
#' @param issue_json file path indicating the JSON file to be used to define what issues to create
#' @param repo_name string, name for the new repository
#' @param org string, GitHub organization to create repository. Defaults to "USGS-VIZLAB"
#' @param ctx GitHub context for authentication, see \link[grithub]{get.github.context}
#' 
#' @importFrom grithub get.github.context
#' @importFrom grithub create.issue
#' 
#' @export
createNewIssues <- function(issue_json, repo_name, org="USGS-VIZLAB", ctx = get.github.context()){
  # make sure that the repo exists
  stopifnot(repo_name %in% vizlab:::getRepoNames(org=org))
  
  # make issues from the issue template JSON file
  issue_content <- readLines(issue_json)
  new_issues <- lapply(issue_content, create.issue, owner=org, repo=repo_name, ctx=ctx)
  
  return(new_issues)
}
