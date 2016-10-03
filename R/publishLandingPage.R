#' Get information about other visualizations from VIZLAB to generate the landing page
#' 
#' note for future: github package is from GitHub repo rgithub (https://github.com/cscheid/rgithub)
#' there are plans to move the package to CRAN and call it grithub (https://github.com/cscheid/rgithub/issues/67)
#' @param org character, name of GitHub organization in which to pull out repository names
#' @export
publishLandingPage <- function(org = "USGS-VIZLAB", index_loc = './output'){
  
  index_header <- readLines(system.file('landing/templates/header.mustache', package="vizlab"))
  
  repos <- getRepoNames(org)
  viz_info <- lapply(repos, getVizInfo, org=org)
  list_viz_info <- list(vizzies=viz_info)
  index_vizzies <- getVizHTML(list_viz_info)
  
  if(!dir.exists(index_loc)) dir.create(index_loc, recursive=TRUE)
  index <- file.path(index_loc, 'index.html')
  ### \\\ probably need an index.mustache instead of cat() here
  cat(index_header, 
      index_vizzies,
      file=index, sep="")
     
  return(index)
}

#' Populate mustache template with vizzy information
#' 
#' @param viz_info list with a list of viz information called "vizzies", e.g. list(vizzies=list(viz_info1, viz_info2))
#' @importFrom whisker whisker.render
getVizHTML <- function(viz_info){
  template <- readLines(system.file('landing/templates/vizzies.mustache', package="vizlab"))
  whisker.render(template, data=viz_info)
}

#' Get a vector a repository names for a specific organization
#' 
#' @param org character, name of GitHub organization in which to pull out repository names
#' @importFrom github get.organization.repositories
getRepoNames <- function(org){
  vizlab_repos <- get.organization.repositories(org, ctx = get.github.context())
  repo_nms <- unlist(lapply(vizlab_repos$content, function(r){ r[['name']]}))
}

#' Get the full file path for the raw viz.yaml within a repository
#'
#' @param org character, name of GitHub organization in which to look for a repository
#' @param repo character, name of the repository to find the viz.yaml
#' @importFrom github get.repository.path
getVizYamlUrl <- function(org, repo){
  p <- get.repository.path(org, repo, "viz.yaml")
  viz.yaml_url <- p$content$html_url
  viz.yaml_url <- gsub(pattern = "github.com", replacement = "raw.githubusercontent.com", viz.yaml_url)
  viz.yaml_url <- gsub(pattern = "blob/", replacement = "", viz.yaml_url)
  return(viz.yaml_url)
}

#' Load yamls and find if the published date has passed yet
#' --> TO DO: handle NULL `publish-date` fields as FALSE
#' 
#' @param org character, name of GitHub organization in which to look for a repository
#' @param repo character, name of the repository to find the viz.yaml
#' @importFrom yaml yaml.load_file
getVizInfo <- function(org, repo){
  viz.yaml_url <- getVizYamlUrl(org, repo)
  if(length(viz.yaml_url) == 0){
    return()
  }
  
  viz.yaml <- yaml.load_file(viz.yaml_url)
  
  is_published <- as.Date(viz.yaml$info$`publish-date`) <= Sys.Date()
  if(!is_published){
     return()
  } 
  
  return(viz.yaml$info)
}
