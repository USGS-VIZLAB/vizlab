#' Get information about other visualizations from VIZLAB to generate the landing page
#' 
#' the github package from the GitHub repo rgithub (https://github.com/cscheid/rgithub)
#' was moved to GRAN and called grithub
#' @export
publishLandingPage <- function(){

  file.copy(from=system.file('landing', package="vizlab"), to=getwd(),
            recursive = TRUE, overwrite = TRUE)
  oldwd <- getwd()
  setwd('landing')
  on.exit(setwd(oldwd))
  
  publish('landing')
}

#' Get a vector a repository names for a specific organization
#' 
#' @param org character, name of GitHub organization in which to pull out repository names
#' @importFrom grithub get.organization.repositories get.github.context
getRepoNames <- function(org){
  vizlab_repos <- get.organization.repositories(org, ctx = get.github.context())
  repo_nms <- unlist(lapply(vizlab_repos$content, function(r){ r[['name']]}))
}

#' Get the full file path for the raw viz.yaml within a repository
#'
#' @param org character, name of GitHub organization in which to look for a repository
#' @param repo character, name of the repository to find the viz.yaml
#' @importFrom grithub get.repository.path
getVizYamlUrl <- function(org, repo){
  p <- get.repository.path(org, repo, "viz.yaml")
  viz.yaml_url <- p$content$html_url
  viz.yaml_url <- gsub(pattern = "github.com", replacement = "raw.githubusercontent.com", viz.yaml_url)
  viz.yaml_url <- gsub(pattern = "blob/", replacement = "", viz.yaml_url)
  return(viz.yaml_url)
}

#' Load yamls and find if the published date has passed yet
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
  
  has_publish_date <- !is.null(viz.yaml$info$`publish-date`)
  if(has_publish_date){
    is_published <- as.Date(viz.yaml$info$`publish-date`) <= Sys.Date()  
    if(!is_published){
      return()
    } 
  } else {
    return()
  }

  viz_info <- viz.yaml$info
  
  #what viz info needs to look like:
  viz_info_required <- list(id=viz_info$id,
                            template="templates/vizzies.mustache",
                            publisher="section",
                            context=list(name=viz_info$name,
                                         thumbnail=getVizUrl(viz_info$thumbnail$url),
                                         alttext=viz_info$thumbnail$alttext,
                                         path=getVizUrl(viz_info$path)))
  
  viz_info_required <- as.viz(viz_info_required)
  viz_info_required <- as.publisher(viz_info_required)
  
  return(viz_info_required)
}

#' function to keep the absolute viz path or make the path relative
#' 
#' @param path the path defined in the viz object
#'
getVizUrl <- function(path){
  if(!is.null(path)){
    if(grepl('^/', path)){
      path <- paste0('.', path)
    } else if(grepl('^http', path)){
      path <- path
    } else {
      path <- paste0('./', path)
    }
  }
  return(path)
}

