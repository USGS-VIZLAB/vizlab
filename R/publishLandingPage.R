#' Get information about other visualizations from VIZLAB to generate the landing page
#' 
#' the github package from the GitHub repo rgithub (https://github.com/cscheid/rgithub)
#' was moved to GRAN and called grithub
#' @param dev logical create development version of landing page
#' @export
publishLandingPage <- function(dev = FALSE){

  file.copy(from=system.file('landing', package="vizlab"), to=getwd(),
            recursive = TRUE, overwrite = TRUE)
  oldwd <- getwd()
  setwd('landing')
  on.exit(setwd(oldwd))
  
  viz <- as.viz('landing')
  viz <- as.publisher(viz)
  viz$dev <- dev
  publish(viz)
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
  if(!p$ok) stop("grithub::get.repository.path was unsuccessful")
  viz.yaml_url <- p$content$html_url
  viz.yaml_url <- gsub(pattern = "github.com", replacement = "raw.githubusercontent.com", viz.yaml_url)
  viz.yaml_url <- gsub(pattern = "blob/", replacement = "", viz.yaml_url)
  return(viz.yaml_url)
}

#' Load yamls and find if the published date has passed yet
#' 
#' @param org character, name of GitHub organization in which to look for a repository
#' @param repo character, name of the repository to find the viz.yaml
#' @param dev logical create development version of landing page
#' @importFrom yaml yaml.load_file
getVizInfo <- function(org, repo, dev){
  viz.yaml_url <- getVizYamlUrl(org, repo)

  if(length(viz.yaml_url) == 0){
    return()
  }
  
  viz.yaml <- yaml.load_file(viz.yaml_url)
  
  has_publish_date <- !is.null(viz.yaml$info$`publish-date`)
  if(!dev){
    if(has_publish_date){
      publish_date <- as.Date(viz.yaml$info$`publish-date`)
      is_published <- publish_date <= Sys.Date()  
      if(!is_published){
        return()
      } 
    } else {
      return()
    }
  } else {
    publish_date <- as.Date(viz.yaml$info$`publish-date`)
    is_published <- TRUE
    if(is.null(publish_date) | is.na(publish_date)){
      publish_date <- Sys.Date()
    }
  }
  viz_info <- viz.yaml$info
  
  viz_url <- viz_info$url
  if(is.null(viz_url)) {
    viz_url <- viz_info$path
    if(!grepl('^http', viz_url)){
      viz_url <- paste0("https://owi.usgs.gov/vizlab/",viz_url)
    }
  }
  
  if(substring(viz_url, nchar(viz_url)) != "/"){
    viz_url <- paste0(viz_url,"/")
  }
  
  #what viz info needs to look like:
  thumbnail_stuff <- list(url = viz_info$`thumbnail-landing`$url,
                          alttext = viz_info$`thumbnail-landing`$alttext)
  
  if(is.null(thumbnail_stuff$url)){
    publish_stuff <- viz.yaml$publish

    landing_context <- lapply(publish_stuff, function(x) x$context$thumbnails$landing)
    
    if(length(landing_context) > 0){
      publish_id <- which(sapply(landing_context, function(x) !is.null(x)))
      if(length(publish_id) > 0 && any(publish_id)){
        thumb_landing_id <- unlist(landing_context)
        real_thumb_id <- publish_stuff[[publish_id]][['depends']][[thumb_landing_id]]
        
        thumb_index <- which(sapply(publish_stuff, function(x) x[['id']] == real_thumb_id))
        
        publish_thumb_info <- publish(publish_stuff[[thumb_index]])
        thumbnail_stuff[['url']] <- publish_thumb_info[['url']]
        thumbnail_stuff[['alttext']] <- publish_thumb_info[['alttext']]      
      }
    }

  }
  
  viz_info_required <- list(id=viz_info$id,
                            template="templates/vizzies.mustache",
                            publisher="section",
                            "publish-date"=publish_date,
                            context=list(name=viz_info$name,
                                         thumbnail=paste0(viz_url,
                                                         checkRelVizUrl(thumbnail_stuff[['url']])),
                                         alttext=thumbnail_stuff[['alttext']],
                                         path=checkRelVizUrl(viz_url),
                                         description=viz_info$description))
  
  viz_info_required <- as.viz(viz_info_required)
  viz_info_required <- as.publisher(viz_info_required)
  
  return(viz_info_required)
}

#' function to keep the absolute viz path or make the path relative
#' 
#' @param path the path defined in the viz object
#'
checkRelVizUrl <- function(path){
  if(!is.null(path)){
    if(grepl('^/', path)){
      path <- paste0('.', path)
    } else if(grepl('^http', path)){
      path <- path
    } 
  }
  return(path)
}

