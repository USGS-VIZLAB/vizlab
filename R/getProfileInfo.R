#' Get information on profile settings
#' 
#' Reads all values from the user's profile.yaml (path found through findProfileYaml)
#' 
#' @seealso findProfileYaml
#' @import yaml
#' @export
getProfileInfo <- function() {
  # get profile information from profile.yaml
  profile.path <- findProfileYaml()
  if(!file.exists(profile.path)) {
    message(profile.path, ' does not exist; using defaults')
    profile.yaml <- list()
  } else {
    profile.yaml <- yaml.load_file(profile.path, handlers=list(
      eval=function(x) eval(parse(text=x))
    ))
  }
  
  # fill in missing values with defaults
  defaults <- list(
    SHELL=NULL,
    R='R',
    RSCRIPT='Rscript',
    R_LIBS_USER=NULL)
  use.defaults <- setdiff(names(defaults), names(profile.yaml))
  profile.yaml <- c(profile.yaml, defaults[use.defaults])
  
  # return
  profile.yaml
}

#' Find where the profile.yaml file is saved.
#' 
#' Searches two directories, '~/.vizlab/' and './vizlab/', for a file called 
#' 'profile.yaml'. If found, returns the file path where it was found; otherwise
#' throws an error.
findProfileYaml <- function(){
  home_dir <- '~/.vizlab/profile.yaml'
  relative_dir <- './vizlab/profile.yaml'
  if(file.exists(home_dir)){
    filepath <- home_dir
  } else if(file.exists(relative_dir)){
    filepath <- relative_dir
  } else {
    stop('profile.yaml does not exist in a supported directory. See createProfile().')
  }
  return(filepath)
}
