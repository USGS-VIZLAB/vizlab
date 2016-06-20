#' Get information on profile settings
#' 
#' Reads all values from the specified user's profile.yaml
#' 
#' @param user username, probably either 'local' or 'docker', whose profile to
#'   retrieve
#' @import yaml
#' @export
getProfileInfo <- function(user='local') {
  # get profile information from profile.yaml
  profile.path <- file.path('user', user, 'profile.yaml')
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