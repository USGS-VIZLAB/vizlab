#' Define user profile information
#' 
#' This creates a file that describes where necessary executables are located in order to run make.
#' This sets up a template, it is necessary to fill in the missing information
#' 
#' @param directory existing file directory for where to save the profile.yaml file
#' @export
#' @examples 
#' \dontrun{
#' # Example file for Windows user[s]:
#' SHELL: /usr/bin/sh
#' R: C:/Program Files/R/R-3.3.0/bin/x64/R.exe
#' RSCRIPT: C:/Program Files/R/R-3.3.0/bin/x64/Rscript.exe
#' R_LIBS_USER: !eval >
#'   paste0(
#'     if(basename(Sys.getenv("R_USER"))=='Documents') 
#'       Sys.getenv("R_USER") 
#'     else 
#'       normalizePath(file.path(Sys.getenv("R_USER"), 'Documents'), winslash='/'), 
#'     "/R/win-library/3.3")
#' }
createProfile <- function(directory){
  if(Sys.info()[['sysname']] == "Windows"){
    createProfile.Windows(directory)
  } else if(Sys.info()[['sysname']] == "Darwin"){
    createProfile.Mac(directory)
  } else {
    stop("Unrecognized operating system. You'll need to create 'profile.yaml' on your own.")
  }
}

#' Create user profile yaml file for Windows operating systems
#' 
#' @param directory existing file directory for where to save the profile.yaml file
#' @keywords internal
createProfile.Windows <- function(directory){
  if(file.exists(file.path(directory, 'profile.yaml'))) {
    message("profile.yaml already exists; leaving as-is")
  } else {
    message("Creating profile.yaml ...", domain = NA)
    profile.yaml <- file(file.path(directory, "profile.yaml"))
    cat('SHELL: > path to shell.exe\n',
        'R: > path to R.exe\n',
        'RSCRIPT: > path to Rscript.exe\n',
        'R_LIBS_USER: !eval >\n',
        '          paste0(\n',
        '            if(basename(Sys.getenv("R_USER"))=="Documents"")\n',
        '              Sys.getenv("R_USER")\n',
        '            else\n',
        '              normalizePath(file.path(Sys.getenv("R_USER"), "Documents"), winslash="/"),\n',
        '            "/R/win-library/3.3")\n',
        file = profile.yaml, sep = "")
    close(profile.yaml)
  }
}

#' Create user profile yaml file for Mac operating systems
#' 
#' @param directory existing file directory for where to save the profile.yaml file
#' @keywords internal
createProfile.Mac <- function(directory){
  if(file.exists(file.path(directory, 'profile.yaml'))) {
    message("profile.yaml already exists; leaving as-is")
  } else {
    stop("createProfile.Mac is not supported yet")
  }
}
