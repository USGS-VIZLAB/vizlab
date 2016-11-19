#' Define user profile information
#'
#' This creates a file that describes where necessary executables are located in order to run make.
#' This sets up a template, it is necessary to fill in the missing information
#'
#' @param directory location to save the profile.yaml file. There are currently 3 options: "home" 
#' (uses '~' to add the file to the home directory, varies by OS), "relative" (uses '.' to 
#' add the profile.yaml to a location relative to the current working directory), or the user can 
#' type the absolute file path of their choice.
#' @export
#' @examples
#' \dontrun{
#' createProfile(directory = "home")
#' createProfile(directory = "C:/Users")
#' 
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
createProfile <- function(directory = "home"){
  if(directory=="home"){
    file_dir <- file.path('~', '.vizlab')
  } else if(directory=="relative"){
    file_dir <- file.path('.', 'vizlab')
  } else {
    file_dir <- file.path(directory, 'vizlab')
  }

  if (!dir.exists(file_dir)) dir.create(file_dir)

  if (Sys.info()[['sysname']] == "Windows") {
    createProfile.Windows(file_dir)
  } else if (Sys.info()[['sysname']] == "Darwin") {
    createProfile.Mac(file_dir)
  } else if (Sys.info()[['sysname']] == "Linux") {
    createProfile.Linux(file_dir)
  } else {
    stop("Unrecognized operating system. You'll need to create 'profile.yaml' on your own.")
  }
}

#' Create user profile yaml file for Windows operating systems
#'
#' @param file_dir existing file directory for where to save the profile.yaml file
#' @keywords internal
createProfile.Windows <- function(file_dir){
  if(file.exists(file.path(file_dir, 'profile.yaml'))) {
    message("profile.yaml already exists; leaving as-is")
  } else {
    message("Creating profile.yaml ...", domain = NA)
    profile.yaml <- file(file.path(file_dir, "profile.yaml"))
    cat(sprintf('SHELL: %s\n', Sys.which('sh.exe')),
        sprintf('R: %s\n', Sys.which('R.exe')),
        sprintf('RSCRIPT: %s\n', Sys.which('Rscript.exe')),
        sprintf('R_LIBS_USER: %s\n', .libPaths()[1]),
        file = profile.yaml, sep = "")
    close(profile.yaml)
    message(paste("profile.yaml added to", file_dir))
  }
}

#' Create user profile yaml file for Mac operating systems
#'
#' @param file_dir existing file directory for where to save the profile.yaml file
#' @keywords internal
createProfile.Mac <- function(file_dir){
  if(file.exists(file.path(file_dir, 'profile.yaml'))) {
    message("profile.yaml already exists; leaving as-is")
  } else {
    message("Creating profile.yaml ...", domain = NA)
    profile.yaml <- file(file.path(file_dir, "profile.yaml"))
    cat(sprintf('SHELL: %s\n', Sys.which('sh')),
        sprintf('R: %s\n', Sys.which('R')),
        sprintf('RSCRIPT: %s\n', Sys.which('Rscript')),
        sprintf('R_LIBS_USER: %s\n', .libPaths()[1]),
        file = profile.yaml, sep = "")
    close(profile.yaml)
    message(paste("profile.yaml added to", file_dir))
  }
}

#' Create user profile yaml file for Linux operating systems
#'
#' @param file_dir existing file directory for where to save the profile.yaml file
#' @keywords internal
createProfile.Linux <- function(file_dir) {
  if(file.exists(file.path(file_dir, 'profile.yaml'))) {
    message("profile.yaml already exists; leaving as-is")
  } else {
    message("Creating profile.yaml ...", domain = NA)
    profile.yaml <- file(file.path(file_dir, "profile.yaml"))
    cat(sprintf('SHELL: %s\n', Sys.which('sh')),
        sprintf('R: %s\n', Sys.which('R')),
        sprintf('RSCRIPT: %s\n', Sys.which('Rscript')),
        sprintf('R_LIBS_USER: %s\n', .libPaths()[1]),
        file = profile.yaml, sep = "")
    close(profile.yaml)
    message(paste("profile.yaml added to", file_dir))
  }
}
