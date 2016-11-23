#' Define user profile information
#' 
#' This creates a file that describes where necessary executables are located in
#' order to run make. This sets up a template, it is necessary to fill in the 
#' missing information
#' 
#' @param directory location to save the profile.yaml file. Though 
#'   \code{directory} accepts any directory path, the only useful values are 
#'   those that point to \code{~/.vizlab} or code{./vizlab} at runtime. The 
#'   default is recommended except when the system variable \code{HOME} differs 
#'   between the current R session (try \code{Sys.getenv('HOME')}) and the bash 
#'   session from which \code{make} will be run (open bash and try \code{echo 
#'   $HOME}). This parameter should match the bash version at a minimum; it 
#'   might also be useful to run \code{createProfile()} once for each possible 
#'   value of \code{HOME} (see examples). Another option is "./vizlab" within
#'   the project directory for this particular visualization.
#' @param overwrite logical. If the file exists, should it be overwritten?
#' @export
#' @examples
#' \dontrun{
#' # The two calls commonly useful for Windows:
#' createProfile()
#' createProfile("~/../.vizlab")
#' 
#' # Example file for Windows user[s]:
#' SHELL: c:/Rtools/bin/sh.exe
#' R: C:/Program Files/R/R-3.3.2/bin/x64/R.exe
#' RSCRIPT: C:/Program Files/R/R-3.3.2/bin/x64/Rscript.exe
#' R_LIBS_USER: C:/Users/aappling/Documents/R/win-library/3.3
#' }
createProfile <- function(directory = "~/.vizlab", overwrite = FALSE){
  
  if (!dir.exists(directory)) dir.create(directory)
  
  if(!overwrite && file.exists(file.path(directory, 'profile.yaml'))) {
    message("profile.yaml already exists; leaving as-is")
  } else {
    message("Creating profile.yaml ...", domain = NA)
    switch(
      Sys.info()[['sysname']],
      'Windows' = createProfile.Windows(directory),
      'Darwin' = createProfile.Mac(directory),
      'Linux' = createProfile.Linux(directory),
      "Unrecognized operating system. You'll need to create 'profile.yaml' on your own."
    )
    message(paste("profile.yaml added to", normalizePath(directory)))
  }
}

#' Create user profile yaml file for Windows operating systems
#'
#' @param file_dir existing file directory for where to save the profile.yaml file
#' @keywords internal
createProfile.Windows <- function(file_dir){
  profile.yaml <- file(file.path(file_dir, "profile.yaml"))
  cat(sprintf('SHELL: %s\n', normalizePath(Sys.which('sh.exe'), winslash='/')),
      sprintf('R: %s\n', normalizePath(Sys.which('R.exe'), winslash='/')),
      sprintf('RSCRIPT: %s\n', normalizePath(Sys.which('Rscript.exe'), winslash='/')),
      sprintf('R_LIBS_USER: %s\n', normalizePath(.libPaths()[1], winslash='/')),
      file = profile.yaml, sep = "")
  close(profile.yaml)
}

#' Create user profile yaml file for Mac operating systems
#'
#' @param file_dir existing file directory for where to save the profile.yaml file
#' @keywords internal
createProfile.Mac <- function(file_dir){
  profile.yaml <- file(file.path(file_dir, "profile.yaml"))
  cat(sprintf('SHELL: %s\n', Sys.which('sh')),
      sprintf('R: %s\n', Sys.which('R')),
      sprintf('RSCRIPT: %s\n', Sys.which('Rscript')),
      sprintf('R_LIBS_USER: %s\n', .libPaths()[1]),
      file = profile.yaml, sep = "")
  close(profile.yaml)
}

#' Create user profile yaml file for Linux operating systems
#'
#' @param file_dir existing file directory for where to save the profile.yaml file
#' @keywords internal
createProfile.Linux <- function(file_dir) {
  profile.yaml <- file(file.path(file_dir, "profile.yaml"))
  cat(sprintf('SHELL: %s\n', Sys.which('sh')),
      sprintf('R: %s\n', Sys.which('R')),
      sprintf('RSCRIPT: %s\n', Sys.which('Rscript')),
      sprintf('R_LIBS_USER: %s\n', .libPaths()[1]),
      file = profile.yaml, sep = "")
  close(profile.yaml)
}
