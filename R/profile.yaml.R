#' Define user profile information
#' 
#' This file should live in user/local, etc. user/docker and should be called profile.yaml
#' 
#' @name profile.yaml
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
NULL