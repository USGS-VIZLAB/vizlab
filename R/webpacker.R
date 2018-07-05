#' Run webpack for the project
#'
#' This is called at the very end of `vizmake()` if you have a webpack block in your `viz.yaml`.
#' @param webpack.config list containing at least an input and output element that contain a
#' filepath for the javasript file going into webpack, and the name for the bundled file that results.
#' @md
#' @export
webpacker <- function() {
  
  # check if node, npm are installed
  checkInstalled("node")
  checkInstalled("npm")
  
  checkAndInstallNodeModule("webpack")
  checkAndInstallNodeModule("webpack-cli")
  
  ## have a way to fail if `start` is not the default?
  system("npm run start")
}

#' Check that a system library is installed
#' 
#' @param name string of the system library to look for
#' 
checkInstalled <- function(name) {
  tryCatch(system(paste(name, "-v")), 
           warning = function(w) { 
             stop(paste(name, "not installed:", w)) 
           })
}

#' Check if a node package is installed already or not. If not, install it
#' 
#' @param module character string of the node module to install,
#' e.g. `webpack`, `d3`, etc
#' @param addSave logical indicating whether `--save` should be inlcuded 
#' in the install command
#' 
checkAndInstallNodeModule <- function(module, addSave=TRUE) {
  
  oldwd <- getwd()
  setwd(exportLocation())
  
  cmd <- paste("npm list", module)
  output <- system(cmd, intern=TRUE)
  
  err_status <- !is.null(attr(output, "status")) && attr(output, "status") != 0
  notInstalled <-  err_status || any(grep("empty", output))
  if(notInstalled) {
    cmd_install <- paste("npm install", module)
    if(addSave) { cmd_install <- paste(cmd_install, "--save") }
    system(cmd_install)
  }
  
  setwd(oldwd)
}
