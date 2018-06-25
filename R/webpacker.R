#' Run webpack for the project
#'
#' This is called at the very end of `vizmake()` if you have a webpack block in your `viz.yaml`.
#' @param webpack.config list containing at least an input and output element that contain a
#' filepath for the javasript file going into webpack, and the name for the bundled file that results.
#' @md
#' @export
webpacker <- function(webpack.config) {
  
  checkRequired(webpack.config[["context"]], c("input", "output"))
  
  # check if node, npm are installed
  checkInstalled("node")
  checkInstalled("npm")
  
  # create webpack.config.js
  configFilepath <- file.path(exportLocation(), "webpack.config.js")
  if(!file.exists(configFilepath)) {
    generateWebpackConfigFile(configFilepath, webpack.config)
  }
  
  # create package.json
  packagejsonFilepath <- file.path(exportLocation(), "package.json")
  if(!file.exists(packagejsonFilepath)) {
    generatePackageJsonFile(packagejsonFilepath, webpack.config)
  }
  
  checkAndInstallNodeModule("webpack")
  checkAndInstallNodeModule("webpack-cli")
  checkAndInstallNodeModule("d3", addSave=FALSE)
  
  # setup appropriate directories
  srcDir <- file.path(exportLocation(), "src")
  moduleDir <- file.path(srcDir, "modules")
  if(!dir.exists(srcDir)) { dir.create(srcDir, showWarnings = FALSE) }
  if(!dir.exists(moduleDir)) { dir.create(moduleDir, showWarnings = FALSE) }
  
}

#' Check that a system library is installed
#' 
checkInstalled <- function(name) {
  tryCatch(system(paste(name, "-v")), 
           warning = function(w) { 
             stop(paste(name, "not installed:", w)) 
           })
}

#' Check if an npm package is installed already or not. If not, install it
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

#' Create the webpack.config.js file
#' Needs an input and output in webpack.config
#' 
#' Could switch out the template to a custom one.
generateWebpackConfigFile <- function(file, webpack.config) {
  
  if(!is.null(webpack.config[['template-webpackconfig']])) {
    template <- template(webpack.config[['template-webpackconfig']])
  } else {
    template <- template("webpackconfig")
  }
  
  template_output <- render(template, webpack.config[["context"]])
  write(template_output, file)
  
}

#' Create the package.json file
#' 
#' Could switch out the template to a custom one.
generatePackageJsonFile <- function(file, webpack.config) {
  
  if(!is.null(webpack.config[['template-packagejson']])) {
    template <- template(webpack.config[['template-packagejson']])
  } else {
    template <- template("packagejson")
  }
  
  template_output <- render(template, webpack.config)
  write(template_output, file)
  
}
