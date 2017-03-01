#' Report on the installation status of packages needed by the vizlab project
#' 
#' Throw messages, warnings, errors, etc. if any packages are missing or newer 
#' or older than required/suggested in the viz.yaml
#' 
#' @param newer the function to call on a message about too-new packages (if 
#'   there are any)
#' @param older the function to call on a message about too-old packages (if 
#'   there are any)
#' @param absent the function to call on a message about missing packages (if 
#'   there are any)
#' @export
checkVizPackages <- function(newer=warning, older=stop, absent=stop) {
  status <- vizPackageStatus()
  miss <- status$package.name[status$status == 'missing']
  old <- status$package.name[status$status == 'older']
  new <- status$package.name[status$status == 'newer']
  info <- setNames(paste0(status$package.name, " (need ", status$target.version, ", have ", status$current.version, ")"), status$package.name)
  if(length(new) > 0) {
    newer("these packages are newer than required: ", paste0(info[new], collapse=", "))
  }
  if(length(old) > 0) {
    older("these packages are older than required: ", paste0(info[old], collapse=", "))
  }
  if(length(miss) > 0) {
    absent("these packages are absent: ", paste0(info[miss], collapse=", "))
  }
  return(status)
}

#' Identify any missing or outdated packages
#' 
#' Compares the installed package versions to those given in the
#' `required-packages` section of the viz.yaml. Returns a list of two vectors,
#' one with the names of completely missing packages and one with the names of
#' out of date packages.
vizPackageStatus <- function() {
  packages.info <- getBlocks('info', FALSE)[[1]]$'required-packages'
  if(is.null(packages.info)) stop("info$required-packages section of viz.yaml cannot be empty")
  
  # Get a vector of installed package versions
  installed <- installed.packages()[,"Version",drop=TRUE]
  
  # Create a data.frame of package statuses
  status <- do.call(rbind, lapply(names(packages.info), function(package.name) {
    target.version <- numeric_version(packages.info[[package.name]]$version)
    missing.package <- !package.name %in% names(installed)
    current.version <- if(!missing.package) numeric_version(installed[package.name]) else numeric_version(NA, strict=FALSE)
    status <- if(missing.package) {
      'missing'
    } else if(current.version < target.version) {
      'older'
    } else if(current.version > target.version) {
      'newer'
    } else {
      'perfect'
    }
    data.frame(
      package.name,
      target.version,
      current.version,
      status,
      stringsAsFactors=FALSE
    )
  }))
  rownames(status) <- NULL
  
  return(status)
}

#' [Re]install any missing or outdated packages
#' 
#' For packages whose `repo` field in `required-packages` is CRAN or GRAN, the 
#' newly installed package will be the most current one, not the precise version
#' specified in the `version` field. For packages whose `repo` is github, the 
#' package will be installed from the specified `ref`.
#' 
#' @param install.if package statuses that should cause that package to be 
#'   reinstalled. The default is recommended, but "newer" is a 
#'   sometimes-reasonable additional option, and "perfect" is technically also 
#'   an option (if you want to reinstall everything no matter what).
#' @export
updateVizPackages <- function(install.if=c('older','missing')) {
  # find out which packages need installation
  status <- checkVizPackages(message, message, message)
  needed <- status$package.name[status$status %in% install.if]
  
  # get installation instructions
  packages.info <- getBlocks('info', FALSE)[[1]]$'required-packages'
  
  # install each package
  success <- sapply(needed, function(package.name) {
    message("Installing package '", package.name, "'")
    package.info <- packages.info[[package.name]]
    switch(
      package.info$repo,
      CRAN={
        install.packages(package.name)
      },GRAN={
        install.packages(package.name, repos=union(getOption("repos"), 'https://owi.usgs.gov/R'))
      },
      github={
        remotes::install_github(repo=package.info$name, ref=package.info$ref)
      })
  })
}
