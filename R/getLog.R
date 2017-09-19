#' Display, optionally open, and return the contents of a log file
#'
#' Prints a message with the tail of the log file if `tail=TRUE`. Opens the file
#' in an editor if `open=TRUE`. Always returns the entire log file as a
#' character vector.
#'
#' @param id character the viz id, eventually any possible target listed in the
#'   build output.
#' @param open logical. Should the file be opened in an editor?
#' @param tail logical. Should the tail of the file be printed to the console?
#'
#' @export
getLog <- function(id, open=FALSE, tail=TRUE) {
  
  logpath <- getLogPath(id)
  
  # read the file. this is what we'll return
  loglines <- readLines(logpath)
  
  # open the file if requested
  if(open) openFile(logpath)
  
  # print the tail of the file
  callstart <- grep("# Call function", loglines)+2
  callend <- length(loglines)
  message(paste(loglines[callstart:callend], collapse='\n'))
  
  # return the full file contents
  invisible(loglines)
}

#' Get the path to the .Rout file corresponding to a viz id (or an id_timestamp)
#' 
#' @param id character the viz id, eventually any possible target listed in the
#'   build output.
#' @export
getLogPath <- function(id) {
  if(grepl('_timestamp$', id)) {
    is_tstamp <- TRUE
    id <- gsub('_timestamp$', '', id)
  } else {
    is_tstamp <- FALSE
  }
  
  viz <- as.viz(id)
  block <- class(viz)[[1]]
  logpath <- sprintf('vizlab/make/log/%s/%s%s.Rout', block, viz$id, if(is_tstamp) '_timestamp' else '')
  
  return(logpath)
  
}

#' @keywords internal
openFile <- function(path) {
  if(is.na(path)) {
    warning("ignoring NA path")
    return()
  }
  path <- normalizePath(path)
  switch(
    .Platform$OS.type,
    'windows'=suppressWarnings(shell(paste0("explorer ", gsub("/", "\\\\", path, fixed=TRUE)), intern=TRUE)), # https://stackoverflow.com/questions/11031317/open-windows-explorer-with-specific-path-using-system-command
    'unix'=system(paste0("open ", path))) # https://stackoverflow.com/questions/11780810/launch-mac-finder-window-with-specified-path
  invisible()
}
