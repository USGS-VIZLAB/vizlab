#' Prepare the arguments to pass to a fetch, read, process, or
#' visualize
#' 
#' @param data.info character viz.id or list as in getContentInfo(vizid)
#' @param fun data mode: read or write? write is the default
#' @param verbose logical. should there be a message that we're using autoargs?
#' @export
getAutoargs <- function(data.info, fun=c('write','read'), verbose=TRUE) {
  UseMethod("getAutoargs")
}

#' @param ... args passed on to other getAutoargs methods
#' @rdname getAutoargs
#' @export
getAutoargs.character <- function(data.info, ...) {
  data.info <- getContentInfo(data.info)
  getAutoargs(data.info, ...)
}

#' @rdname getAutoargs
#' @export
getAutoargs.list <- function(data.info, fun=c('write','read'), verbose=TRUE) {
  fun <- match.arg(fun)
  
  if(verbose) {
    message('autoargs: overriding manual & default args with viz.yaml args')
  }
  
  # source any scripts in case they've changed
  sourceScripts(data.info$scripts, verbose=TRUE)
   
  # return the dependencies and user args as a list
  c(list(viz.id=data.info$id),
    if(fun == 'read') list(location=data.info$location),
    lapply(setNames(nm=data.info$depends), function(dep) readData(dep) ),
    data.info$args, 
    if(fun == 'write') list(outfile=data.info$location))
}
