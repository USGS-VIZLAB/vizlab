#' Process raw or intermediate data products into intermediate or final data 
#' products
#' 
#' This function should be called from the generic, \code{processData()}. 
#' Process raw or intermediate data products into intermediate or final data 
#' products
#' 
#' @param viz.id the identifier for this data item in viz.yaml
#' @param ... other arguments passed to processData methods. These should 
#'   include any data dependencies, named according to the viz.id of those data 
#'   items
#' @param outfile the filename to which the processed data should be written
#'   
#' @export
processData <- function(viz.id, ..., outfile) UseMethod("processData")

#' @rdname processData
#' @export
processData.character <- function(viz.id, ..., outfile) {
  # get the reading information for this data ID from viz.yaml
  data.info <- getContentInfo(viz.id, block='process', no.match='stop')
  
  # collect the user args and autopopulate if appropriate
  user.args <- list(...)
  if(missing(outfile) || (length(user.args) == 0 && length(data.info$args) > 0)) {
    all.args <- getAutoargs(data.info, fun='write')
  } else {
    all.args <- c(list(viz.id=viz.id), user.args, list(outfile=outfile))
  }

  # route subsequent calls to a specific processData method
  if(!exists('processor', data.info)) 
    stop("please specify a processor for viz.id '", viz.id, "' in viz.yaml")
  class(all.args$viz.id) <- data.info$processor
  
  # call the processData method applicable to this fetcher
  invisible(do.call(processData, all.args))
}

#' \code{processData.unzip} unzip a zip file
#'
#' @rdname processData
#' @importFrom utils unzip
#' @export
processData.unzip <- function(viz.id, ..., outfile) {
  args <- list(...)
  sapply(args, unzip, exdir = outfile)
  return()
}
