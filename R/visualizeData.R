#' Visualize raw or intermediate data products into intermediate or final data 
#' products
#' 
#' This function should be called from the generic, \code{visualizeData()}. 
#' Visualize raw or intermediate data products into intermediate or final data 
#' products
#' 
#' @param viz.id the identifier for this data item in viz.yaml
#' @param ... other arguments passed to visualizeData methods. These should 
#'   include any data dependencies, named according to the viz.id of those data 
#'   items
#' @param outfile the filename to which the visualized data should be written
#'   
#' @export
visualizeData <- function(viz.id, ..., outfile) UseMethod("visualizeData")

#' @rdname visualizeData
#' @export
visualizeData.character <- function(viz.id, ..., outfile) {
  # get the reading information for this data ID from viz.yaml
  data.info <- getContentInfo(viz.id, no.match='NA')
  
  # collect the user args and autopopulate if appropriate
  user.args <- list(...)
  if(missing(outfile) || (length(user.args) == 0 && length(data.info$args) > 0)) {
    all.args <- getAutoargs(data.info, fun='write')
  } else {
    all.args <- c(list(viz.id=viz.id), user.args, list(outfile=outfile))
  }

  # route subsequent calls to a specific visualizeData method
  class(all.args$viz.id) <- data.info$visualizer
  
  # call the visualizeData method applicable to this fetcher
  invisible(do.call(visualizeData, all.args))
}