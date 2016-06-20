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

#' @param data.info content information for this viz.id from the viz.yaml
#' 
#' @rdname visualizeData
#' @export
visualizeData.default <- function(viz.id, data.info, ..., outfile) {
  # explain the problem if we're headed for infinite recursion
  if(class(viz.id) != 'character') 
    stop('could not find visualizeData method for viz.id=', viz.id, ', visualizer=', class(viz.id))
  
  # get the reading information for this data ID from viz.yaml
  data.info <- getContentInfo(viz.id, no.match='NA')
  
  # routes subsequent calls to a specific visualizeData method
  class(viz.id) <- data.info$visualizer
  
  # call the visualizeData method applicable to this fetcher
  visualizeData(viz.id=viz.id, data.info=data.info, ..., outfile=outfile)
}