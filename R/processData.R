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

#' @param data.info content information for this viz.id from the viz.yaml
#' 
#' @rdname processData
#' @export
processData.default <- function(viz.id, data.info, ..., outfile) {
  # explain the problem if we're headed for infinite recursion
  if(class(viz.id) != 'character') 
    stop('could not find processData method for viz.id=', viz.id, ', processor=', class(viz.id))
  
  # get the reading information for this data ID from viz.yaml
  data.info <- getContentInfo(viz.id, no.match='NA')
  
  # routes subsequent calls to a specific processData method
  class(viz.id) <- data.info$processor
  
  # call the processData method applicable to this fetcher
  processData(viz.id=viz.id, data.info=data.info, ..., outfile=outfile)
}