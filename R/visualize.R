#' Visualize raw or intermediate data products into intermediate or final data
#' products
#'
#' This function should be called from the generic, \code{visualize()}.
#' Visualize raw or intermediate data products into intermediate or final data
#' products
#'
#' @param viz.id the identifier for this data item in viz.yaml
#' @param ... other arguments passed to visualize methods. These should
#'   include any data dependencies, named according to the viz.id of those data
#'   items
#' @param outfile the filename to which the visualized data should be written
#'
#' @export
visualize <- function(viz) UseMethod("visualize")

#' @rdname visualize
#' @export
visualize.character <- function(viz) {
  # get the reading information for this data ID from viz.yaml
  viz <- as.viz(viz)
  viz <- as.visualizer(viz)

  # collect the user args and autopopulate if appropriate
  #user.args <- list(...)
  #if(missing(outfile) || (length(user.args) == 0 && length(data.info$args) > 0)) {
  #  all.args <- getAutoargs(data.info, fun='write')
  #} else {
  #  all.args <- c(list(viz.id=viz.id), user.args, list(outfile=outfile))
  #}

  # call the visualize method applicable to this fetcher
  visualize(viz)
}

## viz object creation

as.visualizer <- function(viz, ...) {
  if(!exists('visualizer', viz)) {
    stop("please specify a visualizer for '", viz[['id']], "' in viz.yaml")
  }
  class(viz) <- c(viz[['visualizer']], "visualizer", class(viz))
  return(viz)
}
