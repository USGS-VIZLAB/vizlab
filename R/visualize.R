#' Visualize raw or intermediate data products into intermediate or final data
#' products
#'
#' This function should be called from the generic, \code{visualize()}.
#' Visualize raw or intermediate data products into intermediate or final data
#' products
#'
#' @param viz vizlab object described in viz.yaml
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

#' Coerce to a visualizer
#'
#' @param viz vizlab object
#' @param ... not used, follows convention
#' @return visualizer object
#' @export
as.visualizer <- function(viz, ...) {
  if(!exists('visualizer', viz)) {
    stop("please specify a visualizer for '", viz[['id']], "' in viz.yaml")
  }
  class(viz) <- c(viz[['visualizer']], "visualizer", class(viz))
  return(viz)
}
