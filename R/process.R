#' Process raw or intermediate data products into intermediate or final data
#' products
#'
#' This function should be called from the generic, \code{process()}.
#' Process raw or intermediate data products into intermediate or final data
#' products
#'
#' @param viz.id the identifier for this data item in viz.yaml
#' @param ... other arguments passed to process methods. These should
#'   include any data dependencies, named according to the viz.id of those data
#'   items
#' @param outfile the filename to which the processed data should be written
#'
#' @export
process <- function(viz) UseMethod("process")

#' @rdname process
#' @export
process.character <- function(viz) {
  viz <- as.viz(viz)
  viz <- as.processor(viz)
  # collect the user args and autopopulate if appropriate
  #user.args <- list(...)
  #if(missing(outfile) || (length(user.args) == 0 && length(data.info$args) > 0)) {
  #  all.args <- getAutoargs(data.info, fun='write')
  #} else {
  #  all.args <- c(list(viz.id=viz.id), user.args, list(outfile=outfile))
  #}

  # route subsequent calls to a specific process method

  # call the process method applicable to this fetcher
  process(viz)
}

#' \code{process.unzip} unzip a zip file
#'
#' @rdname process
#' @importFrom utils unzip
#' @export
process.unzip <- function(viz.id, ..., outfile) {
  args <- list(...)
  sapply(args, unzip, exdir = outfile)
  return()
}

### Make processor object
as.processor <- function(viz, ...) {
  if(!exists('processor', viz))
    stop("please specify a processor for viz.id '", viz[['id']], "' in viz.yaml")
  class(viz) <- c(viz[['processor']], 'processor', class(viz))
  return(viz)
}
