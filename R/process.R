#' Process raw or intermediate data products into intermediate or final data
#' products
#'
#' This function should be called from the generic, \code{process()}.
#' Process raw or intermediate data products into intermediate or final data
#' products
#'
#' @param viz vizlab object described in viz.yaml
#' @export
process <- function(viz) UseMethod("process")

#' @rdname process
#' @export
process.character <- function(viz) {
  viz <- as.viz(viz)
  viz <- as.processor(viz)
  process(viz)
}

#' \code{process.unzip} unzip a zip file
#'
#' @rdname process
#' @importFrom utils unzip
#' @export
process.unzip <- function(viz) {
  required <- c("location", "depends")
  checkRequired(required)

  zips <- sapply(as.viz(viz[['depends']]), `[[`, 'location')
  exdir <- viz[['location']]
  sapply(zips, unzip, exdir = exdir)
  return()
}

### Make processor object
as.processor <- function(viz, ...) {
  if(!exists('processor', viz))
    stop("please specify a processor for viz.id '", viz[['id']], "' in viz.yaml")
  class(viz) <- c(viz[['processor']], 'processor', class(viz))
  return(viz)
}
