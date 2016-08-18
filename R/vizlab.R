#' Utilities for building online data visualizations
#'
#' @name vizlab
NULL

### Setting up the viz object ###

#' Coerce viz objects
#'
#' @rdname as.viz
#' @param x object to coerce to viz
#' @param ... unused but following as. pattern
#' @export
as.viz <- function(x, ...) UseMethod("as.viz")

#' Convert character (id) to vizlab object
#'
#' @rdname as.viz
#' @param x character vector to convert to vizlab
#' @param ... unused
#' @export
as.viz.character <- function(x, ...) {
  # get the reading information for this data ID from viz.yaml
  viz <- getContentInfo(x, no.match='stop')
  return(as.viz(viz))
}

#' Convert list into vizlab object
#' @rdname as.viz
#' @export
as.viz.list <- function(x, ...) {
  class(x) <- "viz"
  return(x)
}
