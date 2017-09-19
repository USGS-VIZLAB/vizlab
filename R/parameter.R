#' Get parameter variables for project
#'
#' This function should be called from the generic, \code{parameter()}.
#'
#' @param viz vizlab object described in viz.yaml
#' @export
parameter <- function(viz) UseMethod("parameter")

#' @rdname parameter
#' @export
parameter.character <- function(viz) {
  viz <- as.viz(viz)
  viz <- as.parameter(viz)
  parameter(viz)
}

#' @rdname parameter
#' @export
parameter.parameter <- function(viz) {
  invisible()
}

#' Coerce vizlab object to a parameter type
#'
#' @param viz vizlab object
#' @param ... not used, following convention
#' @return vizlab parameter object
#' @export
as.parameter <- function(viz, ...) {
  class(viz) <- c('parameter', class(viz))
  return(viz)
}