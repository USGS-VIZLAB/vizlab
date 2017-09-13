#' Get global variables for project
#'
#' This function should be called from the generic, \code{global()}.
#'
#' @param viz vizlab object described in viz.yaml
#' @export
global <- function(viz) UseMethod("global")

#' @rdname global
#' @export
global.character <- function(viz) {
  viz <- as.viz(viz)
  viz <- as.global(viz)
  global(viz)
}

#' @rdname global
#' @export
global.global <- function(viz) {
  invisible()
}

#' Coerce vizlab object to a global type
#'
#' @param viz vizlab object
#' @param ... not used, following convention
#' @return vizlab global object
#' @export
as.global <- function(viz, ...) {
  class(viz) <- c('global', class(viz))
  return(viz)
}