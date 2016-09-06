#' Generic function that will apply a particular analytics
#' approach to a vizlab object
#'
#' @param viz vizlab object or identifier
#' @export
analytics <- function(viz) UseMethod("analytics")

#' Add a trigger for a given element to cause a analytics event
#' when it is scrolled to
#'
#' @rdname analytics
analytics.scroll <- function(viz) {
  # add the necessary info to javascript to cause trigger
}

#' Add an event handler for a user clicking on a visualization
#' that causes an analytics event to occur
analytics.click <- function(viz) {
  # add info to js
}

#' Coerce vizlab object to include analytics
#'
#' @param viz vizlab object
#' @param ... unused, left for possible pass-through
as.analytics <- function(viz, ...) {
  required <- c("analytics")
  checkRequired(viz, required)

  class(viz) <- c(viz[['analytics']], "analytics", class(viz))
  return(viz)
}
