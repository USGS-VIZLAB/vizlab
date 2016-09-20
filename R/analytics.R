#' Generic function that will apply a particular analytics
#' approach to a vizlab object
#'
#' @param viz vizlab object or identifier
#' @return vizlab object decorated with analytics
#' @export
analytics <- function(viz) UseMethod("analytics")

#' Dispatch to appropriate analytics function
#'
#' @rdname analytics
#' @export
analytics.viz <- function(viz) {
  viz <- as.analytics(viz)
  analytics(viz)
}

#' Catch analytics calls that have no matching type
#'
#' @rdname analytics
#' @export
analytics.analytics <- function(viz) {
  warning("this analytics type is not yet supported")
}

#' Add a trigger for a given element to cause a analytics event
#' when it is scrolled to
#'
#' @importFrom xml2 read_xml xml_has_attr xml_attr xml_attr<-
#' @rdname analytics
#' @export
analytics.scroll <- function(viz) {
  required <- c("output")
  checkRequired(viz, required)

  class <- "vizScroll"
  html <- read_xml(viz[['output']])
  if (xml_has_attr(html, "class")) {
    xml_attr(html, "class") <- paste(xml_attr(html, "class"), class)
  } else {
    xml_attr(html, "class") <- class
  }
  viz[['output']] <- format(html)
  return(viz)
}

#' Add an event handler for a user clicking on a visualization
#' that causes an analytics event to occur
#'
#' @importFrom xml2 read_xml xml_has_attr xml_attr xml_attr<-
#' @rdname analytics
#' @export
analytics.click <- function(viz) {
  required <- c("output")
  checkRequired(viz, required)

  class <- "vizClick"
  html <- read_xml(viz[['output']])
  if (xml_has_attr(html, "class")) {
    xml_attr(html, "class") <- paste(xml_attr(html, "class"), class)
  } else {
    xml_attr(html, "class") <- class
  }
  viz[['output']] <- format(html)
  return(viz)
}

#' Coerce vizlab object to include analytics
#'
#' @param viz vizlab object
#' @param ... unused, left for possible pass-through
#' @export
as.analytics <- function(viz, ...) {
  required <- c("analytics")
  checkRequired(viz, required)

  class(viz) <- c(viz[['analytics']], "analytics", class(viz))
  return(viz)
}
