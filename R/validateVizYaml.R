#' Find obvious errors in your viz.yaml
#' 
#' @export
validateVizYaml <- function() {
  # require that ids have no spaces in them,
  # require that locations have no spaces in them (i think make requires this),
  # require that expected sections exist,
  # require taht unexpected sections don't exist,
  # require that mimeTypes are real mimeTypes,
  # require that item locations are in data, cache/fetch, cache/process, cache/visualize, or 
  #   maybe external to visualization directory (?)
  # etc.
  warning("this function currently doesn't do any checking")
}