#' Reads a template file and returns the contents
#'
#' @param viz character vector representing template by name or file path
#' @return character vector containing template contents
#' @export
template <- function(viz) UseMethod("template")

#' Read a template from file or id
#'
#' @importFrom digest digest
#' @rdname template
#' @export
template.character <- function(viz) {
  if (file.exists(viz)) {
    new.obj <- list(
      id = digest(viz, algo="sha1", file=TRUE),
      location = viz,
      publisher = "template",
      mimetype = "text/mustache",
      context = NULL
    )
    viz <- new.obj
  }
  viz <- as.viz(viz) # lookup here if file doesn't exist
  viz <- as.publisher(viz)
  return(viz)
}

#' Coerce to template type publisher
#'
#' @param viz vizlab object to coerce
#' @export
as.template <- function(viz, ...) {
  required <- c("location")
  checkRequired(viz, required)

  if (!file.exists(viz[['location']])) {
    internal <- system.file(viz[['location']], package = packageName())
    if (file.exists(internal)) {
      viz[['location']] <- internal
    }
  }
  template <- readData(viz)
  viz[['template']] <- template
  return(viz)
}
