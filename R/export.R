#' Export content to target
#'
#' Determine the type and dispatch to that method to produce
#' files to serve up as the final viz
#'
#' Currently the publish phase is responsible for exporting,
#' but this could be separated in the future.
#'
#' @param viz object to export
#' @export
export <- function(viz) UseMethod("export")

#' Handle exporting of a page
#'
#' This returns the target location to write the page to, in
#' its current form this is a single page, but may result in
#' a list of pages named according to different formats
#' (tags, media type, language, etc.)
#'
#' @param viz vizlab object
#' @rdname export
#' @export
export.page <- function(viz) {
  name <- viz[['id']]
  if ("name" %in% names(viz)) {
    name <- viz[['name']]
  }

  file <- NULL
  if (doExport(viz, default = TRUE)) {
    file <- paste0(exportLocation(), name, ".html")
  }

  return(file)
}

#' @param viz vizlab object
#' @rdname export
#' @export
export.img <- function(viz) {
  location <- viz[['location']]
  file <- NULL
  if (doExport(viz, TRUE)) {
    file <- paste0(exportLocation(), "images/", basename(location))
  }
  return(file)
}

#' @param viz vizlab object
#' @rdname export
#' @export
export.svg <- function(viz) {
  # call image export directly
  return(export.img(viz))
}

#' @param viz vizlab object
#' @rdname export
#' @export
export.js <- function(viz) {
  location <- viz[['location']]
  file <- NULL
  if (doExport(viz, TRUE)) {
    file <- paste0(exportLocation(), "js/", basename(location))
  }
  return(file)
}

#' @param viz vizlab object
#' @rdname export
#' @export
export.css <- function(viz) {
  location <- viz[['location']]
  file <- NULL
  if (doExport(viz, TRUE)) {
    file <- paste0(exportLocation(), "css/", basename(location))
  }
  return(file)
}

### Utility functions provided by export

#' Set where exports should be placed
#'
#' Currently not configurable
#'
#' @export
exportLocation <- function() {
  return("target/")
}

### Private functions used to help in exporting
doExport <- function(viz, default){
  do.export <- default
  if ("export" %in% names(viz)) {
    do.export <- viz[['export']]
  }
  return(do.export)
}
