#' Export content to target
#'
#' Determine the type and dispatch to that method to produce
#' files to serve up as the final viz
#'
#' Currently the publish phase is responsible for exporting,
#' but this could be separated in the future.
#'
#'
#' @param viz object to export
#' @return location to export or NULL if export shouldn't take place
#' @export
export <- function(viz) UseMethod("export")

#' Handle exporting of a page
#'
#' This returns the target location to write the page to, in
#' its current form this is a single page, but may result in
#' a list of pages named according to different formats
#' (tags, media type, language, etc.)
#'
#' @rdname export
#' @export
export.page <- function(viz) {
  name <- viz[['id']]
  if ("name" %in% names(viz)) {
    name <- viz[['name']]
  }

  file <- NULL
  if (doExport(viz)) {
    file <- paste0(exportLocation(), name, ".html")
  }

  return(file)
}

export.section <- function(viz) {
  if (!isTRUE(viz[['embed']])) {
    warning("cannot export section except for embeds")
  }
  name <- viz[['id']]
  if ("name" %in% names(viz)) {
    name <- viz[['name']]
  }
  
  file <- NULL
  if (doExport(viz)) {
    file <- paste0(exportLocation(), "embed/", name, ".html")
  }
  
  return(file)
}

#' @rdname export
#' @export
export.img <- function(viz) {
  location <- viz[['location']]
  file <- NULL
  if (doExport(viz)) {
    file <- paste0(exportLocation(), "images/", basename(location))
  }
  return(file)
}

#' @rdname export
#' @export
export.ico <- function(viz) {
  location <- viz[['location']]
  file <- NULL
  if (doExport(viz)) {
    file <- paste0(exportLocation(), "images/", basename(location))
  }
  return(file)
}

#' @rdname export
#' @export
export.svg <- function(viz) {
  # call image export directly
  return(export.img(viz))
}

#' @rdname export
#' @export
export.js <- function(viz) {
  location <- viz[['location']]
  file <- NULL
  if (doExport(viz)) {
    file <- paste0(exportLocation(), "js/", basename(location))
  }
  return(file)
}

#' @rdname export
#' @export
export.css <- function(viz) {
  location <- viz[['location']]
  file <- NULL
  if (doExport(viz)) {
    file <- paste0(exportLocation(), "css/", basename(location))
  }
  return(file)
}

#' Default export. Used to default to not export, but Alison couldn't think of
#' when that would be useful so changed the default to TRUE.
#'
#' @rdname export
#' @export
export.resource <- function(viz) {
  location <- viz[['location']]
  file <- NULL
  if (doExport(viz)) {
    file <- paste0(exportLocation(), "data/", basename(location))
  }
  return(file)
}

#' export thumbnail class to images
#' @rdname export
#' @export
export.thumbnail <- function(viz) {
  location <- viz[['location']]
  file <- NULL
  if (doExport(viz)) {
    file <- paste0(exportLocation(), "images/", basename(location))
  }
  return(file)
}

### Utility functions provided by export

#' Set where exports should be placed
#'
#' Currently not configurable
#' @return character export location path
#' @export
exportLocation <- function() {
  return("target/")
}

### Private functions used to help in exporting

#' Return logical declaring whether file export (copying to target) should be
#' performed
#'
#' @param viz vizlab object
#' @param default logical for default export if not specified
#' @return logical should this object be exported
doExport <- function(viz, default=TRUE){
  if ("export" %in% names(viz)) {
    viz[['export']]
  } else {
    default
  }
}
