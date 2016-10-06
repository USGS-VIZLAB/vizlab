### Utility functions for use in vizlab ###

#' Check required fields
#'
#' @param x object to check for fields
#' @param required list of fields required
#' @export
checkRequired <- function(x, required) {
  hasAll <- all(required %in% names(x))
  if (!hasAll) {
    stop(x[['id']], " missing at least one of ", paste(required, collapse = ", "))
  }
}

#' Get webapp path
#'
#' @param file path to file being exported
#' @return character vector describing relative path
#' @export
relativePath <- function(file) {
  exportAndPath <- sub(exportLocation(), "", file)
  return(exportAndPath)
}

#' Build context for templating
#'
#' @param viz vizlab object
#' @param dependencies list of dependency ids
#' @return list of context with dependencies injected
buildContext <- function(viz, dependencies) {
  # allow for context to be inline
  data <- viz[["context"]]
  if (is.null(data)) {
    data <- list()
  }
  else if (is.character(data)) {
    data <- readData(data)
  }
  # replace dependencies with contents
  data <- rapply(data, function(x) {
      dep.ids <- x %in% names(dependencies)
      if (any(dep.ids)) {
        x[which(dep.ids)] <- dependencies[x[which(dep.ids)]]
      }
      # TODO run smartypants on x before returning
      return(x)
  }, how = "replace", classes = "character")
  return(data)
}

#' Get vizlab js as a resource
#'
#' @return vizlab object describing vizlab.js
getVizlabJS <- function() {
  vizlab.js <- list(
    id = "_vizlabJS",
    location = "js/vizlab.js",
    packaging = "vizlab",
    publisher = "resource",
    mimetype = "application/javascript",
    export = TRUE
  )
  vizlab.js <- as.viz(vizlab.js)
  vizlab.js <- as.publisher(vizlab.js)
  return(vizlab.js)
}

#' Use mimetype lookup to get reader
#'
#' @importFrom utils modifyList
#' @param mimetype character vector of length one with the mimetype name
#' @return character vector describing the reader to be used
#' @importFrom utils modifyList
#' @export
lookupMimetype <- function(mimetype){
  # add to and replace default mimetypes using the file specified in viz.yaml
  mimetype_list_default <- yaml.load_file(system.file('mimetypes.default.yaml', package="vizlab"))
  mimetype_file_user <- getBlocks('info')[[1]]$mimetypeDictionary[[1]]
  if(length(mimetype_file_user) != 0){
    mimetype_list_user <- yaml.load_file(mimetype_file_user)
  } else {
    mimetype_list_user <- list()
  }
  mimetype_list <- modifyList(mimetype_list_default, mimetype_list_user)

  # match the current mimetype with one in the list to get the correct reader/publisher
  type_match <- which(unlist(lapply(mimetype_list,
                                    FUN=function(mimetype_list, mimetype){
                                      mimetype %in% mimetype_list},
                                    mimetype=mimetype)))
  type_nm <- names(type_match)
  return(type_nm)
}

#' Assemble whisker partials from vizlab package
#'
#' @importFrom tools file_path_sans_ext
#' @return list containing partials available
getPartialLibrary <- function() {
  template.dir <- system.file("templates", package = "vizlab")
  template.files <- dir(template.dir, pattern = "*.mustache")
  template.names <- file_path_sans_ext(template.files)
  partials <- lapply(template.files, function(x, dir) {
    viz <- list(
      location = file.path(dir, x),
      reader = "txt"
    )
    viz <- as.reader(as.viz(viz))
    return(readData(viz))
  }, template.dir)
  names(partials) <- template.names
  return(partials)
}

#' Sets up folders so file can be written without warnings
#'
#' @param file file that is about to be written
#' @export
setupFoldersForFile <- function(file) {
  dir <- dirname(file)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
