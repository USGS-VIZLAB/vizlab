#' Ensure data is on the local file system
#' 
#' This function should be called from the generic, \code{readData()}. Reads 
#' data from a file into R format. If you just want the filepath so you can do
#' it yourself, specify "customReader: filepath" in the viz.yaml
#' 
#' @param viz.id the identifier for this data item in viz.yaml
#' @param ... other arguments passed to readData methods
#'   
#' @export
readData <- function(viz.id, ...) UseMethod("readData")

#' \code{readData.character} is the standard entry point; from here, viz.id gets
#' assigned a class to route it to a more specific reader
#' 
#' @param location path and filename of the file to read
#'   
#' @rdname readData
#' @export
readData.character <- function(viz.id, location, ...) {
  # get the reading information for this data ID from viz.yaml
  data.info <- getContentInfo(viz.id, no.match='stop')
  
  # collect the user args. we'd consider autopopulating, but it's currently
  # never appropriate
  user.args <- list(...)
  # if(no.args.and.expected.otherwise) {
  #   all.args <- getAutoargs(data.info, fun='read')
  # } else {
  all.args <- c(list(viz.id=viz.id, location=data.info$location), user.args)
  # }
  
  # route subsequent calls to a specific readData method
  if(exists('customReader', data.info)) {
    sourceScripts('scripts/read')
    reader <- data.info$customReader
  } else {
    reader <- switch(
      data.info$mimeType,
      "text/csv" = "csv",
      "text/tab-separated-values" = "csv",
      "text/yaml" = "yaml",
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = "excel",
      "RDS" = "RDS",
      "folder" = "folder",
      "application/zip" = "filepath",
      {
        warning(
          'could not find specific readData method for viz.id=', all.args$viz.id, 
          ', mimeType=', data.info$mimeType, '; returning filepath')
        "filepath"
      })
  }
  class(all.args$viz.id) <- reader
  
  # call the readData method applicable to this item
  do.call(readData, all.args)
}

#' \code{readData.csv} reads a csv file.
#'
#' @rdname readData
#' @export
readData.csv <- function(viz.id, location, ...) {
  if(!requireNamespace('data.table', quietly = TRUE)) stop("package data.table is required for readData.csv")
  x <- data.table::setDF(data.table::fread(location))
  x # assign to x first so it returns visibly
}

#' \code{readData.yaml} reads a yaml file.
#'
#' @rdname readData
#' @import yaml
#' @export
readData.yaml <- function(viz.id, location, ...) {
  yaml.load_file(location)
}

#' \code{readData.excel} reads the first spreadsheet of an Excel file.
#'
#' @rdname readData
#' @export
readData.excel <- function(viz.id, location, ...) {
  if(!requireNamespace('readxl', quietly = TRUE)) stop("package readxl is required for readData.excel")
  readxl::read_excel(location)
}

#' \code{readData.RDS} reads an R object saved as an RDS.
#'
#' @rdname readData
#' @export
readData.RDS <- function(viz.id, location, ...){
  readRDS(location)
}

#' \code{readData.filepath} returns the file path
#'
#' @rdname readData
#' @export
readData.filepath <- function(viz.id, location, ...){
  location
}

#' \code{readData.folder} returns names of files inside a folder
#'
#' @rdname readData
#' @export
readData.folder <- function(viz.id, location, ...){
  dir(location, full.names=TRUE)
}
