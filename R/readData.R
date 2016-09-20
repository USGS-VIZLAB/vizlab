#' Ensure data is on the local file system
#'
#' This function should be called from the generic, \code{readData()}. Reads
#' data from a file into R format. If you just want the filepath so you can do
#' it yourself, specify "customReader: filepath" in the viz.yaml
#'
#' @param viz vizlab object or identifier
#'
#' @export
readData <- function(viz) UseMethod("readData")
  
#' \code{readData.character} is the standard entry point; from here, viz.id gets
#' assigned a class to route it to a more specific reader
#'
#' @rdname readData
#' @export
readData.character <- function(viz) {
  viz <- as.viz(viz)
  viz <- as.reader(viz)
  sourceScripts('scripts/read')
  # call the readData method applicable to this item
  readData(viz)
}

#' \code{readData.tabular} reads a text file.
#'
#' @rdname readData
#' @export
readData.tabular <- function(viz) {
  if(!requireNamespace('data.table', quietly = TRUE)) stop("package data.table is required for readData.tabular")
  x <- data.table::setDF(data.table::fread(viz[['location']]))
  x # assign to x first so it returns visibly
}

#' \code{readData.yaml} reads a yaml file.
#'
#' @rdname readData
#' @import yaml
#' @export
readData.yaml <- function(viz) {
  yaml.load_file(viz[['location']])
}

#' \code{readData.excel} reads the first spreadsheet of an Excel file.
#'
#' @rdname readData
#' @export
readData.excel <- function(viz) {
  if(!requireNamespace('readxl', quietly = TRUE)) stop("package readxl is required for readData.excel")
  readxl::read_excel(viz[['location']])
}

#' \code{readData.RDS} reads an R object saved as an RDS.
#'
#' @rdname readData
#' @export
readData.rds <- function(viz){
  readRDS(viz[['location']])
}

#' \code{readData.filepath} returns the file path
#'
#' @rdname readData
#' @export
readData.filepath <- function(viz){
  viz[['location']]
}

#' \code{readData.folder} returns names of files inside a folder
#'
#' @rdname readData
#' @export
readData.folder <- function(viz){
  # present: all files need to be the same mimetype, no mimetype = filepath
  #          you need to specify "reader: folder" and the shared mimetype in "viz.yaml"
  # to do: make this recursive to get into subdirs (add to viz object)
  # to do: add pattern so that it only reads in files that meet some pattern (e.g ".csv")
  filepaths <- dir(viz[['location']], full.names=TRUE)
  
  #create new vizlab object (one list element for each file)
  viz_lists <- lapply(filepaths, function(fp, id, m){
    list(id=id, location=fp, mimetype=m)
  }, id=viz[['id']], m=viz[['mimetype']])

  lt2 <- lapply(viz_lists, function(v){
    v <- as.reader(v)
    readData(v)
  })
  
}

#' \code{readData.txt} returns names of files inside a folder
#'
#' @rdname readData
#' @export
readData.txt <- function(viz){
  scan(viz[['location']], what="character", sep="\n")
}

### Set up the reader class

#' Treat viz object as a reader
#' Doesn't need to be generic at this point (ever?)
#'
#' @param viz object to coerce to reader
#' @param ... does nothing, following pattern
#' @export
as.reader <- function(viz, ...) {
  id <- viz[['id']]
  location <- viz[['location']]
  if (is.null(location)) {
    stop("Readers require 'location' property")
  }
  reader <- viz[['reader']]
  if (is.null(reader)) {
    mimetype <- viz[['mimetype']]
    reader <- lookupMimetype(mimetype)
    
    if(length(reader) == 0){
      warning('Could not find specific readData method for viz.id=', id,
              ', mimetype=', mimetype, '; returning filepath.',
              ' Specify reader to override.')
      reader <- "filepath"
    }
  }
  class(viz) <- c(reader, "reader", class(viz))
  return(viz)
}
