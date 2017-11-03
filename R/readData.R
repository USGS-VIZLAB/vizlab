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
  readData(viz)
}

#' \code{readData.viz} reads contents represented by vizlab object
#'
#' @rdname readData
#' @export
readData.viz <- function(viz) {
  viz <- as.reader(viz)
  if (isTRUE(viz[['_stop_recursion_']])) {
    readData.filepath(viz) # default
  } else {
    viz[['_stop_recursion_']] <- TRUE
    # call the readData method applicable to this item
    readData(viz)
  }
}

#' \code{readData.tabular} reads a text file.
#'
#' @rdname readData
#' @export
readData.tabular <- function(viz) {
  if(!requireNamespace('data.table', quietly = TRUE)) stop("package data.table is required for readData.tabular")
  required <- c("location")
  checkRequired(viz, required)

  x <- data.table::setDF(data.table::fread(viz[['location']]))
  return(x)
}

#' \code{readData.yaml} reads a yaml file.
#'
#' @rdname readData
#' @import yaml
#' @export
readData.yaml <- function(viz) {
  required <- c("location")
  checkRequired(viz, required)

  yaml <- yaml.load_file(viz[['location']])
  return(yaml)
}

#' \code{readData.excel} reads the first spreadsheet of an Excel file.
#'
#' @rdname readData
#' @export
readData.excel <- function(viz) {
  if(!requireNamespace('readxl', quietly = TRUE)) stop("package readxl is required for readData.excel")

  required <- c("location")
  checkRequired(viz, required)

  xl <- readxl::read_excel(viz[['location']])
  return(xl)
}

#' \code{readData.RDS} reads an R object saved as an RDS.
#'
#' @rdname readData
#' @export
readData.rds <- function(viz){
  required <- c("location")
  checkRequired(viz, required)

  rds <- readRDS(viz[['location']])
  return(rds)
}

#' \code{readData.filepath} returns the file path or null if location is not set
#'
#' @rdname readData
#' @export
readData.filepath <- function(viz){
  # returns NULL if missing
  return(viz[['location']])
}

#' \code{readData.parameter} returns the file path or null if location is not set
#'
#' @rdname readData
#' @export
readData.parameter <- function(viz){
  viz <- viz[names(viz)[!(names(viz) %in% c("id","block"))]]
  class(viz) <- 'list'
  return(viz)
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
  required <- c("id", "location", "mimetype")
  checkRequired(viz, required)

  filepaths <- dir(viz[['location']], full.names=TRUE)

  #create new vizlab object (one list element for each file)
  viz_lists <- lapply(filepaths, function(fp, id, mimetype){
    v <- list(id=id, location=fp, mimetype=mimetype)
    v <- as.viz(v)
    v <- as.reader(v)
    return(v)
  }, id=viz[['id']], mimetype=viz[['mimetype']])

  data_list <- lapply(viz_lists, readData)

  return(data_list)
}

#' \code{readData.txt} returns contents of file as character
#'
#' @rdname readData
#' @export
readData.txt <- function(viz){
  required <- c("location")
  checkRequired(viz, required)

  if(!requireNamespace('readr', quietly = TRUE)) stop("package readr is required for readData.txt")

  txt <- readr::read_file(viz[['location']])
  return(txt)
}

#' \code{readData.markdown} reads and renders markdown
#'
#' Can either be a single markdown file, specified with mimetype: text/markdown
#' or markdown contained in yaml with mimetype: text/yaml
#'
#' If simple markdown is used, return will be the rendered html, otherwise it
#' will be a list of html fragments named with the keys from the yaml.
#'
#' @rdname readData
#' @export
readData.md <- function(viz) {
  required <- c("location", "mimetype")
  checkRequired(viz, required)

  mimetype <- viz[['mimetype']]
  html <- NULL
  if (is.null(mimetype) || mimetype == "text/yaml") {
    yaml <- readData.yaml(viz)
    html <- rapply(yaml, function(x) {
      x <- handleMarkdown(x)
      return(x)
    }, how = "replace", classes = "character")
  } else {
    md <- readData.txt(viz)
    html <- handleMarkdown(md)
  }
  return(html)
}

#' \code{readData.inline} reads data directly from inline yaml
#'
#' @rdname readData
#' @return single value or list of values
#' @export
readData.inline <- function(viz) {
  required <- c("data")
  checkRequired(viz, required)

  data <- viz[["data"]]
  return(data)
}

#' \code{readData.json} reads json files
#'
#' @rdname readData
#' @export
readData.json <- function(viz) {
  
  has_jsonlite <- requireNamespace('jsonlite', quietly=TRUE)
  if(!has_jsonlite) { 
    stop("jsonlite package is required to read JSON data (mimetype='application/javascript', reader='json')")
  }
  
  required <- c("location")
  checkRequired(viz, required)
  
  data <- jsonlite::fromJSON(viz[['location']])
  return(data)
}

#' \code{readData.shp} reads shapefiles 
#' 
#' assumes only one .shp file in zip folder
#' 
#' @rdname readData
#' @export
readData.shp <- function(viz){
  required <- c('location')
  checkRequired(viz, required)
  
  has_rgdal <- requireNamespace('rgdal', quietly = TRUE)
  if(!has_rgdal) { 
    stop("rgdal package is required to read .shp files (mimetype='application/zip', reader='shp')")
  }
  
  shp.path <- file.path(tempdir(), 'tmp')
  if (!dir.exists(shp.path)){
    dir.create(shp.path)
  }
  
  unzip(viz[['location']], exdir = shp.path)
  layer <- tools::file_path_sans_ext(list.files(shp.path, pattern='*.shp'))[1]
  data.out <- rgdal::readOGR(shp.path, layer=layer, verbose=FALSE)
  unlink(shp.path, recursive = TRUE)
  return(data.out)
}

#' \code{readData.svg_map} reads svg
#' 
#' @rdname readData
#' @export
readData.svg <- function(viz){
  required <- c('location') 
  checkRequired(viz, required)
  
  svg <- xml2::read_xml(viz[['location']])
  return(svg)
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
  reader <- viz[['reader']]
  if (is.null(reader)) {
    mimetype <- viz[['mimetype']]
    reader <- lookupMimetype(mimetype)

    if (length(reader) == 0) {
      warning('Could not find specific readData method for viz.id=', id,
              ', mimetype=', mimetype, '; returning filepath.',
              ' Specify reader to override.')
      reader <- "filepath"
    }
  }
  class(viz) <- c(reader, "reader", class(viz))
  return(viz)
}
