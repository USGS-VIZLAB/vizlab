#' Ensure data is on the local file system
#' 
#' This function should be called from the generic, \code{fetchData()}. Fetch 
#' data from remote locations and/or identify a copy on the local file system. 
#' These functions assume that downloading must be attempted if the raw data 
#' file is remote; the makefile is where caching occurs if possible.
#' 
#' @param viz.id the identifier for this data item in viz.yaml
#'   
#' @export
fetchData <- function(viz.id, ...) UseMethod("fetchData")

#' @export
fetchData.default <- function(viz.id, ...) {
  # get the fetching information for this data ID from viz.yaml
  data.info <- getContentInfo(viz.id, block='fetch', no.match='NA')
  if(!exists('fetcher', data.info)) data.info$fetcher <- 'file'
  class(viz.id) <- data.info$fetcher # routes subsequent calls to fetchData
  
  # if this id type doesn't need to be fetched, run the empty fetcher now
  if(data.info$fetcher %in% c('file')) invisible(fetchData(viz.id, data.info))
  
  # create the file directory if it does not yet exist
  fetched.dir <- dirname(data.info$location)
  if(!dir.exists(fetched.dir)) dir.create(fetched.dir, recursive=TRUE)
  
  # call the fetchData method applicable to this fetcher
  invisible(fetchData(viz.id, data.info))
}

#' \code{fetchData.file} currently does nothing. It exists to communicate that
#' the .file type is an option for data items.
#' 
#' @rdname fetchData
#' @export
fetchData.file <- function(viz.id, ...) {
  invisible()
}

#' \code{fetchData.sciencebase} downloads a file from ScienceBase.
#' 
#' @import sbtools
#' @param data.info content information for this viz.id from the viz.yaml
#' @rdname fetchData
#' @export
fetchData.sciencebase <- function(viz.id, data.info) {
  # check for properly formatted data.info values
  if(!(exists('remoteFilename', data.info)) || length(data.info$remoteFilename) != 1)
    stop('expecting exactly 1 remoteFilename per data item')
  
  # authenticate
  authRemote('sciencebase')
  
  # download the file, overwriting if it already exists (this can be simple
  # because caching is handled by the makefiles, and the yaml requires exactly
  # one file per data item)
  if(any(!fexists)){
    item_file_download(
      sb_id=data.info$remoteItemId, 
      names=data.info$remoteFilename, 
      destinations=data.info$location,
      overwrite_file=TRUE)
  }
  
  invisible()
}
