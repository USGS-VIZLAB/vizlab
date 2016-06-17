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
  # explain the problem if we're headed for infinite recursion
  if(class(viz.id) != 'character') 
    stop('could not find fetchData method for viz.id=', viz.id, ', fetcher=', class(viz.id))
  
  # get the fetching information for this data ID from viz.yaml
  data.info <- getContentInfo(viz.id, block='fetch', no.match='NA')
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
  # this is using the :: operator as suggested by
  # https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Suggested-packages
  sbtools::item_file_download(
    sb_id=data.info$remoteItemId,
    names=data.info$remoteFilename,
    destinations=data.info$location,
    overwrite_file=TRUE)

  invisible()
}

#' \code{fetchData.url} downloads a file from the specified URL
#' 
#' @param viz.id the identifier for this data item in viz.yaml
#' @param data.info content information for this viz.id from the viz.yaml
#' @rdname fetchData
#' @export
fetchData.url <- function(viz.id, data.info){
  
  #check that we have one URL
  if(!(exists('remoteURL', data.info)) || length(data.info$remoteURL) != 1)
    stop('expecting exactly 1 remoteURL per data item')
  
  httr::GET(data.info$remoteURL, 
            httr::write_disk(data.info$location, overwrite=TRUE))
  
  invisible()
}
