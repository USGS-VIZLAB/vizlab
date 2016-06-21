#' Ensure data is on the local file system
#' 
#' This function should be called from the generic, \code{fetchData()}. Fetch 
#' data from remote locations and/or identify a copy on the local file system. 
#' These functions assume that downloading must be attempted if the raw data 
#' file is remote; the makefile is where caching occurs if possible.
#' 
#' @param viz.id the identifier for this data item in viz.yaml
#' @param ... other arguments passed to fetchData methods
#' @param outfile the filename to which the fetched data should be written, if
#'   applicable
#'   
#' @export
fetchData <- function(viz.id, ..., outfile) UseMethod("fetchData")

#' @rdname fetchData
#' @export
fetchData.default <- function(viz.id, ..., outfile) {
  # explain the problem if we're headed for infinite recursion
  if(class(viz.id) != 'character') 
    stop('could not find fetchData method for viz.id=', viz.id, ', fetcher=', class(viz.id))
  
  # get the fetching information for this data ID from viz.yaml
  data.info <- getContentInfo(viz.id, block='fetch', no.match='NA')
  class(viz.id) <- data.info$fetcher # routes subsequent calls to fetchData

  # call the fetchData method applicable to this fetcher
  invisible(fetchData(viz.id=viz.id, ..., outfile=outfile))
}

#' \code{fetchData.file} currently does nothing. It exists to communicate that
#' the .file type is an option for data items.
#'
#' @rdname fetchData
#' @export
fetchData.file <- function(viz.id, ..., outfile) {
  invisible()
}

#' \code{fetchData.sciencebase} downloads a file from ScienceBase.
#' 
#' @param remoteItemId the ScienceBase hexadecimal ID of the item
#' @param remoteFilename the name of the file to download, as it is named on 
#'   ScienceBase
#'   
#' @rdname fetchData
#' @export
fetchData.sciencebase <- function(viz.id, remoteItemId, remoteFilename, ..., outfile) {
  # check for properly formatted args
  if(missing('remoteFilename') || length(remoteFilename) != 1)
    stop('expecting exactly 1 remoteFilename per data item')

  # require sbtools package
  if(!requireNamespace('sbtools', quietly = TRUE)) stop("package sbtools is required for fetchTimestamp.sciencebase")
  
  # authenticate
  authRemote('sciencebase')

  # download the file, overwriting if it already exists (this can be simple 
  # because caching is handled by the makefiles, and the yaml requires exactly 
  # one file per data item). this is using the :: operator as suggested by 
  # https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Suggested-packages
  sbtools::item_file_download(
    sb_id=remoteItemId,
    names=remoteFilename,
    destinations=outfile,
    overwrite_file=TRUE)

  invisible()
}

#' \code{fetchData.url} downloads a file from the specified URL.
#' 
#' @param remoteURL the URL from which to download the file
#'   
#' @rdname fetchData
#' @export
fetchData.url <- function(viz.id, remoteURL, ..., outfile) {
  
  #check that we have one URL
  if(missing('remoteURL') || length(remoteURL) != 1)
    stop('expecting exactly 1 remoteURL per data item')
  
  httr::GET(remoteURL, httr::write_disk(outfile, overwrite=TRUE))
  
  invisible()
}
