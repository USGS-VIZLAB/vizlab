#' Update timestamp file if needed
#' 
#' This function should be called from the generic, \code{fetchTimestamp()}.
#' Update timestamp file if the local copy of a file is out of date relative to 
#' a remote copy. The need for a timestamp file, and the method for finding the 
#' timestamp, will be determined by the metadata in viz.yaml for the item.
#' 
#' @param viz.id the identifier for this data item in viz.yaml
#' @param data.info content information for this viz.id from the viz.yaml
#'   
#' @export
fetchTimestamp <- function(viz.id, ...) UseMethod("fetchTimestamp")

#' @export
fetchTimestamp.default <- function(viz.id, ...) {
  # get the fetching information for this data ID from viz.yaml
  data.info <- getContentInfo(viz.id, block='fetch')
  class(viz.id) <- data.info$fetcher # routes subsequent calls to fetchTimestamp
  
  # if this id type doesn't need a timestamp file, exit now
  if(data.info$fetcher %in% c('file')) invisible(fetchTimestamp(viz.id, data.info))
  
  # get the locally stored timestamp if it exists
  timestamp.dir <- paste0('vizlab/make/timestamps')
  timestamp.file <- paste0(timestamp.dir, '/', viz.id, '.txt')
  old.timestamp <- if(file.exists(timestamp.file)) {
    tryCatch({
      old.timestamp.chr <- readLines(timestamp.file)
      as.POSIXct(old.timestamp.chr)
    }, error=function(e) NA, warning=function(w) NA)
  } else {
    NA
  }
  
  # create the timestamp file directory if it does not yet exist
  if(!dir.exists(timestamp.dir)) dir.create(timestamp.dir)
  
  # call the fetchTimestamp method applicable to this fetcher
  invisible(fetchTimestamp(viz.id, data.info, old.timestamp=old.timestamp, timestamp.file=timestamp.file))
}

#' \code{fetchTimestamp.sciencebase} gets the file timestamp from ScienceBase.
#' 
#' @rdname fetchTimestamp
#' @export
fetchTimestamp.sciencebase <- function(viz.id, data.info, old.timestamp, timestamp.file) {
  
  # try to get the timestamp from sciencebase. if we can't get it, give a
  # warning and leave the timestamp.file as it was
  new.timestamp <- tryCatch({
    authRemote('sciencebase')
    sb.info <- item_get(data.info$remoteItemId)
    files.info <- sb.info$files
    file.info <- files.info[[which(sapply(files.info, function(fileinf) { fileinf$name == data.info$remoteFilename }))]]
    as.POSIXct(file.info$dateUploaded, format='%Y-%m-%dT%H:%M:%SZ')
  }, error=function(e) {
    warning(e$message, call.=FALSE)
    warning("could not retrieve timestamp from ScienceBase; local timestamp information will be left as-is", call.=FALSE)
    NA
  })
  
  # write the new timestamp to the file
  if(!is.na(new.timestamp) && (is.na(old.timestamp) || (new.timestamp != old.timestamp))) {
    writeTimestamp(new.timestamp, timestamp.file)
  } 
  
  invisible()
}


#' Write a timestamp file with the conventions used by fetchTimestamp
#' 
#' Should only be used outside the vizlab package if the developer is writing a 
#' new data fetcher
#' 
#' @export
writeTimestamp <- function(new.timestamp, timestamp.file) {
  writeLines(format(new.timestamp, "%Y-%m-%d %H:%M:%S %Z"), timestamp.file)
}
