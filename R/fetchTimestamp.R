#' Update timestamp file if needed
#' 
#' This function should be called from the generic, \code{fetchTimestamp()}.
#' Update timestamp file if the local copy of a file is out of date relative to 
#' a remote copy. The need for a timestamp file, and the method for finding the 
#' timestamp, will be determined by the metadata in viz.yaml for the item.
#' 
#' @param viz.id the identifier for this data item in viz.yaml
#' @param ... other args passed to fetchTimestamp methods
#'   
#' @export
fetchTimestamp <- function(viz.id, ..., outfile) UseMethod("fetchTimestamp")

#' @param data.info content information for this viz.id from the viz.yaml
#' @param old.timestamp the current timestamp, or NA if unavailable
#' @param outfile the filename where the new timestamp should be saved
#'   
#' @rdname fetchTimestamp
#' @export
fetchTimestamp.default <- function(viz.id, data.info, old.timestamp, ..., outfile) {
  # get the fetching information for this data ID from viz.yaml
  data.info <- getContentInfo(viz.id, block='fetch')
  class(viz.id) <- data.info$fetcher # routes subsequent calls to fetchTimestamp
  
  # get the locally stored timestamp if it exists
  old.timestamp <- if(file.exists(outfile)) {
    tryCatch({
      old.timestamp.chr <- readLines(outfile)
      as.POSIXct(old.timestamp.chr)
    }, error=function(e) NA, warning=function(w) NA)
  } else {
    NA
  }
  
  # call the fetchTimestamp method applicable to this fetcher
  invisible(fetchTimestamp(viz.id=viz.id, data.info=data.info, old.timestamp=old.timestamp, ..., outfile=outfile))
}

#' \code{fetchTimestamp.sciencebase} gets the file timestamp from ScienceBase.
#' 
#' @rdname fetchTimestamp
#' @export
fetchTimestamp.sciencebase <- function(viz.id, data.info, old.timestamp, ..., outfile) {
  # require sbtools package
  if(!requireNamespace('sbtools', quietly = TRUE)) stop("package sbtools is required for fetchTimestamp.sciencebase")
  
  # try to get the timestamp from sciencebase. if we can't get it, give a
  # warning and leave the outfile as it was
  new.timestamp <- tryCatch({
    authRemote('sciencebase')
    sb.info <- sbtools::item_get(data.info$remoteItemId)
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
    writeTimestamp(new.timestamp, outfile)
  } 
  
  invisible()
}


#' Write a timestamp file with the conventions used by fetchTimestamp
#' 
#' Should only be used outside the vizlab package if the developer is writing a 
#' new data + timestamp fetcher
#' 
#' @param new.timestamp the new timestamp to write to file
#' @param outfile the filename where the new timestamp should be saved
#'   
#' @export
writeTimestamp <- function(new.timestamp, outfile) {
  writeLines(format(new.timestamp, "%Y-%m-%d %H:%M:%S %Z"), outfile)
}
