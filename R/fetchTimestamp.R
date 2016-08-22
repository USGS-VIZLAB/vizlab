#' Update timestamp file if needed
#'
#' This function should be called from the generic, \code{fetchTimestamp()}.
#' Update timestamp file if the local copy of a file is out of date relative to
#' a remote copy. The need for a timestamp file, and the method for finding the
#' timestamp, will be determined by the metadata in viz.yaml for the item.
#'
#' @param viz the identifier for this data item in viz.yaml
#'
#' @export
fetchTimestamp <- function(viz) UseMethod("fetchTimestamp")

#'
#' @rdname fetchTimestamp
#' @export
fetchTimestamp.character <- function(viz) {
  # get the fetching information for this data ID from viz.yaml
  viz <- as.viz(viz)
  viz <- as.fetcher(viz)

  # call the fetchTimestamp method applicable to this fetcher
  fetchTimestamp(viz)
}

#' \code{fetchTimestamp.sciencebase} gets the file timestamp from ScienceBase.
#'
#' @rdname fetchTimestamp
#' @export
fetchTimestamp.sciencebase <- function(viz) {
  # require sbtools package
  if(!requireNamespace('sbtools', quietly = TRUE)) stop("package sbtools is required for fetchTimestamp.sciencebase")

  required <- c("remoteItemId", "remoteFilename", "location")
  checkRequired(viz, required)
  remoteItemId <- viz[['remoteItemId']]
  remoteFilename <- viz[['remoteFilename']]

  old.timestamp <- readOldTimestamp(viz)
  # try to get the timestamp from sciencebase. if we can't get it, give a
  # warning and leave the outfile as it was
  new.timestamp <- tryCatch({
    authRemote('sciencebase')
    sb.info <- sbtools::item_get(remoteItemId)
    files.info <- sb.info$files
    file.info <- files.info[[which(sapply(files.info, function(fileinf) { fileinf$name == remoteFilename }))]]
    as.POSIXct(file.info$dateUploaded, format = '%Y-%m-%dT%H:%M:%SZ')
  }, error=function(e) {
    warning(e$message, call.=FALSE)
    warning("could not retrieve timestamp from ScienceBase; local timestamp information will be left as-is", call.=FALSE)
    NA
  })

  # write the new timestamp to the file
  if(!is.na(new.timestamp) && (is.na(old.timestamp) || (new.timestamp != old.timestamp))) {
    writeTimestamp(new.timestamp, locateTimestampFile(viz[['id']]))
  }

  invisible()
}

#' Has a file changed
#'
#' @rdname fetchTimestamp
#' @export
fetchTimestamp.file <- function(viz) {
  # TODO this may need to be implemented
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

#' Where is the timestamp file
#'
#' @param id viz id needing timestamp
#' @return character vector location of timestamp file
locateTimestampFile <- function(id) {
  # TODO standardize timestamp file location
}

#' Read an old timestamp for viz
#'
#' @param viz vizlab object
#' @return POSIXct timestamp for vizlab object
readOldTimestamp <- function(viz) {
  timestamp.file <- locateTimestampFile(viz)
  # get the locally stored timestamp if it exists
  old.timestamp <- if(file.exists(timestamp.file)) {
    tryCatch({
      old.timestamp.chr <- readLines(timestamp.file)
      as.POSIXct(old.timestamp.chr)
    }, error=function(e) NA, warning=function(w) NA)
  } else {
    NA
  }
  return(old.timestamp)
}
