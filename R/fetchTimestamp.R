#' Update timestamp file if needed
#'
#' This function should be called from the generic, \code{fetchTimestamp()}.
#' Update timestamp file if the local copy of a file is out of date relative to
#' a remote copy. The need for a timestamp file, and the method for finding the
#' timestamp, will be determined by the metadata in viz.yaml for the item.
#'
#' @param viz the identifier for this data item in viz.yaml
#' @importFrom httr HEAD
#' @importFrom httr headers
#' @importFrom httr parse_http_date
#' @return logical \code{TRUE} if a timestamp does not exist or has changed,
#' \code{FALSE} if the timestamps are identical.
#' @export
fetchTimestamp <- function(viz) UseMethod("fetchTimestamp")

#' \code{fetchTimestamp.character} fetches timestamp for a given id
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
    writeTimestamp(new.timestamp, viz)
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' Does nothing - \code{make} already checks timestamps of local files
#'
#' @rdname fetchTimestamp
#' @export
fetchTimestamp.file <- function(viz) {
  old.timestamp <- readOldTimestamp(viz)
  if(is.na(old.timestamp) && file.exists(viz$location)) {
    new.timestamp <- file.mtime(viz$location)
    writeTimestamp(new.timestamp, viz)
    old.timestamp.loc <- locateTimestampFile(viz$id)
    Sys.setFileTime(old.timestamp.loc, new.timestamp)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' check a URL for timestamp
#'
#' @rdname fetchTimestamp
#' @export
fetchTimestamp.url <- function(viz) {

  #URL will be specified in viz.yaml as remoteURL
  url <- viz$remoteURL
  new.timestamp <- headers(HEAD(url))[['last-modified']]
  old.timestamp <- readOldTimestamp(viz)
  #tag will be NULL if the last-modified tag doesn't exist
  if(is.null(new.timestamp)){
    new.timestamp <- Sys.time()
  }else{
    new.timestamp <- parse_http_date(new.timestamp)
  }

  # write the new timestamp to the file
  if(!is.na(new.timestamp) && (is.na(old.timestamp) || (new.timestamp != old.timestamp))) {
    writeTimestamp(new.timestamp, viz)
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' \code{fetchTimestamp.fetcher} superclass method catches missing
#' implementation
#'
#' @rdname fetchTimestamp
#' @export
fetchTimestamp.fetcher <- function(viz){
  # require the availability of a fetcher-specific method by giving an error if
  # we arrive here. this error is complemented by a near-identical error in
  # needsTimestamp (beneath createMakefiles); we're minimizing time to
  # failure+understanding by giving this error in both places
  stop(paste0("fetchTimestamp.", viz$fetcher, " must be implemented for ",
              viz$id, ", probably in an R file in 'scripts:'"))
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
writeTimestamp <- function(new.timestamp, viz) {
  outfile <- locateTimestampFile(viz[["id"]])
  writeLines(format(new.timestamp, "%Y-%m-%d %H:%M:%S %Z"), outfile)
}

#' Where is the timestamp file
#'
#' @param id viz id needing timestamp
#' @return character vector location of timestamp file
locateTimestampFile <- function(id) {
  timestampDir <- "./vizlab/make/timestamps"
  timestampFile <- file.path(timestampDir, id)
  return(timestampFile)
}

#' Read an old timestamp for viz
#'
#' @param viz vizlab object
#' @return POSIXct timestamp for vizlab object
readOldTimestamp <- function(viz) {
  timestamp.file <- locateTimestampFile(viz$id)
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
