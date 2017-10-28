#' Update timestamp file if needed
#'
#' `fetchTimestamp` updates a timestamp file if the local copy of a file is out
#' of date relative to the data source. In practice, the desired definition of
#' 'out of date' differs widely among data sources, sizes, and projects, so
#' `fetchTimestamp` is a generic function with a method specific to each
#' `fetcher`. Every `fetchTimestamp` method should write a timestamp file, or
#' not, as required to achieve the desired `make` behavior. See
#' [fetchTimestampMethods] and [fetchTimestampHelpers] for assistance in
#' defining new fetchTimestamp methods. Also consider preferences.yaml and
#' [exceededTimeToLive()] for additional options for managing data fetches.
#'
#' @param viz the identifier for a fetch item in viz.yaml
#' @return It doesn't matter what a fetchTimestamp method returns. It matters a
#'   lot more whether it creates/updates the timestamp file or not.
#' @export
#' @md
fetchTimestamp <- function(viz) UseMethod("fetchTimestamp")

#' Routes the timestamp fetching to a more specific fetcher.
#'
#' This particular method is not really a timestamp fetcher, just an
#' intermediary, and so is not documented in [fetchTimestamp()].
#' @inheritParams fetchTimestamp
#' @export
fetchTimestamp.character <- function(viz) {
  # get the fetching information for this data ID from viz.yaml
  viz <- as.viz(viz)
  viz <- as.fetcher(viz)

  # call the fetchTimestamp method applicable to this fetcher
  fetchTimestamp(viz)
}

#' `fetchTimestamp.sciencebase` gets the file timestamp from ScienceBase.
#'
#' @importFrom httr HEAD
#' @importFrom httr headers
#' @importFrom httr parse_http_date
#' @rdname fetchTimestamp
#' @export
fetchTimestamp.sciencebase <- function(viz) {
  # require sbtools package
  if(!requireNamespace('sbtools', quietly = TRUE)) stop("package sbtools is required for fetchTimestamp.sciencebase")

  required <- c("remoteItemId", "remoteFilename", "location")
  checkRequired(viz, required)
  remoteItemId <- viz[['remoteItemId']]
  remoteFilename <- viz[['remoteFilename']]

  old.timestamp <- readTimestamp(viz)
  # try to get the timestamp from sciencebase. if we can't get it, give a
  # warning and leave the outfile as it was
  new.timestamp <- tryCatch({
    authRemote('sciencebase')
    sb.info <- sbtools::item_get(remoteItemId)
    files.info <- sb.info$files
    file.info <- files.info[[which(sapply(files.info, function(fileinf) { fileinf$name == remoteFilename }))]]
    as.POSIXct(file.info$dateUploaded, format = '%Y-%m-%dT%H:%M:%SZ', tz='UTC')
  }, error=function(e) {
    warning(e$message, call.=FALSE)
    warning("could not retrieve timestamp from ScienceBase; local timestamp information will be left as-is", call.=FALSE)
    NA
  })

  # write the new timestamp to the file
  if(!is.na(new.timestamp) && (is.na(old.timestamp) || (new.timestamp != old.timestamp))) {
    writeTimestamp(new.timestamp, viz)
  }
  
  # return nothing
  invisible()
}

#' `fetchTimestamp.file` creates a timestamp file once, with file metadata and
#' file contents that both match the timestamp of the viz data file. If the
#' timestamp file already exists, it is only modified if the data file is
#' updated.
#'
#' @rdname fetchTimestamp
#' @export
fetchTimestamp.file <- function(viz) {
  
  # get the old timestamp
  old.timestamp <- readTimestamp(viz)
  new.timestamp <- if(file.exists(viz$location)) file.mtime(viz$location) else NA
  
  # pretty much always write something, unless both old and new timestamps exist
  # and they're equal
  if(is.na(old.timestamp) || is.na(new.timestamp) || (new.timestamp > old.timestamp)) {
    writeTimestamp(new.timestamp, viz, timestamp.mtime=new.timestamp)
  }
  
  # return nothing
  invisible()
}

#' `fetchTimestamp.url` checks a URL for a timestamp. The URL headers must
#' include a 'last-modified' field; otherwise, this method breaks and you should
#' write your own for the specific URL in question.
#'
#' @rdname fetchTimestamp
#' @export
fetchTimestamp.url <- function(viz) {

  # URL will be specified in viz.yaml as remoteURL
  required <- c("remoteURL")
  checkRequired(viz, required)
  url <- viz$remoteURL
  
  # read the URL header and the current timestamp file
  new.timestamp <- headers(HEAD(url))[['last-modified']]
  old.timestamp <- readTimestamp(viz)
  
  # Parse the new.timestamp if available. tag will be NULL if the last-modified
  # tag doesn't exist
  if(is.null(new.timestamp)) {
    stop("'last-modified' field not found in headers of ", url)
  } else {
    new.timestamp <- parse_http_date(new.timestamp)
    attr(new.timestamp, "tzone") <- "UTC"
  }

  # write the new timestamp to the file
  if(!is.na(new.timestamp) && (is.na(old.timestamp) || (new.timestamp != old.timestamp))) {
    writeTimestamp(new.timestamp, viz)
  }
  
  # return nothing
  invisible()
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
