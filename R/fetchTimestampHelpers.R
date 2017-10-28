# Functions in this file are used by the native fetchTimestamp methods and may
# also be useful to users and creators of custom fetchTimestamp methods

#### fetchTimestampMethods ####

#' Functions to use as custom fetchTimestamp methods.
#'
#' These functions serve common use cases for timestamp fetching. See also
#' [fetchTimestampHelpers], which allows you to write still more customized
#' methods, and pre-defined [fetchTimestamp()] methods for several fetchers
#' supported by `vizlab`.
#'
#' `alwaysCurrent` - writes a timestamp file just once, the first time the data
#' file is present when fetchTimestamp is called. Edits the modified time,
#' mtime, of the timestamp file to equal the modified time of the data file, so
#' that on subsequent builds the data file is always up to date relative to the
#' timestamp file. REFETCHES MAY STILL OCCUR if there are changes to the fetch
#' item's viz.yaml chunk, scripts, or other dependencies.
#'
#' @name fetchTimestampMethods
#' @md
#' @examples
#' fetchTimestamp.myfetcher <- alwaysCurrent
#' fetchTimestamp.myotherfetcher <- neverCurrent
#' @export
alwaysCurrent <- fetchTimestamp.file

#' `neverCurrent` - always writes a timestamp file for the current date and
#' time. This means that whenever the timestamp is checked during the make
#' process, the data file will be interpreted as out of date.
#'
#' @rdname fetchTimestampMethods
#' @inheritParams fetchTimestamp
#' @export
neverCurrent <- function(viz) {
  writeTimestamp(Sys.time(), viz)
  
  invisible()
}


#### fetchTimestampHelpers ####

#' Methods to assist in writing and debugging custom fetchTimestamp methods
#'
#' These functions are used within vizlab [fetchTimestamp()] methods and may be
#' useful within custom methods as well.
#'
#' @name fetchTimestampHelpers
#' @md
#' @param viz a viz item (e.g., from as.viz)
#' @param id the viz id - should be the same as for the viz in the calling
#'   function
#' @examples
#' \dontrun{
#' # this custom method probably won't do what you want,
#' # just shows how to use the helper functions
#' fetchTimestamp.myfetcher <- function(viz) {
#'   timestamp.file <- locateTimestampFile(viz$id)
#'   old.timestamp <- readTimestamp(viz)
#'   new.timestamp <- getOnlineTimestamp() # made-up function, not implemented
#'   if(!file.exists(timestamp.file) || old.timestamp < new.timestamp) {
#'     writeTimestamp(new.timestamp, viz)
#'   }
#'   invisible()
#' }
#' }
NULL

#' `urlFetcher` - Creates a temporary viz object that has `fetcher: url` and a
#' custom remoteURL that was probably derived just now in the calling method.
#' The results of this function can be passed to [fetch()] or
#' [fetchTimestamp()].
#'
#' @rdname fetchTimestampHelpers
#' @md
#' @param location the fetched data file location - should be the same as for
#'   the viz in the calling function. May be NA if you're only passing the
#'   output to [fetchTimestamp()]; required for [fetch()].
#' @param remoteURL the new URL to use for fetching the data and/or timestamp
#'   (assuming [fetch()] or [fetchTimestamp()] is called on the results of this
#'   function)
#' @export
#' @examples
#' \dontrun{
#' fetchTimestamp.myfetcher <- function(viz) {
#'   url <- constructURLFromViz(viz)
#'   urlLastModified(viz$id, remoteURL=url)
#' }
#' }
urlFetcher <- function(id, location=NA, remoteURL) {
  # create a dummy viz that has a url fetcher, then call fetchTimestamp.url
  fetchviz <- list(
    id = id,
    location = location,
    remoteURL = remoteURL,
    fetcher = "url"
  )
  return(as.fetcher(as.viz(fetchviz)))
}

#' `writeTimestamp` writes a timestamp file with the conventions used by
#' fetchTimestamp - should be used within custom timestamp fetchers to ensure
#' the expected format is used.
#'
#' @rdname fetchTimestampHelpers
#' @md
#' @param new.timestamp the new timestamp to write to file
#' @param timestamp.mtime the timestamp to assign to the timestamp file's
#'   metadata (using Sys.setFileTime - sets the mtime=modified time property);
#'   NA leaves the metadata alone
#' @export
writeTimestamp <- function(new.timestamp, viz, timestamp.mtime=NA) {
  # find the old file if present
  viz <- as.viz(viz)
  outfile <- locateTimestampFile(viz[["id"]])
  
  # write the contents of the file
  str.timestamp <- formatTimestamp(new.timestamp)
  writeLines(str.timestamp, outfile)
  
  # edit the metadata (mtime) for the file if specially requested
  if(!is.na(timestamp.mtime)) {
    attr(timestamp.mtime, 'tzone') <- 'UTC'
    Sys.setFileTime(outfile, timestamp.mtime)
  }
}

#' `formatTimestamp` formats a POSIXct timestamp according to vizlab timestamp
#' conventions - converts a timestamp to UTC and creates a character string with
#' format '%Y-%m-%d %H:%M:%S UTC'.
#'
#' @rdname fetchTimestampHelpers
#' @md
#' @param timestamp POSIXct timestamp to format into a character string
#' @export
formatTimestamp <- function(timestamp) {
  if(is.na(timestamp)) return('NA')
  if(!('POSIXct' %in% class(timestamp))) stop('timestamp must be POSIXct')
  attr(timestamp, 'tzone') <- 'UTC'
  format(timestamp, "%Y-%m-%d %H:%M:%OS5 %Z") # presents UTC even though that info will technically be ignored on readTimestamp
}

#' `locateTimestampFile` returns a relative file path to the timestamp file for
#' an id.
#'
#' @rdname fetchTimestampHelpers
#' @md
#' @export
locateTimestampFile <- function(id) {
  timestampFile <- file.path("vizlab/remake/timestamps", sprintf('%s.txt', id))
  return(timestampFile)
}

#' `readTimestamp` reads an old timestamp for a viz item, It returns NA if the
#' file is missing or a POSIXct timestamp if given (as text) within the
#' timestamp file.
#'
#' @rdname fetchTimestampHelpers
#' @md
#' @export
readTimestamp <- function(viz) {
  # find the old file
  viz <- as.viz(viz)
  timestamp.file <- locateTimestampFile(viz[["id"]])
  
  # get the locally stored timestamp if it exists, return NA otherwise
  old.timestamp <- if(file.exists(timestamp.file)) {
    tryCatch({
      old.timestamp.chr <- readLines(timestamp.file)
      as.POSIXct(old.timestamp.chr, format="%Y-%m-%d %H:%M:%OS", tz="UTC") # assumes UTC; overrides anything written in the file
    }, error=function(e) stop('could not parse the timestamp at ', timestamp.file))
  } else {
    NA
  }
  return(old.timestamp)
}
