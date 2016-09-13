#' Ensure data is on the local file system
#'
#' This function should be called from the generic, \code{fetch()}. Fetch
#' data from remote locations and/or identify a copy on the local file system.
#' These functions assume that downloading must be attempted if the raw data
#' file is remote; the makefile is where caching occurs if possible.
#'
#' @param viz the identifier for this data item in viz.yaml
#'
#' @export
fetch <- function(viz) UseMethod("fetch")

#' @rdname fetch
#' @export
fetch.character <- function(viz) {
  # get the fetching information for this data ID from viz.yaml
  viz <- as.viz(viz)

  # Figure out if we need to bring this back
  # all.args <- getAutoargs(data.info, fun='write')

  viz <- as.fetcher(viz)
  # #check timestamp
  # if(!viz$fetchTimestamp || !fetchTimestamp(viz)){
  #   #don't fetch raw files, use already processed
  #   #or no updated file to fetch
  #   return()
  # }

  # call the fetch method applicable to this fetcher
  fetch(viz)
}

#' \code{fetch.file} currently does nothing. It exists to communicate that
#' the .file type is an option for data items.
#'
#' @rdname fetch
#' @export
fetch.file <- function(viz) {
  invisible()
}

#' \code{fetch.sciencebase} downloads a file from ScienceBase.
#'
#' @rdname fetch
#' @export
fetch.sciencebase <- function(viz) {
  required <- c("location", "remoteItemId", "remoteFilename")
  checkRequired(viz, required)

  outfile <- viz[['location']]
  remoteItemId <- viz[['remoteItemId']]
  remoteFilename <- viz[['remoteFilename']]

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

#' \code{fetch.url} downloads a file from the specified URL.
#'
#' @rdname fetch
#' @export
fetch.url <- function(viz) {
  if (all(c("remoteURL", "location") %in% names(viz))) {
    stop('must specify remoteURL and location for', viz[['id']])
  }
  remoteURL <- viz[['remoteURL']]
  outfile <- viz[['location']]
  #check that we have one URL
  if(missing('remoteURL') || length(remoteURL) != 1)
    stop('expecting exactly 1 remoteURL per data item')

  httr::GET(remoteURL, httr::write_disk(outfile, overwrite=TRUE))

  invisible()
}

### Set up fetcher class

#' Coerce vizlab object to a fetcher type
#'
#' @param viz vizlab object
#' @param ... not used, following convention
#' @return vizlab fetcher object
#' @export
as.fetcher <- function(viz, ...) {
  fetcher <- viz[['fetcher']]
  if (is.null(fetcher)) {
    # default to file
    fetcher <- "file"
  }
  class(viz) <- c(fetcher, "fetcher", class(viz))
  return(viz)
}
