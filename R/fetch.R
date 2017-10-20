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
  viz <- as.fetcher(viz)
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
  required <- c("remoteURL", "location")
  checkRequired(viz, required)

  remoteURL <- viz[['remoteURL']]
  outfile <- viz[['location']]
  #check that we have one URL
  if(missing('remoteURL') || length(remoteURL) != 1)
    stop('expecting exactly 1 remoteURL per data item')

  httr::GET(remoteURL, httr::write_disk(outfile, overwrite=TRUE))

  invisible()
}

#' \code{fetch.usgs_watermark} grabs the USGS watermark from the 
#' 
#' @rdname fetch
#' @export
fetch.usgs_watermark <- function(viz) {
  required <- c('location')
  checkRequired(viz, required)
  
  g.watermark <- xml2::xml_new_root('g', id = 'watermark-group', onclick = "vizlab.clicklink('https://www2.usgs.gov/water/')")
  usgs.d = "m234.95 15.44v85.037c0 17.938-10.132 36.871-40.691 36.871-27.569 0-40.859-14.281-40.859-36.871v-85.04h25.08v83.377c0 14.783 6.311 20.593 15.447 20.593 10.959 0 15.943-7.307 15.943-20.593v-83.377h25.08m40.79 121.91c-31.058 0-36.871-18.27-35.542-39.03h25.078c0 11.462 0.5 21.092 14.282 21.092 8.472 0 12.62-5.482 12.62-13.618 0-21.592-50.486-22.922-50.486-58.631 0-18.769 8.968-33.715 39.525-33.715 24.42 0 36.543 10.963 34.883 36.043h-24.419c0-8.974-1.492-18.106-11.627-18.106-8.136 0-12.953 4.486-12.953 12.787 0 22.757 50.493 20.763 50.493 58.465 0 31.06-22.75 34.72-41.85 34.72m168.6 0c-31.06 0-36.871-18.27-35.539-39.03h25.075c0 11.462 0.502 21.092 14.285 21.092 8.475 0 12.625-5.482 12.625-13.618 0-21.592-50.494-22.922-50.494-58.631 0-18.769 8.969-33.715 39.531-33.715 24.412 0 36.536 10.963 34.875 36.043h-24.412c0-8.974-1.494-18.106-11.625-18.106-8.144 0-12.955 4.486-12.955 12.787 0 22.757 50.486 20.763 50.486 58.465 0 31.06-22.75 34.72-41.85 34.72m-79.89-46.684h14.76v26.461l-1.229 0.454c-3.816 1.332-8.301 2.327-12.453 2.327-14.287 0-17.943-6.645-17.943-44.177 0-23.256 0-44.348 15.615-44.348 12.146 0 14.711 8.198 14.933 18.107h24.981c0.198-23.271-14.789-36.043-38.42-36.043-41.021 0-42.52 30.724-42.52 60.954 0 45.507 4.938 63.167 47.12 63.167 9.784 0 25.36-2.211 32.554-4.18 0.436-0.115 1.212-0.596 1.212-1.216v-59.598h-38.612v18.09"
  wave.d = "m48.736 55.595l0.419 0.403c11.752 9.844 24.431 8.886 34.092 2.464 6.088-4.049 33.633-22.367 49.202-32.718v-10.344h-116.03v27.309c7.071-1.224 18.47-0.022 32.316 12.886m43.651 45.425l-13.705-13.142c-1.926-1.753-3.571-3.04-3.927-3.313-11.204-7.867-21.646-5.476-26.149-3.802-1.362 0.544-2.665 1.287-3.586 1.869l-28.602 19.13v34.666h116.03v-24.95c-2.55 1.62-18.27 10.12-40.063-10.46m-44.677-42.322c-0.619-0.578-1.304-1.194-1.915-1.698-13.702-10.6-26.646-5.409-29.376-4.116v11.931l6.714-4.523s10.346-7.674 26.446 0.195l-1.869-1.789m16.028 15.409c-0.603-0.534-1.214-1.083-1.823-1.664-12.157-10.285-23.908-7.67-28.781-5.864-1.382 0.554-2.7 1.303-3.629 1.887l-13.086 8.754v12.288l21.888-14.748s10.228-7.589 26.166 0.054l-0.735-0.707m68.722 12.865c-4.563 3.078-9.203 6.203-11.048 7.441-4.128 2.765-13.678 9.614-29.577 2.015l1.869 1.797c0.699 0.63 1.554 1.362 2.481 2.077 11.418 8.53 23.62 7.303 32.769 1.243 1.267-0.838 2.424-1.609 3.507-2.334v-12.234m0-24.61c-10.02 6.738-23.546 15.833-26.085 17.536-4.127 2.765-13.82 9.708-29.379 2.273l1.804 1.729c0.205 0.19 0.409 0.375 0.612 0.571l-0.01 0.01 0.01-0.01c12.079 10.22 25.379 8.657 34.501 2.563 5.146-3.436 12.461-8.38 18.548-12.507l-0.01-12.165m0-24.481c-14.452 9.682-38.162 25.568-41.031 27.493-4.162 2.789-13.974 9.836-29.335 2.5l1.864 1.796c1.111 1.004 2.605 2.259 4.192 3.295 10.632 6.792 21.759 5.591 30.817-0.455 6.512-4.351 22.528-14.998 33.493-22.285v-12.344"
  xml2::xml_add_child(g.watermark, 'path', d = usgs.d, id = 'usgs-watermark-text', 'class' = 'watermark')
  xml2::xml_add_child(g.watermark, 'path', d = wave.d, id = 'usgs-watermark-wave', 'class' = 'watermark')
  xml2::write_xml(x = g.watermark, viz[['location']])
}

#' \code{fetch.none} skips this fetch
#'
#' @rdname fetch
#' @export
fetch.none <- function(viz) {
  invisible()
}

#' \code{fetchTimestamp.none} skips this fetchTimestamp
#'
#' @rdname fetch
#' @export
fetchTimestamp.none <- function(viz) {
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
