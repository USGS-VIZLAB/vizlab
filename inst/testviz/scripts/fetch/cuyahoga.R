fetch.cuyahoga <- function(viz) {
  # this is a highly made-up function - you wouldn't do this in a viz, you'd
  # just copy in the file yourself. but pretend that viz$fetchargs$sourceloc is
  # actually a remote source location and that somebody might be modifying those
  # data
  checkRequired(viz, c('fetchargs', 'location'))
  file.copy(viz$fetchargs$sourceloc, viz$location)
  invisible()
}

# example fetcher that goes out of date
fetchTimestamp.cuyahoga <- function(viz) {
  checkRequired(viz, c('location'))
  old.timestamp <- readTimestamp(viz)
  # here we're pretending that the sourceloc is some remote location
  new.timestamp <- file.mtime(viz$fetchargs$sourceloc)
  if(!is.na(new.timestamp) && (is.na(old.timestamp) || (new.timestamp != old.timestamp))) {
    writeTimestamp(new.timestamp, viz)
  }
}

fetch.cuyahoga_diff <- function(viz) {
  # show that a fetch item can depend on a process item now
  deps <- readDepends(viz)
  checkRequired(deps, c('cuyahoga_raw','cuyahoga_processed'))
  stopifnot(is.data.frame(deps$cuyahoga_raw))
  stopifnot(is.data.frame(deps$cuyahoga_processed))
  cuyahoga_diff <- dplyr::anti_join(deps$cuyahoga_raw, deps$cuyahoga_processed)
  
  write.table(cuyahoga_diff, viz$location, row.names=FALSE, sep='\t')

}

fetchTimestamp.cuyahoga_diff <- alwaysCurrent
