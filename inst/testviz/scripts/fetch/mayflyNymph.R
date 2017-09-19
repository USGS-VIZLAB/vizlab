#' My mayfly fetcher function
#' 
#' Could also accept the data.info arg but won't because we don't use it.
#' Intentionally switching up the arg order just to show that it can be done
#' (though it's not necessarily a great idea)
fetch.mayflyNymph <- function(viz) {
  # this function doesn't actually do, or need to do, anything
  message("Great! MayflyNymph.csv is already at ", viz[['location']])
}

#' The quickest way to implement a fetchTimestamp method for a local file is by
#' copying fetchTimestamp.file
fetchTimestamp.mayflyNymph <- vizlab:::fetchTimestamp.file
