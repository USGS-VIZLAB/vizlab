#' Get information on a content item from the viz.yaml
#' 
#' Returns a single content item. Reads from the viz.yaml in the current working
#' directory
#' 
#' @param viz.id a length-1 character string giving the ID for a single content 
#'   item to return
#' @inheritParams getContentInfos
#' @param no.match what to do if the viz.id is not found: either 'stop' (throw
#'   error) or 'NA' (return NA)
#' @seealso getContentInfos
#' @import yaml
#' @export
getContentInfo <- function(viz.id, block=c('images','fetch','process','visualize'), no.match='stop') {
  if(length(viz.id) != 1) stop("viz.id must have length 1; see getContentInfos for alternatives")
  infos <- getContentInfos(viz.id, block=block)
  if(length(infos) == 0) {
    switch(
      no.match,
      'stop'=stop("no matches for viz.id ", paste0(viz.id, collapse=", "), " in block[s] ", paste0(block, collapse=", ")),
      'NA'=return(NA))
  }
  if(length(infos) > 1) stop("multiple matches for viz.id ", paste0(viz.id, collapse=", "), " in block[s] ", paste0(block, collapse=", "), ":\n", paste0("  ", names(infos), collapse="\n"))
  infos[[1]]
}

#' Get information on one or more content items from the viz.yaml
#' 
#' Returns a list of content items, each of which contains a list of information
#' key-value pairs. Reads from the viz.yaml in the current working directory
#' 
#' @param viz.id character[s] giving specific IDs of items to return, or missing
#'   to return all content items
#' @param block the name or names of content blocks whose items should be 
#'   included in the query
#' @seealso getContentInfo
#' @import yaml
#' @export
getContentInfos <- function(viz.id, block=c('images','fetch','process','visualize')) {
  # read viz.yaml and isolate the data block
  viz.yaml <- yaml.load_file('viz.yaml')
  content.yaml <- viz.yaml[block[block %in% names(viz.yaml)]]
  # add the block name to the info for each element
  content.list <- lapply(setNames(nm=block), function(bl) {
    lapply(content.yaml[[bl]], function(item) { item$block <- bl; item })
  })
  # flatten the list of lists into a list
  content.info <- unlist(unname(content.list), recursive=FALSE)
  names(content.info) <- sapply(content.info, function(ci) paste0(ci$block, '/', ci$id))
  
  # return the full list or pull out info for a specific id or ids
  if(missing(viz.id)) {
    return(content.info)
  } else {
    viz.id.names <- as.character(interaction(block, viz.id, sep = "/")) 
    viz.matches <- viz.id.names[viz.id.names %in% names(content.info)]
    return(content.info[viz.matches])
  }
}