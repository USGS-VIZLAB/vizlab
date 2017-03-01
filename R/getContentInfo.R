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
#' @export
getContentInfo <- function(viz.id, block=c('fetch','process','visualize','publish','resource'), no.match=c('stop','NA')) {
  if(length(viz.id) != 1) stop("viz.id must have length 1; see getContentInfos for alternatives")
  infos <- getContentInfos(viz.id, block=block)
  if(length(infos) == 0) {
    if(!missing(no.match) && is.na(no.match)) no.match <- 'NA'
    no.match <- match.arg(no.match)
    switch(
      no.match,
      'stop' = stop("no matches for viz.id '", paste0(viz.id, collapse="', '"), "' in block[s] ", paste0(block, collapse=", ")),
      'NA' = return(NA))
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
#' @importFrom stats setNames
#' @export
getContentInfos <- function(viz.id, block=c('fetch','process','visualize', 'publish', 'resource')) {

  content.list <- getBlocks(block)

  # add defaults
  viz.defaults <- yaml.load_file(system.file('viz.defaults.yaml', package='vizlab'))
  relevant.blocks <- intersect(names(viz.defaults), block)
  for(bl in relevant.blocks) {
    default.item <- viz.defaults[[bl]][[1]] # this assumes exactly 1 item per block in viz.default.yaml
    content.list[[bl]] <- lapply(content.list[[bl]], function(item) {
      for(field in names(default.item)) {
        # add defaults, including field=NULL when the default is NULL
        if(!exists(field, item)) item <- c(item, setNames(list(default.item[[field]]), field))
      }
      return(item)
    })
  }

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

#' Read the version of the vizlab config document
#'
#' @export
getVizlabVersion <- function() {
  # version block is unnamed list with version at first index
  return(getBlocks('vizlab')[['vizlab']][[1]][[1]])
}

#' Read vizlab config document and return specific blocks
#'
#' @param block character vector of blocks to read
#' @param keep.block logical whether to include block information in read block
#' @importFrom utils packageName
#' @export
getBlocks <- function(block=c('vizlab', 'info', 'fetch', 'process', 'visualize', 'publish', 'resource'), keep.block=TRUE) {
  # read viz.yaml and isolate the data block
  if (!file.exists('viz.yaml')) {
    stop("viz.yaml does not exist in this working directory (", getwd(), ")")
  }
  viz.yaml <- yaml.load_file('viz.yaml')
  resource.yaml <- yaml.load_file(system.file('resource.library.yaml', package=packageName()))
  viz.yaml[['resource']] <- resource.yaml
  block.yaml <- viz.yaml[block[block %in% names(viz.yaml)]]
  # add the block name to the info for each element
  content.list <- lapply(setNames(nm=block), function(bl) {
    lapply(block.yaml[[bl]], function(item) {
      item <- as.list(item)
      if (keep.block) {
        item$block <- bl
      }
      return(item)
    })
  })
}

