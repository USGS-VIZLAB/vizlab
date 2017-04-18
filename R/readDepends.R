#' read multiple dependency datasets into environment
#'
#' This function should be called from the generic, \code{readDepends()}. Reads
#' all dependency data from files into R format. 
#' 
#' @seealso readData
#'
#' @param viz vizlab object, list, or vizlab identifier
#' @return a list of data objects that are named according to depends
#'
#' @examples 
#' wd <- getwd()
#' setwd(system.file(package = 'vizlab','testviz'))
#' #Read dependencies from list or viz object:
#' viz.data <- readDepends(list(depends = 'MayflyNymph'))
#' viz.data[["MayflyNymph"]]
#' 
#' setwd(wd)
#' @export
readDepends <- function(viz) UseMethod("readDepends")

#' @rdname readDepends
#' @export
readDepends.character <- function(viz) {
  readDepends(as.viz(viz))
}


#' @rdname readDepends
#' @export
readDepends.list <- function(viz){
  viz <- as.viz(viz)
  readDepends(viz)
}

#' @rdname readDepends
#' @export
readDepends.viz <- function(viz){
  viz.ids <- viz[['depends']]
  depends <- lapply(viz.ids, readData)
  if (is.null(names(depends))) {
    names(depends) <- viz.ids
  }
  return(depends)
}