#' Combines and filters one or more script files into a single script
#' appropriate to a single viz item
#'
#' Return the MD5sum of a deparsed script - this shouldn't change if whitespace
#' or comments change but should change if there's anything substantive
#'
#' @param ... length-1 character strings of R script filenames to be parsed`
#' @param functions length-1 character string of 1+ functions to include, or ""
#'   to include all functions. The contents of the combined scripts can be
#'   optionally filtered to include definitions for only those functions (or R
#'   objects) listed here. If
#'   there are >1 functions to include, they should be separated by commas wtihin the same
#'   string, e.g., `'fetch.cars,cars.helper'`
#' @param outfile full file path where the combined source file should be
#'   written. This will usually be vizlab/remake/scripts/{vizitemname}.R
#' @export
prepSources <- function(..., functions, outfile) {
  
  # accept scripts as filenames in ...
  scripts <- c(...)
  
  # accept functions as comma-separated length-1 character strings
  functions <- if(missing(functions)) c() else strsplit(functions, split=',')[[1]]
  
  # parse all the scripts into a single list of expressions
  parsed <- unlist(lapply(scripts, parse))
  function_names <- lapply(parsed, `[[`, 2) # assumes every expression is a function definition
  parsed <- setNames(parsed, function_names)
  
  # if requested, filter parsed to a subset of functions
  if(length(functions) > 0) {
    unknown <- setdiff(functions, names(parsed))
    if(length(unknown)) {
      stop(paste0('functions or objects not found in sources: ', paste(unknown, collapse=', ')))
    }
    parsed <- parsed[functions]
  }
  
  # make sure the receiving directory exists
  outdir <- dirname(outfile)
  if(!dir.exists(outdir)) dir.create(outdir, recursive=TRUE)
  
  # deparse and write to file
  minified <- unlist(unname(lapply(parsed, deparse)))
  writeLines(minified, con=outfile)
}
