#' Source R scripts
#'
#' Sources the script or scripts indicated.
#'
#' @param scripts character or character vectory. If an element of scripts is a
#'   directory, sources the contents of that directory. Otherwise sources the
#'   script. If missing, all scripts in the 'scripts' directory will be sourced
#' @param verbose logical. Declare which scripts are being sourced via messages?
#' @param recursive logical. If sourcing a directory, should scripts in
#'   subdirectories be included?
#' @export
sourceScripts <- function(scripts, verbose=TRUE, recursive=TRUE) {
  if(missing(scripts)) scripts <- 'scripts'
  scripts <- do.call(c, lapply(scripts, function(script) {
    if(dir.exists(script))
      dir(script, full.names=TRUE, recursive=recursive)
    else if (file.exists(script))
      script
  }))
  if(verbose) message("Sourcing scripts:\n", paste("  ", scripts, collapse="\n"))
  sapply(scripts, source)
  invisible()
}
