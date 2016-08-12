#' Source R scripts
#'
#' Sources the script or scripts indicated.
#'
#' @param scripts character or character vectory. If an element of scripts is a
#'   directory, sources the contents of that directory. Otherwise sources the
#'   script.
#' @param verbose logical. Declare which scripts are being sourced via messages?
#'
#' @export
sourceScripts <- function(scripts, verbose=TRUE) {
  scripts <- do.call(c, lapply(scripts, function(script) {
    if(dir.exists(script))
      file.path(script, dir(script))
    else if (file.exists(script))
      script
  }))
  if(verbose) message("Sourcing scripts:\n", paste("  ", scripts, collapse="\n"))
  sapply(scripts, source)
  invisible()
}
