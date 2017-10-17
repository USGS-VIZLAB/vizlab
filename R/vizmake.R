#' Build a visualization project
#'
#' This is the front-line function to call when you want to build all or part of
#' a visualization.
#' @param target_names names of targets to build, or NULL for the default, as
#'   passed to `remake::make()`
#' @param ... arguments passed to `remake::make()` (besides `target_names`,
#'   above, or `remake_file`, which is fixed at 'remake.yaml')
#' @md
#' @export
vizmake <- function(target_names=NULL, ...) {
  
  # check the format of the viz.yaml (limited checks so far)
  validateVizYaml()
  
  # decide whether createRemakefile needs to be rerun
  if(file.mtime('remake.yaml') < file.mtime('viz.yaml')) {
    createRemakefile()
  }
  
  # remake::delete any timestamps or fetch items whose time is up

  
  # source all scripts listed in the remake.yaml. we would normally skip this
  # when calling remake::make, but remake doesn't recognize nested S3 methods
  # such as fetch.custom() being called via fetch()
  remake.yaml <- yaml::yaml.load_file('remake.yaml')
  for(i in seq_along(remake.yaml$sources)) {
    source(remake.yaml$sources[i])
  }
  
  # run remake::make(target_name, ..., remake_file='remake.yaml')
  message('Starting build at ', Sys.time())
  remake::make(target_names=target_names, ..., remake_file='remake.yaml')
  message('Finished build at ', Sys.time())
}
