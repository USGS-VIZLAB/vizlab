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
  
  # check whether the right packages are installed
  checkVizPackages()
  
  # run createRemakefile and createDirectories if needed
  if(!file.exists('remake.yaml') ||
     file.mtime('remake.yaml') < file.mtime('viz.yaml') ||
     file.mtime('remake.yaml') < file.mtime(system.file('DESCRIPTION', package='vizlab'))) {
    
    viz.items <- getContentInfos()
    createRemakefile(viz.items)
    createDirectories(viz.items)
    
  }
  # read in the remake file
  remake.yaml <- yaml::yaml.load_file('remake.yaml')
  
  # remake::delete any timestamps whose time is up
  timestamp.pattern <- 'vizlab/remake/timestamps/'
  timestamp.targets <- grep(timestamp.pattern, names(remake.yaml$targets), value=TRUE)
  timestamped.items <- tools::file_path_sans_ext(basename(timestamp.targets))
  for(i in seq_along(timestamped.items)) {
    if(exceededTimeToLive(timestamped.items[i])) {
      remake::delete(timestamp.targets[i], remake_file='remake.yaml')
    }
  }
  
  # source all scripts listed in the remake.yaml. we would normally skip this
  # when calling remake::make, but remake doesn't recognize nested S3 methods
  # such as fetch.custom() being called via fetch()
  for(i in seq_along(remake.yaml$sources)) {
    source(remake.yaml$sources[i])
  }
  
  webpack.config <- getWebpackConfig()
  if(!is.null(webpack.config)) {
    # run webpack
    message('Setting up webpack configs ', Sys.time())
    webpacker(webpack.config)
    message('Finishing webpack configs ', Sys.time())
  }
  
  # run remake::make(target_name, ..., remake_file='remake.yaml')
  message('Starting build at ', Sys.time())
  out <- remake::make(target_names=target_names, ..., remake_file='remake.yaml')
  message('Finished build at ', Sys.time())
  
  # return whatever remake returned unless we can do even better
  return(out)
}
