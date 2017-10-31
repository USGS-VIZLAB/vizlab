#' Read the preferences.yaml file, if available, in the current working
#' directory. Return as a formatted R list.
#'
#' @section Sections in preferences.yaml and the output of `getPreferences()`:
#'
#'   * `timetolive`: a list of difftimes, named for the corresponding viz IDs.
#'   The format is a length-2 space-separated string describing a difftime. For
#'   example, `Inf days` becomes `as.difftime(as.numeric("Inf"), units="days"`,
#'   `6 hours` becomes `as.difftime(as.numeric("6"), units="days")`
#'
#' @md
#' @export
getPreferences <- function() {
  
  # the basics: read the file if it's there
  pref_file <- 'preferences.yaml'
  if(file.exists(pref_file)) {
    prefs <- yaml::yaml.load_file(pref_file)
  } else {
    prefs <- list()
  }
  
  # time to live: read in any ttl values that are specified (assume "number
  # units" format for passing to as.difftime) and override the defaults
  # (as.difftime(0, units='secs')) with that information
  all_ttls <- unlist(unname(lapply(getContentInfos(block='fetch'), function(viz) viz[['id']])))
  all_ttls <- setNames(rep(list(as.difftime(0, units='secs')), length(all_ttls)), all_ttls)
  set_ttls <- prefs[['timetolive']]
  if(!is.null(set_ttls)) {
    # parse the user-specified ttls into difftimes
    set_ttls <- lapply(set_ttls, function(t) {
      tryCatch({
        time_info <- strsplit(t, split='[[:blank:]]')[[1]]
        as.difftime(tim=as.numeric(time_info[[1]]), units=time_info[[2]])
      }, error=function(e) {
        stop("expecting time in format 'tim units', to be passed to as.difftime")
      })
    })
    
    # override the default with the user-specified ttls where available
    all_ttls[names(set_ttls)] <- set_ttls
  }
  # write the result back into the prefs list
  prefs[['timetolive']] <- all_ttls
  
  return(prefs)
}

#' Return logical: has the time to live been exceeded for this viz item
#'
#' @param id the character id of a viz item in the "fetch" block
#' @export
exceededTimeToLive <- function(id) {
  viz <- as.viz(id)
  if(!file.exists(locateTimestampFile(id))) {
    exceeded <- TRUE
  } else {
    ttl <- getPreferences()[['timetolive']][[id]]
    old.timestamp <- readTimestamp(viz)
    exceeded <- Sys.time() > old.timestamp + ttl
  }
  return(exceeded)
}
