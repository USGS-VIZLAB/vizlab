#' Log on to a remote file service
#'
#' Uses authentication information in the ~/.vizlab driectory to log onto a remote
#' file service such as ScienceBase. This function is primarily for internal use
#' but can be called directly to check credentials.
#'
#' To add ScienceBase credentials, run the \code{storeSBcreds} function.
#'
#' @param fetcher the name of the fetcher for which to authenticate, e.g.,
#'   'sciencebase'
#' @param ... other arguments passed to authRemote methods
#' @export
authRemote <- function(fetcher, ...) UseMethod("authRemote")

#' @param user not yet well thought out, but the idea is to accept 'local' or
#'   'docker' and use the corresponding user profile to authenticate
#'
#' @rdname authRemote
#' @export
authRemote.default <- function(fetcher, user='local', ...) {
  # explain the problem if we're headed for infinite recursion
  if(class(fetcher) != 'character')
    stop('could not find authRemote method for fetcher=', fetcher)

  # route the method to the specified fetcher
  class(fetcher) <- fetcher
  invisible(authRemote(fetcher, user=user, ...))
}

#' \code{authRemote.sciencebase} Looks for an existing ScienceBase session or
#' uses stored credentials to log into ScienceBase. There are several
#' possibilities for authentication. From first to last priority: (1) If you're
#' already logged in within the current R session, this function does nothing.
#' (2) If the dssecrets package is installed, uses the vizlabrobot secret. (3)
#' If the secret package is installed, uses the 'vizlab-sciencebase' secret with
#' fields 'username' and 'password'. (4) Otherwise gives a warning and proceeds
#' without authenticating.
#'
#' @rdname authRemote
#' @export
authRemote.sciencebase <- function(fetcher, user, ...) {

  # use existing session if already logged in
  if(sbtools::is_logged_in()){
    message("Using existing ScienceBase session")
    return(invisible())
  }
  
  # next try dssecrets
  if(suppressWarnings(require(dssecrets, quietly=TRUE))) {
    success <- tryCatch({
      creds <- dssecrets::get_dssecret('vizlab-sb-srvc-acct')
      sbtools::authenticate_sb(username=creds[['username']], password=creds[['password']])
      message("Logged into ScienceBase with dssecrets package")
      TRUE
    }, error=function(e) {
      message("Failed to log in using the dssecrets package. proceeding to next option")
      FALSE
    })
    if(success) return(invisible())
  }
  
  # next try secret
  if(suppressWarnings(require(secret, quietly=TRUE))) {
    success <- tryCatch({
      creds <- secret::get_secret('vizlab-sciencebase')
      sbtools::authenticate_sb(username=creds[['username']], password=creds[['password']])
      message("Logged into ScienceBase with secret package")
      TRUE
    }, error=function(e) {
      message("Failed to log in using the secret package. proceeding to next option")
      FALSE
    })
    if(success) return(invisible())
  }
  
  # next give up
  warning(paste(strwrap(paste(
    'Requesting data from ScienceBase without authentication because',
    'no credentials exist. Log in first or use the dssecrets or secret packages.')), collapse='\n'))
}
