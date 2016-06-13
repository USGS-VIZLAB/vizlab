#' Log on to a remote file service
#' 
#' Uses authentication information in the user directory to log onto a remote 
#' file service such as ScienceBase. This function is primarily for internal use
#' but can be called directly to check credentials.
#' 
#' To add credentials, create an R script called "auth.fetcher.R" where 
#' "fetcher" is replaced by the specific fetcher as designated in viz.yaml, 
#' e.g., "auth.sciencebase.R". Place this script in the user/local or 
#' user/docker folder (depending on whether you will be building the 
#' visualization directly or via docker) (BUT docker-specific authentication 
#' isn't yet implemented. stay tuned). This script should log on to the service
#' with valid credentials. Take care to never commit the user directory to an
#' unsecured repository.
#' 
#' @param fetcher the name of the fetcher for which to authenticate, e.g., 
#'   'sciencebase'
#' @examples 
#' # An example 1-line user/local/auth.sciencebase.R (fill in the credentials):
#' sbtools::authenticate_sb(myusername, mypassword)
#' @export
authRemote <- function(fetcher, ...) UseMethod("authRemote")

#' @export
authRemote.default <- function(fetcher, ...) {
  # give error if fetcher doesn't specify a defined methods (otherwise the error
  # is infinite recursion, which is confusing)
  knownMethods <- setdiff(sapply(strsplit(as.character(methods('authRemote')), '\\.'), `[[`, 2), 'default')
  if(!(fetcher %in% knownMethods)) stop('unknown fetcher')
  
  # route the method to the specified fetcher
  class(fetcher) <- fetcher
  invisible(authRemote(fetcher, user='local')) # how to know whether this should be local or docker?
}

#' \code{authRemote.file} currently does nothing. It exists to communicate that 
#' the .file type is an option for data items.
#' 
#' @rdname authRemote
#' @export
authRemote.file <- function(fetcher, user) {
  invisible()
}

#' \code{authRemote.sciencebase} calls the user's script at 
#' user/local/auth.sciencebase.R. That script should contain the R commands and
#' credentials to log on to ScienceBase with read (and possibly write)
#' permissions.
#' 
#' @rdname authRemote
#' @export
authRemote.sciencebase <- function(fetcher, user) {
  auth.script <- paste0("user/", user, "/auth.sciencebase.R")
  if(file.exists(auth.script)) {
    source(auth.script)
  } else {
    message('requesting data from ScienceBase without authentication because ', auth.script, ' does not exist')
  }
  invisible()
}
