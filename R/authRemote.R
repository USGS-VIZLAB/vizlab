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

#' \code{authRemote.sciencebase} Uses the credentials stored in ~/.vizlab to log in to 
#' ScienceBase.  Use the storeSBcreds function to verify and store credentials.  Since vizlab
#' starts its own R session, an existing ScienceBase session cannot be used. 
#' 
#' @rdname authRemote
#' @export
authRemote.sciencebase <- function(fetcher, user, ...) {
  #change to home directory for storage
  home <- path.expand('~')
  sbCreds <- file.path(home, ".vizlab/sbCreds")
  
  #check if already logged in; don't think there is a way for this to happen
  if(sbtools::is_logged_in()){ 
    message("Using existing sciencebase session")
  } else if(file.exists(sbCreds)) {
    credList <- readRDS(sbCreds)
    un <- rawToChar(credList$username)
    pw <- rawToChar(credList$password)
    sbtools::authenticate_sb(un, pw)
    message("Logging into ScienceBase with stored credentials")
  } else {
    message('requesting data from ScienceBase without authentication because   
            no credentials exist.  Use the storeSBcreds() function to add credentials.')
  } 
  
  invisible()
}

#' Store and verify credentials for ScienceBase.  User is prompted for credentials.
#' Using a ScienceBase account tied to an AD account is not recommended for password security.
#' @export
#' 
storeSBcreds<- function(){
  dotVizlab <- file.path(path.expand("~"),".vizlab")
  if(!dir.exists(dotVizlab)){
    dir.create(dotVizlab)
  }
  un <- readline(prompt = "Enter username: ")
  pw <- readline(prompt = "Enter password: ")
  sbtools::authenticate_sb(un, pw)
  session_logout()
  sbCreds <- list(username=charToRaw(un),password=charToRaw(pw))
  saveRDS(sbCreds, file.path(dotVizlab, "sbCreds"))
}
