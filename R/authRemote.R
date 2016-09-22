#' Log on to a remote file service
#' 
#' TODO: update documentation
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

#' \code{authRemote.sciencebase} calls the user's script at 
#' user/local/auth.sciencebase.R. That script should contain the R commands and
#' credentials to log on to ScienceBase with read (and possibly write)
#' permissions.
#' 
#' @rdname authRemote
#' @export
authRemote.sciencebase <- function(fetcher, user, ...) {
  #change to home directory for storage
  home <- path.expand('~')
  sbCreds <- file.path(home, ".vizlab/sbCreds")
  
  #check if already logged in 
  if(sbtools::is_logged_in()){
    message("Using existing sciencebase session")
  } else if(file.exists(sbCreds)) {
    credList <- readRDS(sbCreds)
    un <- rawToChar(credList$username)
    pw <- rawToChar(credList$password)
    sbtools::authenticate_sb(un, pw)
    message("Logging into sciencebase with stored credentials")
  } else {
    message('requesting data from ScienceBase without authentication because   
            no credentials exist.  Use the storeSBcreds() function to add credentials.')
  } 
  
  invisible()
}

#' Store and verify credentials for sciencebase
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
