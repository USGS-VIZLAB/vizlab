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
  sbCred1 <- paste0(home, "/.vizlab/sbCred1")
  sbCred2 <- paste0(home, "/.vizlab/sbCred2")
  if(file.exists(sbCred1) && file.exists(sbCred2)) {
    un <- rawToChar(readRDS(sbCred1))
    pw <- rawToChar(readRDS(sbCred2))
    sbtools::authenticate_sb(un, pw)
  } else {
    message('requesting data from ScienceBase without authentication because   
            no credentials exist. Use the storeSB function to enter your sciencebase credentials')
  } 
  
  invisible()
}

#' Store password for sciencebase
#' @export
#' 
sbAuthenticate<- function(){
  dotVizlab <- paste(path.expand("~"),".vizlab", sep="/")
  if(!dir.exists(dotVizlab)){
    dir.create(dotVizlab)
  }
  sbCred1 <- readline(prompt = "Enter username: ")
  sbCred2 <- readline(prompt = "Enter password: ")
  sbtools::authenticate_sb(sbCred1, sbCred2)
  storeCreds <- readline(prompt = "Store credentials? (T/F): ")
  if(storeCreds){
    saveRDS(charToRaw(sbCred1), paste0(dotVizlab, "/cred1"))
    saveRDS(charToRaw(sbCred2), paste0(dotVizlab, "/cred2"))
  }
}
