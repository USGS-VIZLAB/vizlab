#' updateConfigInfoFile
#' 
#' Creates 1 file per viz id with the content info. This is necessary to 
#' update the project if only the viz.yaml is updated (otherwise make)
#' won't trigger an update.
#' 
#' @param viz.id character[s] giving specific IDs of items to return, or missing
#'   to return all content items
#'   
#' @export
updateConfigInfoFile <- function(viz.id){
  
  info <- getContentInfo(viz.id)
  
  file.config <- paste0(viz.id,".rds")
  full.config <- file.path("vizlab/make/config",file.config) 
  if(file.exists(full.config)){
    orig.info <- readRDS(full.config)
    if(!isTRUE(all.equal(orig.info, info))) {
      saveRDS(info, full.config)
    }
  } else {
    saveRDS(info, full.config)
  }
  
}
