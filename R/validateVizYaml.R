#' Find obvious errors in your viz.yaml
#' 
#' @export
validateVizYaml <- function() {
  viz.items <- getContentInfos()
  
  # require that ids are always present and have no spaces, dashes, etc. in them
  for(i in seq_along(viz.items)) {
    id <- viz.items[[i]]$id
    if(is.null(id)) {
      stop(paste0("missing or null viz item id in ", i, "th item"))
    } else if(viz.items[[i]]$block != 'resource'){
      parsed <- tryCatch(
        parse(text=id),
        error=function(e) {
          warning(paste0("unparseable viz item id: ", id))
        })
      if(length(parsed[[1]]) > 1) {
        warning(paste0("viz item id parses to >1 R variable name: ", id))  
      }
    }
  }
  
  # require that expected sections exist
  # require taht unexpected sections don't exist
  # require that mimeTypes are real mimeTypes
  # require that item locations are in data, cache/fetch, cache/process, cache/visualize, or 
  #   maybe external to visualization directory (?)
  # etc.
}
