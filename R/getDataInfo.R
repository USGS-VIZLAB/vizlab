#' Get the data elements
#' 
#' Get information on the data block of the viz.yaml
#' 
#' @import yaml
#' @import jsonlite
#' @export
getDataInfo <- function() {
  # read viz.yaml
  viz.yaml <- yaml.load_file('viz.yaml')
  
  # isolate the data block and turn ids into list element names
  data <- viz.yaml[["data"]]
  data.list <- data[sapply(data, function(item) 'id' %in% names(item))]
  names(data.list) <- sapply(data.list, function(item) item$id)

  return(data.list)
}