#' @import yaml
#' @import jsonlite
getDataInfo <- function() {
  # read viz.yaml
  viz.yaml <- yaml.load_file('viz.yaml')
  
  # isolate the data block
  data <- viz.yaml[["data"]]
  data.list <- list()
  for (data.item in data) {
    data.list[[data.item$id]] <- data.item
  }

  return(data.list)
}