getData <- function(x) UseMethod("getData")

getData.default <- function(identifier) {
  data.info <- getDataInfo()
  data.ids <- names(data.info)
  data <- NULL
  if(!(identifier %in% data.ids)) {
    data.item <- identifier
    class(data.item) <- 'cachefile'
  } else {
    data.item <- data.info[[identifier]]
    if (!is.null(data.item)) {
      class(data.item) <- ifelse(exists("handler", data.item), data.item$handler, "file")
    }
  } 
  data <- getData(data.item)
  return(data)
}

getData.cachefile <- function(data) {
  id_basename <- basename(identifier)
  data.item <- data.info[sapply(data.info, function(di) {
    'filename' %in% names(di) && di$filename == id_basename
  })]
  if(length(data.item) == 0) {
    stop("identifier couldn't be found among data filenames")
  }
  return(getData(data.item$id))
}

getData.file <- function(data) {
  fileItem <- list()
  fileItem$location <- data$location
  fileItem$mimeType <- data$mimeType
  class(fileItem) <- "fileItem"
  return(fileItem)
}

getData.sciencebase <- function(data) {
  fileItem <- list()
  file <- getFiles(data$itemId, data$filename)
  fileItem$location <- file
  fileItem$mimeType <- data$mimeType
  class(fileItem) <- "fileItem"
  return(fileItem)
}