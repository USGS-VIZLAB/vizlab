readData <- function(x) UseMethod("readData")

readData.default <- function(identifier) {
  data <- getData(identifier)
  return(readData(data))
}

readData.fileItem <- function(file.item) {
  mimeType <- file.item[["mimeType"]]
  class <- switch(mimeType,
                  "text/csv" = "csv",
                  "text/tab-seperated-values" = "csv",
                  "text/yaml" = "yaml",
                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = "excel",
                  "none")
  class(file.item) <- class
  return(readData(file.item))
}

readData.csv <- function(data) {
  raw.data <- fread(data$location)
  raw.data <- setDF(raw.data)
  raw.data
}

readData.yaml <- function(data) {
  yaml.load_file(data$location)
}

readData.excel <- function(data) {
  # The only thing coming in is location and mime type,....would like to read in more parameters
  read_excel(data$location, skip=2)
}

readData.none <- function(data) {
  throw("Could not read file of this type")
}